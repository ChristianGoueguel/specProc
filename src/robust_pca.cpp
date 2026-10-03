// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>

// Computational kernels for robpca() and rospca(). They are written from the
// published descriptions of the algorithms:
//  - univariate MCD and FAST-MCD: Rousseeuw and Van Driessen (1999),
//    Technometrics 41(3):212-223;
//  - Stahel-Donoho outlyingness over directions through pairs of points:
//    Hubert, Rousseeuw and Vanden Branden (2005), Technometrics 47(1):64-79;
//  - sparse PCA by a grid search: Croux, Filzmoser and Fritz (2013),
//    Technometrics 55(2):202-214.
// Random numbers come from R's generator, so set.seed() makes results
// reproducible.

using Eigen::MatrixXd;
using Eigen::VectorXd;

namespace {

// Relative tolerance: quantities below kTiny times the scale of the data
// they come from are zero at the rounding level. All the tolerances are
// relative, so that the results do not depend on the units of the data.
const double kTiny = 1e-12;

// Consistency factor of the raw MCD covariance for normal data (the
// fraction h/n of the data is used).
double mcd_consistency(int n, int h, int p) {
  const double alpha = static_cast<double>(h) / n;
  if (alpha >= 1.0) return 1.0;
  const double q = R::qchisq(alpha, p, 1, 0);
  return alpha / R::pchisq(q, p + 2, 1, 0);
}

// Exact univariate MCD: the h contiguous order statistics with the smallest
// variance. Returns the raw location and consistent scale.
void univariate_mcd(const VectorXd& x, int h, double& location, double& scale) {
  const int n = x.size();
  std::vector<double> s(x.data(), x.data() + n);
  std::sort(s.begin(), s.end());
  double sum = 0.0, sumsq = 0.0;
  for (int i = 0; i < h; ++i) {
    sum += s[i];
    sumsq += s[i] * s[i];
  }
  double best_ss = sumsq - sum * sum / h, best_sum = sum;
  for (int i = h; i < n; ++i) {
    sum += s[i] - s[i - h];
    sumsq += s[i] * s[i] - s[i - h] * s[i - h];
    const double ss = sumsq - sum * sum / h;
    if (ss < best_ss) {
      best_ss = ss;
      best_sum = sum;
    }
  }
  location = best_sum / h;
  const double var = std::max(best_ss, 0.0) / (h - 1);
  scale = std::sqrt(var * mcd_consistency(n, h, 1));
}

// Indices of the h smallest values.
std::vector<int> smallest(const VectorXd& d, int h) {
  std::vector<int> idx(d.size());
  std::iota(idx.begin(), idx.end(), 0);
  std::nth_element(idx.begin(), idx.begin() + (h - 1), idx.end(),
                   [&d](int a, int b) { return d[a] < d[b]; });
  idx.resize(h);
  std::sort(idx.begin(), idx.end());
  return idx;
}

struct MeanCov {
  VectorXd mean;
  MatrixXd cov;
  double logdet;
  bool singular;
};

MeanCov mean_cov(const MatrixXd& x, const std::vector<int>& idx) {
  const int p = x.cols();
  const int m = idx.size();
  MeanCov out;
  out.mean = VectorXd::Zero(p);
  for (int i : idx) out.mean += x.row(i).transpose();
  out.mean /= m;
  out.cov = MatrixXd::Zero(p, p);
  for (int i : idx) {
    const VectorXd d = x.row(i).transpose() - out.mean;
    out.cov.noalias() += d * d.transpose();
  }
  out.cov /= (m - 1);
  Eigen::LDLT<MatrixXd> ldlt(out.cov);
  const VectorXd diag = ldlt.vectorD();
  // singular when a pivot is negligible relative to the largest one
  out.singular = (ldlt.info() != Eigen::Success) || (diag.minCoeff() <= kTiny * diag.maxCoeff());
  out.logdet = out.singular ? -std::numeric_limits<double>::infinity() : diag.array().log().sum();
  return out;
}

VectorXd mahalanobis2(const MatrixXd& x, const MeanCov& mc) {
  const MatrixXd centered = x.rowwise() - mc.mean.transpose();
  Eigen::LLT<MatrixXd> llt(mc.cov);
  const MatrixXd z = llt.matrixL().solve(centered.transpose());
  return z.colwise().squaredNorm().transpose();
}

// C-steps from an initial h-subset until the determinant stops decreasing.
MeanCov csteps(const MatrixXd& x, std::vector<int> subset, int h, int max_steps) {
  MeanCov mc = mean_cov(x, subset);
  for (int s = 0; s < max_steps && !mc.singular; ++s) {
    std::vector<int> next = smallest(mahalanobis2(x, mc), h);
    if (next == subset) break;
    MeanCov mc_next = mean_cov(x, next);
    // a C-step never increases the determinant; stop when it no longer decreases
    const bool converged = mc_next.singular || mc_next.logdet >= mc.logdet - 1e-12;
    subset = next;
    mc = mc_next;
    if (converged) break;
  }
  return mc;
}

// Random subset of size m from 0..n-1 (R's RNG).
std::vector<int> random_subset(int n, int m) {
  std::vector<int> pool(n);
  std::iota(pool.begin(), pool.end(), 0);
  for (int i = 0; i < m; ++i) {
    const int j = i + static_cast<int>(R::unif_rand() * (n - i));
    std::swap(pool[i], pool[std::min(j, n - 1)]);
  }
  pool.resize(m);
  return pool;
}

}  // namespace

// Univariate MCD location and scale.
// [[Rcpp::export]]
Rcpp::NumericVector univariate_mcd_cpp(const Eigen::Map<Eigen::VectorXd> x, int h) {
  if (h < 2 || h > x.size()) Rcpp::stop("'h' must be between 2 and length(x).");
  double location, scale;
  univariate_mcd(x, h, location, scale);
  return Rcpp::NumericVector::create(Rcpp::Named("location") = location, Rcpp::Named("scale") = scale);
}

// Stahel-Donoho outlyingness of the rows of z, over directions through pairs
// of rows. All pairs are used when ndir <= 0 or ndir >= n(n-1)/2; otherwise
// ndir random pairs. The univariate location and scale on each direction are
// the MCD estimates with coverage h.
// [[Rcpp::export]]
Rcpp::NumericVector sd_outlyingness_cpp(const Eigen::Map<Eigen::MatrixXd> z, int h, int ndir) {
  const int n = z.rows();
  if (n < 3) Rcpp::stop("At least 3 observations are needed.");
  const double npairs = 0.5 * n * (n - 1.0);
  const bool all_pairs = ndir <= 0 || ndir >= npairs;
  const long ndirs = all_pairs ? static_cast<long>(npairs) : ndir;
  // scale of the data: the projections on unit directions are bounded by it
  const double zscale = z.rowwise().norm().maxCoeff();
  VectorXd outl = VectorXd::Zero(n);
  int used = 0;
  int i = 0, j = 1;
  for (long d = 0; d < ndirs; ++d) {
    int a, b;
    if (all_pairs) {
      a = i;
      b = j;
      if (++j == n) {
        ++i;
        j = i + 1;
      }
    } else {
      a = static_cast<int>(R::unif_rand() * n);
      b = static_cast<int>(R::unif_rand() * (n - 1));
      if (b >= a) ++b;
      a = std::min(a, n - 1);
      b = std::min(b, n - 1);
    }
    VectorXd v = (z.row(a) - z.row(b)).transpose();
    const double norm = v.norm();
    if (norm <= kTiny * zscale) continue;  // identical rows
    v /= norm;
    const VectorXd proj = z * v;
    double location, scale;
    univariate_mcd(proj, h, location, scale);
    if (scale <= kTiny * zscale) continue;  // exact fit on this direction
    outl = outl.cwiseMax(((proj.array() - location).abs() / scale).matrix());
    ++used;
    if ((d & 255) == 0) Rcpp::checkUserInterrupt();
  }
  if (used == 0) Rcpp::stop("All directions give a zero scale: the data lie in a lower-dimensional subspace.");
  return Rcpp::wrap(outl);
}

// Centers the columns of x and writes the observations in an orthonormal
// basis of the subspace they span: x - 1 center' = z v', with v (p x r)
// orthonormal and r the rank. The basis comes from the eigen-decomposition of
// the smaller cross-product matrix, x x' (n x n) when n <= p and x'x
// otherwise, which is much faster than an SVD of x when p is large. With
// x x', z = u d comes directly from the eigenvectors, so the reduced
// observations keep the inner products of the centered data. Eigenvalues
// below tol times the largest are treated as zero: the cross-product squares
// the singular values, so smaller ones cannot be resolved.
// [[Rcpp::export]]
Rcpp::List svd_reduce_cpp(const Eigen::Map<Eigen::MatrixXd> x, double tol) {
  const int n = x.rows();
  const int p = x.cols();
  const VectorXd center = x.colwise().mean().transpose();
  const MatrixXd xc = x.rowwise() - center.transpose();
  const bool wide = n <= p;
  const int m = wide ? n : p;
  MatrixXd cross = MatrixXd::Zero(m, m);
  if (wide) {
    cross.selfadjointView<Eigen::Lower>().rankUpdate(xc);
  } else {
    cross.selfadjointView<Eigen::Lower>().rankUpdate(xc.transpose());
  }
  // the solver reads the lower triangle; eigenvalues in increasing order
  Eigen::SelfAdjointEigenSolver<MatrixXd> es(cross);
  if (es.info() != Eigen::Success) Rcpp::stop("The eigen-decomposition failed.");
  const VectorXd values = es.eigenvalues().reverse();
  const MatrixXd vectors = es.eigenvectors().rowwise().reverse();
  if (!(values[0] > 0.0)) Rcpp::stop("The data have no variation.");
  int rank = 0;
  while (rank < m && values[rank] > tol * values[0]) ++rank;
  const VectorXd d = values.head(rank).cwiseSqrt();
  MatrixXd z, v;
  if (wide) {
    const MatrixXd u = vectors.leftCols(rank);
    z = u * d.asDiagonal();
    v = xc.transpose() * u * d.cwiseInverse().asDiagonal();
  } else {
    v = vectors.leftCols(rank);
    z = xc * v;
  }
  return Rcpp::List::create(Rcpp::Named("center") = center, Rcpp::Named("v") = v,
                            Rcpp::Named("z") = z);
}

// FAST-MCD of the rows of x with coverage h: nsamp random (p+1)-subsets, two
// C-steps each, full C-steps on the 10 best, consistency correction and one
// reweighting step at the 0.975 chi-squared quantile.
// [[Rcpp::export]]
Rcpp::List fast_mcd_cpp(const Eigen::Map<Eigen::MatrixXd> x, int h, int nsamp) {
  const int n = x.rows();
  const int p = x.cols();
  if (h <= p || h > n) Rcpp::stop("'h' must be larger than the number of variables and at most n.");

  MeanCov best;
  if (h == n) {
    std::vector<int> all(n);
    std::iota(all.begin(), all.end(), 0);
    best = mean_cov(x, all);
  } else {
    std::vector<MeanCov> candidates;
    std::vector<std::vector<int>> subsets;
    for (int s = 0; s < nsamp; ++s) {
      std::vector<int> start = random_subset(n, p + 1);
      MeanCov mc = mean_cov(x, start);
      // enlarge singular starting subsets
      while (mc.singular && static_cast<int>(start.size()) < h) {
        std::vector<int> extra = random_subset(n, n);
        for (int e : extra) {
          if (std::find(start.begin(), start.end(), e) == start.end()) {
            start.push_back(e);
            break;
          }
        }
        mc = mean_cov(x, start);
      }
      if (mc.singular) continue;
      std::vector<int> sub = smallest(mahalanobis2(x, mc), h);
      MeanCov refined = csteps(x, sub, h, 2);
      if (refined.singular) {
        Rcpp::List out = Rcpp::List::create(Rcpp::Named("singular") = true);
        return out;
      }
      candidates.push_back(refined);
      subsets.push_back(smallest(mahalanobis2(x, refined), h));
      if ((s & 31) == 0) Rcpp::checkUserInterrupt();
    }
    if (candidates.empty()) Rcpp::stop("No non-singular subset found: the data lie in a lower-dimensional subspace.");
    std::vector<int> order(candidates.size());
    std::iota(order.begin(), order.end(), 0);
    std::sort(order.begin(), order.end(),
              [&candidates](int a, int b) { return candidates[a].logdet < candidates[b].logdet; });
    const int nbest = std::min<int>(10, order.size());
    bool first = true;
    for (int k = 0; k < nbest; ++k) {
      MeanCov mc = csteps(x, subsets[order[k]], h, 100);
      if (mc.singular) {
        Rcpp::List out = Rcpp::List::create(Rcpp::Named("singular") = true);
        return out;
      }
      if (first || mc.logdet < best.logdet) {
        best = mc;
        first = false;
      }
    }
  }
  if (best.singular) {
    return Rcpp::List::create(Rcpp::Named("singular") = true);
  }

  // raw estimates, corrected for consistency
  MeanCov raw = best;
  raw.cov *= mcd_consistency(n, h, p);
  const VectorXd d2_raw = mahalanobis2(x, raw);

  // reweighting
  const double cutoff = R::qchisq(0.975, p, 1, 0);
  std::vector<int> keep;
  for (int i = 0; i < n; ++i) if (d2_raw[i] <= cutoff) keep.push_back(i);
  MeanCov rew = mean_cov(x, keep);
  rew.cov *= 0.975 / R::pchisq(cutoff, p + 2, 1, 0);
  Rcpp::LogicalVector weights(n, false);
  for (int i : keep) weights[i] = true;

  return Rcpp::List::create(
    Rcpp::Named("singular") = false,
    Rcpp::Named("raw_center") = raw.mean,
    Rcpp::Named("raw_cov") = raw.cov,
    Rcpp::Named("center") = rew.mean,
    Rcpp::Named("cov") = rew.cov,
    Rcpp::Named("weights") = weights
  );
}

namespace {

// Grid search for one sparse component of x (centered), starting from the
// unit vector a. Maximizes var(x a) - lambda * ||a||_1 by rotating a towards
// each coordinate axis in turn, over a grid of angles refined at every
// cycle. The angle that sets the coordinate to zero is always among the
// candidates, so loadings can be exactly zero. vscale is the scale of the
// variances (the mean variance of the variables), to which the tolerances on
// the objective are relative. Returns the objective.
double grid_component(const MatrixXd& x, const VectorXd& colvar, VectorXd& a, double lambda,
                      int ngrid, int maxiter, double tol, double vscale) {
  const int p = x.cols();
  const double denom = std::max<double>(x.rows() - 1, 1);
  const double pi = 3.14159265358979323846;
  VectorXd ya = x * a;
  double vaa = ya.squaredNorm() / denom;
  double objective = vaa - lambda * a.cwiseAbs().sum();
  double range = pi / 2.0;
  for (int iter = 0; iter < maxiter; ++iter) {
    const double previous = objective;
    for (int j = 0; j < p; ++j) {
      const double vjj = colvar[j];
      if (vjj <= 0.0 && a[j] == 0.0) continue;
      const double caj = ya.dot(x.col(j)) / denom;
      const double aj = a[j];
      const double l1_rest = a.cwiseAbs().sum() - std::abs(aj);
      double best_phi = 0.0, best_obj = objective;
      auto eval = [&](double phi) {
        const double c1 = std::cos(phi), s1 = std::sin(phi);
        const double norm2 = c1 * c1 + 2.0 * c1 * s1 * aj + s1 * s1;
        if (norm2 <= kTiny) return;
        const double var = (c1 * c1 * vaa + 2.0 * c1 * s1 * caj + s1 * s1 * vjj) / norm2;
        const double l1 = (std::abs(c1) * l1_rest + std::abs(c1 * aj + s1)) / std::sqrt(norm2);
        const double obj = var - lambda * l1;
        if (obj > best_obj + 1e-14 * vscale) {
          best_obj = obj;
          best_phi = phi;
        }
      };
      for (int g = 0; g < ngrid; ++g) eval(-range + 2.0 * range * (g + 0.5) / ngrid);
      if (aj != 0.0) eval(std::atan(-aj));  // sets coordinate j to zero
      if (best_phi != 0.0) {
        const double c1 = std::cos(best_phi), s1 = std::sin(best_phi);
        VectorXd a_new = c1 * a;
        a_new[j] += s1;
        if (std::abs(a_new[j]) < 1e-12) a_new[j] = 0.0;
        const double norm = a_new.norm();
        a = a_new / norm;
        ya = (c1 * ya + s1 * x.col(j)) / norm;
        vaa = ya.squaredNorm() / denom;
        objective = vaa - lambda * a.cwiseAbs().sum();
      }
    }
    range /= 2.0;
    if (iter > 0 && std::abs(objective - previous) <= tol * std::max(vscale, std::abs(previous))) break;
    Rcpp::checkUserInterrupt();
  }
  return objective;
}

}  // namespace

// Sparse PCA by a grid search (Croux, Filzmoser and Fritz, 2013). Each
// component maximizes var(X a) - lambda * ||a||_1 over unit vectors a. The
// search is run from two starting directions and the better solution is
// kept: the leading (non-sparse) principal direction, found by power
// iteration, and the axis of the variable with the largest covariances with
// the others. A single start can get stuck: the principal direction mixes
// blocks of variables with similar variances, and a single variable can lie
// in a weaker block. The data are deflated by each component (projection
// deflation), so the loadings of different components are not exactly
// orthogonal. x must be centered.
// [[Rcpp::export]]
Eigen::MatrixXd spca_grid_cpp(const Eigen::Map<Eigen::MatrixXd> x_in, int k, double lambda,
                              int ngrid, int maxiter, double tol) {
  const int n = x_in.rows();
  const int p = x_in.cols();
  if (k < 1 || k > p) Rcpp::stop("'k' must be between 1 and the number of variables.");
  MatrixXd x = x_in;
  MatrixXd loadings = MatrixXd::Zero(p, k);
  const double denom = std::max(n - 1, 1);
  // mean variance of the variables: the scale of the objective
  const double vscale = x.squaredNorm() / (denom * p);

  for (int c = 0; c < k; ++c) {
    const VectorXd colvar = x.colwise().squaredNorm().transpose() / denom;
    // variable with the largest covariances with the others
    const MatrixXd gram = x.transpose() * x;
    int j0;
    gram.colwise().squaredNorm().maxCoeff(&j0);

    // start 1: leading principal direction (power iteration)
    VectorXd a1 = VectorXd::Zero(p);
    a1[j0] = 1.0;
    for (int it = 0; it < 500; ++it) {
      VectorXd next = x.transpose() * (x * a1);
      const double norm = next.norm();
      if (norm <= kTiny * denom * vscale) break;  // no variance left
      next /= norm;
      const double change = (next - a1).norm();
      a1 = next;
      if (change < 1e-10) break;
    }
    // start 2: the axis of variable j0
    VectorXd a2 = VectorXd::Zero(p);
    a2[j0] = 1.0;

    const double obj1 = grid_component(x, colvar, a1, lambda, ngrid, maxiter, tol, vscale);
    const double obj2 = grid_component(x, colvar, a2, lambda, ngrid, maxiter, tol, vscale);
    VectorXd a = obj2 > obj1 ? a2 : a1;

    // sign convention: largest absolute loading positive
    int jmax;
    a.cwiseAbs().maxCoeff(&jmax);
    if (a[jmax] < 0) a = -a;
    loadings.col(c) = a;
    // projection deflation
    x -= (x * a) * a.transpose();
  }
  return loadings;
}
