// cellPCA: robust PCA by casewise and cellwise weighting (Centofanti,
// Hubert and Rousseeuw). See cellpca() in R/robust_pca.R. The steps follow
// the reference R code of the authors.
#include <RcppEigen.h>
#include <algorithm>
#include <cmath>
#include <vector>

// [[Rcpp::depends(RcppEigen)]]

namespace {

using Eigen::MatrixXd;
using Eigen::VectorXd;

// Hyperbolic tangent function rho_{b,c} of Hampel et al. (1981). The
// cellwise function rho_1 uses b = 1.5, c = 4 on z * 1.5 / b_1; the casewise
// function rho_2 has tuning constants computed in R (quadratic when b > 100).
struct Tanh {
  double b, c, q1, q2, d, stretch;
  bool quadratic;
  Tanh(double b_, double c_, double q1_, double q2_, double d_, double stretch_)
      : b(b_), c(c_), q1(q1_), q2(q2_), d(d_), stretch(stretch_), quadratic(b_ > 100) {}
  double rho(double z) const {
    if (quadratic) return z * z;
    const double x = z * stretch;
    const double a = std::fabs(x);
    if (a <= b) return x * x / 2;
    if (a < c) return d - q1 / q2 * std::log(std::cosh(q2 * (c - a)));
    return d;
  }
  double weight(double z) const {
    if (quadratic) return 1.0;
    const double x = z * stretch;
    const double a = std::fabs(x);
    if (x == 0 || a <= b) return 1.0;
    if (a <= c) return q1 * std::tanh(q2 * (c - a)) / a;
    return 0.0;
  }
};

Tanh rho1_function(double b1) {
  return Tanh(1.5, 4.0, 1.540793, 0.8622731, 3.7622126668, 1.5 / b1);
}

Tanh rho2_function(const Rcpp::NumericVector& par) {
  const double b = par[0], c = par[1], q1 = par[2], q2 = par[3];
  const double d = b > 100 ? 0.0 : b * b / 2 + q1 / q2 * std::log(std::cosh(q2 * (c - b)));
  return Tanh(b, c, q1, q2, d, 1.0);
}

double median_of(std::vector<double> v) {
  const std::size_t n = v.size();
  const std::size_t mid = n / 2;
  std::nth_element(v.begin(), v.begin() + mid, v.end());
  double m = v[mid];
  if (n % 2 == 0) m = (m + *std::max_element(v.begin(), v.begin() + mid)) / 2.0;
  return m;
}

// M-scale of z around zero with rho_{1.5,4}, delta = 1.8811 and the
// consistency constant 0.34308 (scale_tanh of the reference code).
double scale_tanh(const std::vector<double>& z) {
  if (z.empty()) return NA_REAL;
  const Tanh rho = rho1_function(1.5);
  const double delta = 1.8811, cst = 0.34308;
  std::vector<double> abs_z(z.size());
  for (std::size_t i = 0; i < z.size(); ++i) abs_z[i] = std::fabs(z[i]);
  double s0 = median_of(abs_z) / 0.6745;
  if (s0 < 2.220446e-16) return 0.0;
  for (int it = 0; it < 100; ++it) {
    double mean_rho = 0;
    for (double v : z) mean_rho += rho.rho(v / (cst * s0));
    mean_rho /= z.size();
    const double s1 = std::sqrt(s0 * s0 * mean_rho / delta);
    const double err = std::fabs(s1 - s0) / s0;
    s0 = s1;
    if (err <= 1e-6) break;
  }
  return s0;
}

// Generalized inverse solution of A x = b, as with MASS::ginv(A) %*% b.
VectorXd pinv_solve(const MatrixXd& A, const VectorXd& b) {
  Eigen::JacobiSVD<MatrixXd> svd(A, Eigen::ComputeThinU | Eigen::ComputeThinV);
  const VectorXd& s = svd.singularValues();
  // tolerance of MASS::ginv: sqrt(machine epsilon) times the largest singular value
  const double tol = (s.size() ? s[0] : 0.0) * 1.490116119384765625e-08;
  VectorXd inv = VectorXd::Zero(s.size());
  for (int i = 0; i < s.size(); ++i) {
    if (s[i] > tol) inv[i] = 1.0 / s[i];
  }
  return svd.matrixV() * inv.asDiagonal() * svd.matrixU().transpose() * b;
}

struct State {
  MatrixXd cell;   // cellwise weights (0 for missing cells)
  VectorXd casew;  // casewise weights
  VectorXd t;      // casewise total deviations
  MatrixXd W;      // casewise * cellwise weights * M
  double objective;
};

// Cellwise and casewise weights and objective of the residuals x - mu - U V'.
State update_state(const MatrixXd& x, const MatrixXd& m, const MatrixXd& UV, const VectorXd& mu,
                   const VectorXd& sigma1, double sigma2, const Tanh& rho1, const Tanh& rho2) {
  const int n = x.rows(), p = x.cols();
  State s;
  s.cell = MatrixXd::Zero(n, p);
  s.casew = VectorXd::Zero(n);
  s.t = VectorXd::Zero(n);
  double total = 0;
  for (int i = 0; i < n; ++i) {
    double sum = 0, mi = 0;
    for (int j = 0; j < p; ++j) {
      if (m(i, j) == 0) continue;
      double z = (x(i, j) - mu[j] - UV(i, j)) / sigma1[j];
      if (!std::isfinite(z)) z = 0;
      s.cell(i, j) = rho1.weight(z);
      sum += rho1.rho(z) * sigma1[j] * sigma1[j];
      mi += 1;
    }
    s.t[i] = mi > 0 ? std::sqrt(sum / mi) : NA_REAL;
    s.casew[i] = mi > 0 ? rho2.weight(s.t[i] / sigma2) : 0.0;
    if (mi > 0) total += rho2.rho(s.t[i] / sigma2) * mi;
  }
  s.objective = sigma2 * sigma2 * total / n;
  s.W = s.cell;
  for (int i = 0; i < n; ++i) s.W.row(i) *= s.casew[i];
  return s;
}

}  // namespace

// M-scales of the columns of a matrix, without its missing values.
// [[Rcpp::export]]
Rcpp::NumericVector scale_tanh_cols_cpp(const Rcpp::NumericMatrix x) {
  const int n = x.nrow(), p = x.ncol();
  Rcpp::NumericVector out(p);
  std::vector<double> col;
  col.reserve(n);
  for (int j = 0; j < p; ++j) {
    col.clear();
    for (int i = 0; i < n; ++i) {
      if (!ISNAN(x(i, j))) col.push_back(x(i, j));
    }
    out[j] = scale_tanh(col);
  }
  return out;
}

// rho_1 of standardized residuals (NA kept).
// [[Rcpp::export]]
Rcpp::NumericVector rho_tanh_cpp(const Rcpp::NumericVector z, double b1 = 1.5) {
  const Tanh rho = rho1_function(b1);
  Rcpp::NumericVector out(z.size());
  for (R_xlen_t i = 0; i < z.size(); ++i) out[i] = ISNAN(z[i]) ? NA_REAL : rho.rho(z[i]);
  return out;
}

// IRLS algorithm of cellPCA. `x` has its missing values set to zero and `m`
// is the 0/1 matrix of observed cells. Each iteration updates V (eq. 21),
// U (eq. 23), orthonormalizes V, updates the center (eq. 24, then moved
// inside the subspace) and the weights, until the relative change of U V'
// is below `tol`. When more than `max_col_frac` of a column gets zero
// weights, the previous iteration is kept and the iterations stop.
// [[Rcpp::export]]
Rcpp::List cellpca_irls_cpp(const Eigen::Map<Eigen::MatrixXd> x, const Eigen::Map<Eigen::MatrixXd> m,
                            const Eigen::Map<Eigen::MatrixXd> V0, const Eigen::Map<Eigen::MatrixXd> U0,
                            const Eigen::Map<Eigen::VectorXd> mu0, const Eigen::Map<Eigen::VectorXd> sigma1,
                            double sigma2, const Rcpp::NumericVector par2, double b1, int maxiter,
                            double tol, double max_col_frac) {
  const int n = x.rows(), p = x.cols(), k = V0.cols();
  const Tanh rho1 = rho1_function(b1);
  const Tanh rho2 = rho2_function(par2);
  MatrixXd V = V0, U = U0;
  VectorXd mu = mu0;
  MatrixXd UV_old = U * V.transpose();
  State s = update_state(x, m, UV_old, mu, sigma1, sigma2, rho1, rho2);
  std::vector<double> objective{s.objective};
  bool converged = false, stopped = false;
  int iter = 0;
  while (iter < maxiter) {
    ++iter;
    const MatrixXd U_prev = U, V_prev = V;
    const VectorXd mu_prev = mu;
    const State s_prev = s;
    // (a) loadings, one variable at a time, with the full weights
    MatrixXd V1(p, k);
    for (int j = 0; j < p; ++j) {
      MatrixXd Uw = U.array().colwise() * s.W.col(j).array();
      const VectorXd r = (x.col(j).array() - mu[j]).matrix();
      V1.row(j) = pinv_solve(Uw.transpose() * U, Uw.transpose() * r).transpose();
    }
    // (b) scores, one case at a time, with the cellwise weights only
    MatrixXd U1(n, k);
    for (int i = 0; i < n; ++i) {
      MatrixXd Vw = V1.array().colwise() * s.cell.row(i).transpose().array();
      const VectorXd r = x.row(i).transpose() - mu;
      U1.row(i) = pinv_solve(Vw.transpose() * V1, Vw.transpose() * r).transpose();
    }
    // orthonormal loadings spanning the same subspace, with the same fit
    Eigen::JacobiSVD<MatrixXd> svd(V1, Eigen::ComputeThinU | Eigen::ComputeThinV);
    MatrixXd E = svd.matrixU() * svd.matrixV().transpose();
    const MatrixXd EV = E.transpose() * V1;
    for (int l = 0; l < k; ++l) {
      if (EV(l, l) < 0) E.col(l) *= -1;
    }
    U = U1 * (V1.transpose() * E);
    V = E;
    // (c) center with the full weights, then moved inside the subspace
    const MatrixXd UVt = U * V.transpose();
    VectorXd A = s.W.colwise().sum().transpose();
    if (A.minCoeff() / n < 1e-5) {
      Eigen::Index j;
      A.minCoeff(&j);
      Rcpp::stop("Variable %d has a tiny average weight in cellPCA.", static_cast<int>(j) + 1);
    }
    VectorXd mu_fit(p), mu_mean(p);
    for (int j = 0; j < p; ++j) {
      double b_fit = 0, b_mean = 0;
      for (int i = 0; i < n; ++i) {
        b_fit += s.W(i, j) * (x(i, j) - UVt(i, j));
        b_mean += s.W(i, j) * x(i, j);
      }
      mu_fit[j] = b_fit / A[j];
      mu_mean[j] = b_mean / A[j];
    }
    const VectorXd u_shift = V.transpose() * (mu_mean - mu_fit);
    mu = mu_fit + V * u_shift;
    U.rowwise() -= u_shift.transpose();
    // (d) weights
    const MatrixXd UV = U * V.transpose();
    s = update_state(x, m, UV, mu, sigma1, sigma2, rho1, rho2);
    const double diff = std::sqrt((UV - UV_old).squaredNorm() / (n * p)) /
                        (std::sqrt(UV_old.squaredNorm() / (n * p)) + 1e-20);
    UV_old = UV;
    objective.push_back(s.objective);
    if (max_col_frac < 1) {
      int max_zero = 0;
      for (int j = 0; j < p; ++j) {
        int zeros = 0;
        for (int i = 0; i < n; ++i) zeros += (m(i, j) == 0 || s.W(i, j) == 0);
        max_zero = std::max(max_zero, zeros);
      }
      if (max_zero > max_col_frac * n) {
        U = U_prev;
        V = V_prev;
        mu = mu_prev;
        s = s_prev;
        stopped = true;
        break;
      }
    }
    if (diff <= tol) {
      converged = true;
      break;
    }
  }
  return Rcpp::List::create(Rcpp::Named("V") = V, Rcpp::Named("U") = U, Rcpp::Named("mu") = mu,
                            Rcpp::Named("cell_weights") = s.cell, Rcpp::Named("case_weights") = s.casew,
                            Rcpp::Named("t") = s.t, Rcpp::Named("objective") = objective,
                            Rcpp::Named("iterations") = iter, Rcpp::Named("converged") = converged,
                            Rcpp::Named("stopped") = stopped);
}

// Scores of new cases for a cellPCA fit (cellPCA_predict of the reference
// code): starting from the projection of the observed cells, the scores and
// the cellwise weights are updated until the relative change of the scores
// is below `tol`. Cases without observed cells get NA scores.
// [[Rcpp::export]]
Rcpp::List cellpca_predict_cpp(const Eigen::Map<Eigen::MatrixXd> x, const Eigen::Map<Eigen::MatrixXd> m,
                               const Eigen::Map<Eigen::MatrixXd> V, const Eigen::Map<Eigen::VectorXd> mu,
                               const Eigen::Map<Eigen::VectorXd> sigma1, double b1, int maxiter, double tol) {
  const int n = x.rows(), p = x.cols(), k = V.cols();
  const Tanh rho1 = rho1_function(b1);
  MatrixXd U(n, k);
  MatrixXd weights = MatrixXd::Zero(n, p);
  VectorXd rows_obs = m.rowwise().sum();
  for (int i = 0; i < n; ++i) {
    const VectorXd xc = x.row(i).transpose() - mu;
    U.row(i) = ((V.array().colwise() * m.row(i).transpose().array()).matrix().transpose() * xc).transpose();
  }
  auto cell_weights = [&](const MatrixXd& Ucur) {
    const MatrixXd UV = Ucur * V.transpose();
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < p; ++j) {
        if (m(i, j) == 0) {
          weights(i, j) = 0;
        } else {
          double z = (x(i, j) - mu[j] - UV(i, j)) / sigma1[j];
          if (!std::isfinite(z)) z = 0;
          weights(i, j) = rho1.weight(z);
        }
      }
    }
  };
  cell_weights(U);
  for (int iter = 0; iter < maxiter; ++iter) {
    const MatrixXd U_old = U;
    for (int i = 0; i < n; ++i) {
      if (rows_obs[i] == 0) continue;
      MatrixXd Vw = V.array().colwise() * weights.row(i).transpose().array();
      const VectorXd r = x.row(i).transpose() - mu;
      U.row(i) = pinv_solve(Vw.transpose() * V, Vw.transpose() * r).transpose();
    }
    const double diff = (U - U_old).norm() / (U_old.norm() + 1e-20);
    cell_weights(U);
    if (diff <= tol) break;
  }
  Rcpp::NumericMatrix scores(n, k);
  for (int i = 0; i < n; ++i) {
    for (int l = 0; l < k; ++l) scores(i, l) = rows_obs[i] == 0 ? NA_REAL : U(i, l);
  }
  return Rcpp::List::create(Rcpp::Named("scores") = scores, Rcpp::Named("cell_weights") = weights);
}
