// Detection of deviating cells (DDC) of Rousseeuw and Van den Bossche
// (2018), following the C++ code of the cellWise package (DDC_cpp). See
// ddc() in R/ddc.R.
#include <RcppEigen.h>
#include <algorithm>
#include <cmath>
#include <vector>

// [[Rcpp::depends(RcppEigen)]]

namespace {

using Eigen::MatrixXd;
using Eigen::VectorXd;

const double PREC_SCALE = 1e-12;

double median_of(std::vector<double> v) {
  const std::size_t n = v.size();
  if (n == 0) return NA_REAL;
  const std::size_t mid = n / 2;
  std::nth_element(v.begin(), v.begin() + mid, v.end());
  double m = v[mid];
  if (n % 2 == 0) m = (m + *std::max_element(v.begin(), v.begin() + mid)) / 2.0;
  return m;
}

std::vector<double> finite_values(const double* x, int n) {
  std::vector<double> out;
  out.reserve(n);
  for (int i = 0; i < n; ++i) {
    if (std::isfinite(x[i])) out.push_back(x[i]);
  }
  return out;
}

// Wrapping (hyperbolic tangent psi) function with b = 1.5 and c = 4.
double wrap_value(double z) {
  const double b = 1.5, c = 4.0, q1 = 1.540793, q2 = 0.8622731;
  const double a = std::fabs(z);
  if (a <= b) return z;
  if (a > c) return 0.0;
  const double v = q1 * std::tanh(q2 * (c - a));
  return z < 0 ? -v : v;
}

// Weight of the 1-step location M-estimator with the tanh psi function.
double tanh_location_weight(double z) {
  const double a = std::fabs(z);
  if (a < 1.5) return 1.0;
  if (a > 4.0) return 0.0;
  return 1.540793 * std::tanh(0.8622731 * (4.0 - a)) / a;
}

// Huber rho function with b = 2.5 * qnorm(0.75), divided by its expectation
// (rhoHuber25 of cellWise).
double rho_huber25(double z) {
  return std::min(z * z, 2.8433526444973292) / 1.688942410165249;
}

// 1-step scale M-estimator around zero with rho_huber25, from the MAD.
double scale_1step_m(const std::vector<double>& x) {
  if (x.empty()) return 0.0;
  std::vector<double> a(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) a[i] = std::fabs(x[i]);
  const double s0 = 1.482602218505602 * median_of(a);
  if (s0 < PREC_SCALE) return 0.0;
  double sum = 0;
  for (double v : x) sum += rho_huber25(v / s0);
  return s0 * std::sqrt(sum / (0.5 * x.size()));
}

double scale_1step_m(const double* x, int n) {
  return scale_1step_m(finite_values(x, n));
}

// 1-step location M-estimator with biweight weights, from (median, MAD).
double loc_1step_m_biweight(const std::vector<double>& x) {
  if (x.empty()) return 0.0;
  const double m0 = median_of(x);
  std::vector<double> a(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) a[i] = std::fabs(x[i] - m0);
  const double s0 = 1.482602218505602 * median_of(a);
  if (s0 <= PREC_SCALE) return m0;
  double num = 0, den = 0;
  for (double v : x) {
    double u = (v - m0) / s0 * 1.482602218505602 / 3;
    u = 1 - u * u;
    const double w = std::pow((u + std::fabs(u)) / 2, 2);
    num += v * w;
    den += w;
  }
  return den > 0 ? num / den : m0;
}

// Reweighted univariate MCD (uniMcd of cellWise, with a center): location
// and scale of a fraction `alpha` of the values.
std::pair<double, double> unimcd(std::vector<double> y, double alpha) {
  const int n = y.size();
  if (n == 0) return {0.0, 0.0};
  const int quan = std::ceil(n * alpha);
  const int len = n - quan + 1;
  if (len == 1 || n < 3) {
    double mean = 0;
    for (double v : y) mean += v;
    mean /= n;
    double ss = 0;
    for (double v : y) ss += (v - mean) * (v - mean);
    return {mean, n > 1 ? std::sqrt(ss / (n - 1)) : 0.0};
  }
  std::sort(y.begin(), y.end());
  std::vector<double> sh(len), sh2(len), sq(len);
  sh[0] = 0;
  double ss0 = 0;
  for (int i = 0; i < quan; ++i) {
    sh[0] += y[i];
    ss0 += y[i] * y[i];
  }
  sh2[0] = sh[0] * sh[0] / quan;
  sq[0] = ss0 - sh2[0];
  for (int i = 1; i < len; ++i) {
    sh[i] = sh[i - 1] - y[i - 1] + y[i + quan - 1];
    sh2[i] = sh[i] * sh[i] / quan;
    sq[i] = sq[i - 1] - y[i - 1] * y[i - 1] + y[i + quan - 1] * y[i + quan - 1] - sh2[i] + sh2[i - 1];
  }
  const double sqmin = *std::min_element(sq.begin(), sq.end());
  double tied = 0;
  int ntied = 0;
  for (int i = 0; i < len; ++i) {
    if (sq[i] == sqmin) {
      tied += sh[i];
      ++ntied;
    }
  }
  const double initmean = tied / ntied / quan;
  std::vector<double> sqres(n);
  for (int i = 0; i < n; ++i) sqres[i] = (y[i] - initmean) * (y[i] - initmean);
  std::vector<double> sorted = sqres;
  std::sort(sorted.begin(), sorted.end());
  double cfac1;
  if (n < 10) {
    const double allc[] = {4.252, 1.970, 2.217, 1.648, 1.767, 1.482, 1.555};
    cfac1 = allc[n - 3];
  } else {
    cfac1 = n % 2 == 0 ? n / (n - 3.0) : n / (n - 3.4);
  }
  const double rawsq = cfac1 * cfac1 * sorted[quan - 1] / R::qchisq(static_cast<double>(quan) / n, 1, true, false);
  const double cutoff = rawsq * R::qchisq(0.975, 1, true, false);
  double sum = 0;
  int count = 0;
  for (int i = 0; i < n; ++i) {
    if (sqres[i] <= cutoff) {
      sum += y[i];
      ++count;
    }
  }
  const double loc = sum / count;
  double ss = 0;
  for (int i = 0; i < n; ++i) {
    if (sqres[i] <= cutoff) ss += (y[i] - loc) * (y[i] - loc);
  }
  const double temp = std::sqrt(std::max(0.0, ss) / (count - 1));
  double cfac2;
  if (n < 16) {
    const double alld[] = {1.475, 1.223, 1.253, 1.180, 1.181, 1.140, 1.143, 1.105, 1.114, 1.098, 1.103, 1.094, 1.093};
    cfac2 = alld[n - 3];
  } else {
    cfac2 = n / (n - 1.4);
  }
  return {loc, cfac2 * 1.0835 * temp};
}

// Slope of a regression without intercept of y on x: median of the ratios
// (between -2 and 2), then least squares on the observations whose residual
// is within q times the 1-step M scale of the residuals.
double slope_med_wls(const double* x, const double* y, int n, double q) {
  std::vector<double> ratios;
  ratios.reserve(n);
  for (int i = 0; i < n; ++i) {
    const double r = y[i] / x[i];
    if (std::isfinite(r)) ratios.push_back(r);
  }
  if (ratios.size() <= 3) return 0.0;
  double b = median_of(ratios);
  if (!std::isfinite(b)) return 0.0;
  b = std::max(-2.0, std::min(2.0, b));
  std::vector<double> res;
  res.reserve(n);
  std::vector<double> resid(n);
  for (int i = 0; i < n; ++i) {
    resid[i] = y[i] - b * x[i];
    if (std::isfinite(resid[i])) res.push_back(resid[i]);
  }
  const double cutoff = q * scale_1step_m(res);
  double sxy = 0, sxx = 0;
  int count = 0;
  for (int i = 0; i < n; ++i) {
    if (std::isfinite(resid[i]) && std::fabs(resid[i]) <= cutoff) {
      sxy += x[i] * y[i];
      sxx += x[i] * x[i];
      ++count;
    }
  }
  if (count == 0) return 0.0;
  const double slope = sxy / sxx;
  return std::isfinite(slope) ? slope : 0.0;
}

// Robust correlation of Gnanadesikan-Kettenring with 1-step M scales,
// followed by a Pearson correlation (around zero) of the observations within
// the q quantile of the bivariate distances (corrGKWLS of cellWise).
double corr_gkwls(const double* x, const double* y, int n, double q) {
  std::vector<double> sum, dif;
  sum.reserve(n);
  dif.reserve(n);
  for (int i = 0; i < n; ++i) {
    const double s = x[i] + y[i];
    if (std::isfinite(s)) {
      sum.push_back(s);
      dif.push_back(x[i] - y[i]);
    }
  }
  if (sum.size() <= 3) return 0.0;
  const double ss = scale_1step_m(sum), sd = scale_1step_m(dif);
  double corr = (ss * ss - sd * sd) / 4;
  if (!std::isfinite(corr)) return 0.0;
  corr = std::max(-0.99, std::min(0.99, corr));
  const double det = std::fabs(1 - corr * corr);
  double sxy = 0, sxx = 0, syy = 0;
  int count = 0;
  for (int i = 0; i < n; ++i) {
    if (!std::isfinite(x[i] + y[i])) continue;
    const double rd2 = (x[i] * x[i] - 2 * corr * x[i] * y[i] + y[i] * y[i]) / det;
    if (rd2 < q) {
      sxy += x[i] * y[i];
      sxx += x[i] * x[i];
      syy += y[i] * y[i];
      ++count;
    }
  }
  if (count == 0 || sxx <= 0 || syy <= 0) return 0.0;
  return sxy / std::sqrt(sxx * syy);
}

struct Model {
  Rcpp::IntegerMatrix ngbrs;    // p x (1 + k), 0-based, -1 if inactive; column 0 is the variable itself
  Rcpp::NumericMatrix weights;  // absolute correlations (0 if below corrlim)
  Rcpp::NumericMatrix slopes;
  std::vector<bool> standalone;
};

// Estimates of the cells of the connected variables from their neighbors
// (weighted mean of slope * neighbor, including the variable itself).
MatrixXd predict_cells(const MatrixXd& U, const Model& m) {
  const int n = U.rows(), p = U.cols(), nb = m.ngbrs.ncol();
  MatrixXd est = U;
  for (int j = 0; j < p; ++j) {
    if (m.standalone[j]) continue;
    for (int i = 0; i < n; ++i) {
      double num = 0, den = 0;
      for (int l = 0; l < nb; ++l) {
        const int h = m.ngbrs(j, l);
        if (h < 0 || m.weights(j, l) <= 0) continue;
        const double v = m.slopes(j, l) * U(i, h) * m.weights(j, l);
        if (!std::isfinite(v)) continue;
        num += v;
        den += m.weights(j, l);
      }
      est(i, j) = den > 0 ? num / den : NA_REAL;
    }
  }
  return est;
}

}  // namespace

// The DDC algorithm on the data matrix x (NaN for missing values). With
// `fast`, the variables are standardized by the univariate MCD and a 1-step
// M location, and the neighbors are those with the largest wrapped
// correlations (computed by blocks of `block` variables); otherwise by
// 1-step M estimators and the robust correlations corr_gkwls.
// [[Rcpp::export]]
Rcpp::List ddc_core_cpp(const Eigen::Map<Eigen::MatrixXd> x, double tol_prob, double corrlim, int maxnb,
                        bool fast, int block) {
  const int n = x.rows(), p = x.cols();
  const double q_cell = std::sqrt(R::qchisq(tol_prob, 1, true, false));
  const double q_corr = R::qchisq(tol_prob, 2, true, false);

  // Step 1: robust standardization
  VectorXd loc(p), scale(p);
  for (int j = 0; j < p; ++j) {
    std::vector<double> col = finite_values(x.col(j).data(), n);
    if (fast) {
      const auto mcd = unimcd(col, 0.5);
      double l = mcd.first;
      if (mcd.second > PREC_SCALE) {
        double num = 0, den = 0;
        for (double v : col) {
          const double w = tanh_location_weight((v - mcd.first) / mcd.second);
          num += v * w;
          den += w;
        }
        if (den > 0) l = num / den;
      }
      loc[j] = l;
      scale[j] = mcd.second;
    } else {
      loc[j] = loc_1step_m_biweight(col);
      for (double& v : col) v -= loc[j];
      scale[j] = scale_1step_m(col);
    }
  }
  std::vector<bool> constant(p);
  MatrixXd Z(n, p);
  for (int j = 0; j < p; ++j) {
    constant[j] = !(scale[j] > PREC_SCALE) || !std::isfinite(loc[j]);
    if (constant[j]) {
      scale[j] = 1;
      if (!std::isfinite(loc[j])) loc[j] = 0;
    }
    for (int i = 0; i < n; ++i) {
      Z(i, j) = std::isfinite(x(i, j)) ? (constant[j] ? 0.0 : (x(i, j) - loc[j]) / scale[j]) : NA_REAL;
    }
  }

  // Step 2: univariate outliers set to NaN
  MatrixXd U = Z;
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i < n; ++i) {
      if (std::isfinite(U(i, j)) && std::fabs(U(i, j)) > q_cell) U(i, j) = NA_REAL;
    }
  }

  // Step 3: neighbors (largest absolute robust correlations) and slopes
  const int k = std::max(1, std::min(maxnb, p - 1));
  Model m;
  m.ngbrs = Rcpp::IntegerMatrix(p, k + 1);
  m.weights = Rcpp::NumericMatrix(p, k + 1);
  m.slopes = Rcpp::NumericMatrix(p, k + 1);
  std::fill(m.ngbrs.begin(), m.ngbrs.end(), -1);
  Rcpp::NumericMatrix robcors(p, k);
  std::vector<int> order(p);
  auto select = [&](int j, const std::vector<double>& corr) {
    int count = 0;
    for (int h = 0; h < p; ++h) {
      if (h != j) order[count++] = h;
    }
    const int keep = std::min(count, k);
    std::partial_sort(order.begin(), order.begin() + keep, order.begin() + count,
                      [&](int a1, int a2) { return std::fabs(corr[a1]) > std::fabs(corr[a2]); });
    for (int l = 0; l < keep; ++l) {
      const int h = order[l];
      robcors(j, l) = corr[h];
      m.ngbrs(j, l + 1) = h;
      m.weights(j, l + 1) = std::fabs(corr[h]) >= corrlim ? std::fabs(corr[h]) : 0.0;
    }
  };
  std::vector<double> corr(p);
  if (fast) {
    MatrixXd w(n, p);
    for (int j = 0; j < p; ++j) {
      for (int i = 0; i < n; ++i) w(i, j) = std::isfinite(Z(i, j)) ? wrap_value(Z(i, j)) : 0.0;
      w.col(j).array() -= w.col(j).mean();
      const double norm = w.col(j).norm();
      if (norm > 0) w.col(j) /= norm; else w.col(j).setZero();
    }
    for (int start = 0; start < p; start += block) {
      const int len = std::min(block, p - start);
      const MatrixXd r = w.middleCols(start, len).transpose() * w;
      for (int b = 0; b < len; ++b) {
        for (int h = 0; h < p; ++h) corr[h] = r(b, h);
        select(start + b, corr);
      }
    }
  } else {
    MatrixXd C = MatrixXd::Zero(p, p);
    for (int j = 0; j < p; ++j) {
      for (int h = j + 1; h < p; ++h) {
        C(j, h) = C(h, j) = corr_gkwls(U.col(h).data(), U.col(j).data(), n, q_corr);
      }
    }
    for (int j = 0; j < p; ++j) {
      for (int h = 0; h < p; ++h) corr[h] = C(j, h);
      select(j, corr);
    }
  }
  m.standalone.assign(p, true);
  for (int j = 0; j < p; ++j) {
    for (int l = 1; l <= k; ++l) {
      if (m.weights(j, l) > 0) {
        m.standalone[j] = false;
        m.slopes(j, l) = slope_med_wls(U.col(m.ngbrs(j, l)).data(), U.col(j).data(), n, q_cell);
      }
    }
    // the variable itself, with weight and slope 1
    m.ngbrs(j, 0) = j;
    m.weights(j, 0) = 1.0;
    m.slopes(j, 0) = 1.0;
  }

  // Step 4: estimated cells, Step 5: deshrinkage
  MatrixXd Zest = predict_cells(U, m);
  VectorXd deshrink = VectorXd::Ones(p);
  for (int j = 0; j < p; ++j) {
    if (m.standalone[j]) continue;
    deshrink[j] = slope_med_wls(Zest.col(j).data(), Z.col(j).data(), n, q_cell);
    Zest.col(j) *= deshrink[j];
  }
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i < n; ++i) {
      if (!std::isfinite(Zest(i, j))) Zest(i, j) = 0;
    }
  }

  // Step 6: standardized residuals and flagged cells
  MatrixXd res(n, p);
  VectorXd res_scale = VectorXd::Ones(p);
  Rcpp::LogicalMatrix flagged(n, p);
  for (int j = 0; j < p; ++j) {
    if (m.standalone[j]) {
      res.col(j) = Z.col(j);
      for (int i = 0; i < n; ++i) {
        flagged(i, j) = std::isfinite(Z(i, j)) && std::fabs(Z(i, j)) > q_cell;
      }
      continue;
    }
    res.col(j) = Z.col(j) - Zest.col(j);
    res_scale[j] = scale_1step_m(res.col(j).data(), n);
    if (!(res_scale[j] > PREC_SCALE)) res_scale[j] = 1;
    res.col(j) /= res_scale[j];
    for (int i = 0; i < n; ++i) {
      flagged(i, j) = std::isfinite(res(i, j)) && std::fabs(res(i, j)) > q_cell;
    }
  }

  // Step 7: row statistics
  Rcpp::NumericVector Ti(n);
  for (int i = 0; i < n; ++i) {
    double sum = 0;
    int count = 0;
    for (int j = 0; j < p; ++j) {
      if (std::isfinite(res(i, j))) {
        sum += std::erf(std::fabs(res(i, j)) / std::sqrt(2.0));
        ++count;
      }
    }
    Ti[i] = count > 0 ? sum / count - 0.5 : NA_REAL;
  }
  std::vector<double> ti = finite_values(Ti.begin(), n);
  const double med_ti = median_of(ti);
  for (double& v : ti) v = std::fabs(v - med_ti);
  double mad_ti = 1.482602218505602 * median_of(ti);
  if (!(mad_ti > 0)) mad_ti = 1;
  for (int i = 0; i < n; ++i) Ti[i] = (Ti[i] - med_ti) / mad_ti;

  // the estimated cells on the original scale
  MatrixXd Xest = Zest;
  for (int j = 0; j < p; ++j) Xest.col(j) = Zest.col(j) * scale[j] + VectorXd::Constant(n, loc[j]);

  Rcpp::IntegerMatrix ngbrs_r(p, k + 1);
  for (int j = 0; j < p; ++j) {
    for (int l = 0; l <= k; ++l) ngbrs_r(j, l) = m.ngbrs(j, l) < 0 ? NA_INTEGER : m.ngbrs(j, l) + 1;
  }
  Rcpp::LogicalVector standalone(p);
  for (int j = 0; j < p; ++j) standalone[j] = m.standalone[j];
  return Rcpp::List::create(
    Rcpp::Named("loc") = loc, Rcpp::Named("scale") = scale, Rcpp::Named("std_resid") = res,
    Rcpp::Named("estimate") = Xest, Rcpp::Named("flagged") = flagged, Rcpp::Named("Ti") = Ti,
    Rcpp::Named("med_ti") = med_ti, Rcpp::Named("mad_ti") = mad_ti, Rcpp::Named("ngbrs") = ngbrs_r,
    Rcpp::Named("weights") = m.weights, Rcpp::Named("slopes") = m.slopes, Rcpp::Named("robcors") = robcors,
    Rcpp::Named("deshrink") = deshrink, Rcpp::Named("res_scale") = res_scale,
    Rcpp::Named("standalone") = standalone);
}

// DDC of new data with a fitted model (DDCpredict of cellWise): the cells
// are standardized with the location and scale of the fit, predicted from
// the same neighbors and slopes, and flagged with the residual scales of
// the fit.
// [[Rcpp::export]]
Rcpp::List ddc_apply_cpp(const Eigen::Map<Eigen::MatrixXd> x, const Eigen::Map<Eigen::VectorXd> loc,
                         const Eigen::Map<Eigen::VectorXd> scale, const Rcpp::IntegerMatrix ngbrs,
                         const Rcpp::NumericMatrix weights, const Rcpp::NumericMatrix slopes,
                         const Eigen::Map<Eigen::VectorXd> deshrink, const Eigen::Map<Eigen::VectorXd> res_scale,
                         const Rcpp::LogicalVector standalone, double tol_prob, double med_ti, double mad_ti) {
  const int n = x.rows(), p = x.cols();
  const double q_cell = std::sqrt(R::qchisq(tol_prob, 1, true, false));
  Model m;
  m.ngbrs = Rcpp::IntegerMatrix(ngbrs.nrow(), ngbrs.ncol());
  for (int j = 0; j < ngbrs.nrow(); ++j) {
    for (int l = 0; l < ngbrs.ncol(); ++l) {
      m.ngbrs(j, l) = ngbrs(j, l) == NA_INTEGER ? -1 : ngbrs(j, l) - 1;
    }
  }
  m.weights = weights;
  m.slopes = slopes;
  m.standalone.resize(p);
  for (int j = 0; j < p; ++j) m.standalone[j] = standalone[j];
  MatrixXd Z(n, p), U(n, p);
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i < n; ++i) {
      Z(i, j) = std::isfinite(x(i, j)) ? (x(i, j) - loc[j]) / scale[j] : NA_REAL;
      U(i, j) = std::isfinite(Z(i, j)) && std::fabs(Z(i, j)) <= q_cell ? Z(i, j) : NA_REAL;
    }
  }
  MatrixXd Zest = predict_cells(U, m);
  for (int j = 0; j < p; ++j) {
    if (!m.standalone[j]) Zest.col(j) *= deshrink[j];
    for (int i = 0; i < n; ++i) {
      if (!std::isfinite(Zest(i, j))) Zest(i, j) = 0;
    }
  }
  MatrixXd res(n, p);
  Rcpp::LogicalMatrix flagged(n, p);
  for (int j = 0; j < p; ++j) {
    res.col(j) = m.standalone[j] ? Z.col(j) : MatrixXd((Z.col(j) - Zest.col(j)) / res_scale[j]);
    for (int i = 0; i < n; ++i) flagged(i, j) = std::isfinite(res(i, j)) && std::fabs(res(i, j)) > q_cell;
  }
  Rcpp::NumericVector Ti(n);
  for (int i = 0; i < n; ++i) {
    double sum = 0;
    int count = 0;
    for (int j = 0; j < p; ++j) {
      if (std::isfinite(res(i, j))) {
        sum += std::erf(std::fabs(res(i, j)) / std::sqrt(2.0));
        ++count;
      }
    }
    Ti[i] = count > 0 ? (sum / count - 0.5 - med_ti) / mad_ti : NA_REAL;
  }
  MatrixXd Xest(n, p);
  for (int j = 0; j < p; ++j) Xest.col(j) = Zest.col(j) * scale[j] + VectorXd::Constant(n, loc[j]);
  return Rcpp::List::create(Rcpp::Named("std_resid") = res, Rcpp::Named("estimate") = Xest,
                            Rcpp::Named("flagged") = flagged, Rcpp::Named("Ti") = Ti);
}

// Reweighted univariate MCD (location and scale) of the finite values of x.
// [[Rcpp::export]]
Rcpp::NumericVector unimcd_cpp(const Rcpp::NumericVector x, double alpha) {
  const auto r = unimcd(finite_values(x.begin(), x.size()), alpha);
  return Rcpp::NumericVector::create(Rcpp::Named("location") = r.first, Rcpp::Named("scale") = r.second);
}

// 1-step M scale around zero (Huber) of each column, without missing values.
// [[Rcpp::export]]
Rcpp::NumericVector scale_1step_cols_cpp(const Eigen::Map<Eigen::MatrixXd> x) {
  Rcpp::NumericVector out(x.cols());
  for (int j = 0; j < x.cols(); ++j) out[j] = scale_1step_m(x.col(j).data(), x.rows());
  return out;
}
