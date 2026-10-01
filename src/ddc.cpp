// Detection of deviating cells (DDC): the neighbor search and the cell
// predictions, which dominate the cost of the algorithm for spectra with
// thousands of channels. See ddc() in R/ddc.R.
#include <RcppEigen.h>
#include <algorithm>
#include <cmath>
#include <vector>

// [[Rcpp::depends(RcppEigen)]]

namespace {

// Wrapping function of Raymaekers and Rousseeuw (2021): the identity up to
// b, a smooth descent to zero at c, and zero beyond.
double wrap_value(double z) {
  const double b = 1.5, c = 4.0, q1 = 1.540793, q2 = 0.8622731;
  const double a = std::fabs(z);
  if (a <= b) return z;
  if (a >= c) return 0.0;
  const double v = q1 * std::tanh(q2 * (c - a));
  return z < 0 ? -v : v;
}

double median_of(std::vector<double>& v) {
  const std::size_t n = v.size();
  const std::size_t mid = n / 2;
  std::nth_element(v.begin(), v.begin() + mid, v.end());
  double m = v[mid];
  if (n % 2 == 0) {
    const double lower = *std::max_element(v.begin(), v.begin() + mid);
    m = (m + lower) / 2.0;
  }
  return m;
}

}  // namespace

// Neighbors of each column: the (at most `maxnb`) other columns whose
// wrapped correlation is at least `corrlim` in absolute value, with the
// robust slope of the column on each neighbor (the median of the ratios of
// the cells, over the cells of the neighbor at least `min_abs` in absolute
// value). `u` holds the robustly standardized data with the univariate
// outliers and missing values as NaN.
// [[Rcpp::export]]
Rcpp::List ddc_neighbors_cpp(const Eigen::Map<Eigen::MatrixXd> u, double corrlim, int maxnb,
                             double min_abs, int block) {
  const int n = u.rows();
  const int p = u.cols();
  // wrapped columns, centered and scaled to unit length
  Eigen::MatrixXd w(n, p);
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i < n; ++i) {
      const double z = u(i, j);
      w(i, j) = std::isfinite(z) ? wrap_value(z) : 0.0;
    }
    w.col(j).array() -= w.col(j).mean();
    const double norm = w.col(j).norm();
    if (norm > 0) {
      w.col(j) /= norm;
    } else {
      w.col(j).setZero();
    }
  }
  maxnb = std::min(maxnb, p - 1);
  Rcpp::IntegerMatrix index(p, std::max(maxnb, 1));
  Rcpp::NumericMatrix correlation(p, std::max(maxnb, 1));
  Rcpp::NumericMatrix slope(p, std::max(maxnb, 1));
  std::fill(index.begin(), index.end(), NA_INTEGER);
  std::fill(correlation.begin(), correlation.end(), NA_REAL);
  std::fill(slope.begin(), slope.end(), NA_REAL);
  std::vector<int> order(p);
  std::vector<double> ratios;
  ratios.reserve(n);
  for (int start = 0; start < p; start += block) {
    const int len = std::min(block, p - start);
    const Eigen::MatrixXd r = w.middleCols(start, len).transpose() * w;  // len x p
    for (int b = 0; b < len; ++b) {
      const int j = start + b;
      int count = 0;
      for (int h = 0; h < p; ++h) {
        if (h != j && std::fabs(r(b, h)) >= corrlim) order[count++] = h;
      }
      const int keep = std::min(count, maxnb);
      std::partial_sort(order.begin(), order.begin() + keep, order.begin() + count,
                        [&](int a1, int a2) { return std::fabs(r(b, a1)) > std::fabs(r(b, a2)); });
      for (int k = 0; k < keep; ++k) {
        const int h = order[k];
        ratios.clear();
        for (int i = 0; i < n; ++i) {
          const double uh = u(i, h);
          const double uj = u(i, j);
          if (std::isfinite(uh) && std::isfinite(uj) && std::fabs(uh) >= min_abs) {
            ratios.push_back(uj / uh);
          }
        }
        index(j, k) = h + 1;
        correlation(j, k) = r(b, h);
        slope(j, k) = ratios.size() >= 3 ? median_of(ratios) : r(b, h);
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("index") = index,
                            Rcpp::Named("correlation") = correlation,
                            Rcpp::Named("slope") = slope);
}

// Prediction of each cell from its neighbors: the mean of slope * neighbor
// value, weighted by the absolute correlations, over the neighbors whose
// cell is available (0, the column center, when none is).
// [[Rcpp::export]]
Eigen::MatrixXd ddc_predict_cpp(const Eigen::Map<Eigen::MatrixXd> u, const Rcpp::IntegerMatrix index,
                                const Rcpp::NumericMatrix correlation, const Rcpp::NumericMatrix slope) {
  const int n = u.rows();
  const int p = u.cols();
  const int nb = index.ncol();
  Eigen::MatrixXd pred = Eigen::MatrixXd::Zero(n, p);
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i < n; ++i) {
      double num = 0.0, den = 0.0;
      for (int k = 0; k < nb; ++k) {
        const int h = index(j, k);
        if (h == NA_INTEGER) break;
        const double uh = u(i, h - 1);
        if (!std::isfinite(uh)) continue;
        const double weight = std::fabs(correlation(j, k));
        num += weight * slope(j, k) * uh;
        den += weight;
      }
      pred(i, j) = den > 0 ? num / den : 0.0;
    }
  }
  return pred;
}
