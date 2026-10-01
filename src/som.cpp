// Batch self-organizing map (SOM). See som() in R/som.R.
#include <RcppEigen.h>
#include <algorithm>
#include <cmath>
#include <vector>

// [[Rcpp::depends(RcppEigen)]]

namespace {

using Eigen::MatrixXd;
using Eigen::VectorXd;

// Squared Euclidean distances between the rows of x (n x p) and of w (m x p).
MatrixXd squared_distances(const MatrixXd& x, const VectorXd& x_norm2, const MatrixXd& w) {
  const VectorXd w_norm2 = w.rowwise().squaredNorm();
  MatrixXd d = -2.0 * x * w.transpose();
  d.colwise() += x_norm2;
  d.rowwise() += w_norm2.transpose();
  return d.cwiseMax(0.0);
}

double median_of(std::vector<double> v) {
  const std::size_t n = v.size();
  const std::size_t mid = n / 2;
  std::nth_element(v.begin(), v.begin() + mid, v.end());
  double m = v[mid];
  if (n % 2 == 0) m = (m + *std::max_element(v.begin(), v.begin() + mid)) / 2.0;
  return m;
}

// Huber weights of the quantization errors, robustly standardized.
VectorXd huber_weights(const VectorXd& qe, double c) {
  const int n = qe.size();
  std::vector<double> v(qe.data(), qe.data() + n);
  const double med = median_of(v);
  std::vector<double> dev(n);
  for (int i = 0; i < n; ++i) dev[i] = std::fabs(qe[i] - med);
  const double mad = 1.4826 * median_of(dev);
  VectorXd w = VectorXd::Ones(n);
  if (mad <= 0) return w;
  for (int i = 0; i < n; ++i) {
    const double z = (qe[i] - med) / mad;
    if (z > c) w[i] = c / z;
  }
  return w;
}

}  // namespace

// Batch SOM training: at each epoch, every observation is assigned to its
// best-matching unit (BMU), and every codebook vector becomes the mean of the
// observations weighted by a Gaussian neighborhood kernel of the grid
// distance between its unit and their BMU (radius `sigma[epoch]`). With
// `robust`, the observations get Huber weights from their quantization
// errors, updated at each epoch.
// [[Rcpp::export]]
Rcpp::List som_batch_cpp(const Eigen::Map<Eigen::MatrixXd> x, const Eigen::Map<Eigen::MatrixXd> init,
                         const Eigen::Map<Eigen::MatrixXd> grid_dist2, const Eigen::Map<Eigen::VectorXd> sigma,
                         bool robust, double huber_c) {
  const int n = x.rows();
  const int p = x.cols();
  const int m = init.rows();
  const VectorXd x_norm2 = x.rowwise().squaredNorm();
  MatrixXd w = init;
  VectorXd weights = VectorXd::Ones(n);
  std::vector<int> bmu(n);
  VectorXd qe(n);
  for (int epoch = 0; epoch < sigma.size(); ++epoch) {
    const MatrixXd d = squared_distances(x, x_norm2, w);
    for (int i = 0; i < n; ++i) {
      Eigen::Index best;
      qe[i] = std::sqrt(d.row(i).minCoeff(&best));
      bmu[i] = static_cast<int>(best);
    }
    if (robust) weights = huber_weights(qe, huber_c);
    // weighted sums of the observations by BMU
    MatrixXd sums = MatrixXd::Zero(m, p);
    VectorXd counts = VectorXd::Zero(m);
    for (int i = 0; i < n; ++i) {
      sums.row(bmu[i]) += weights[i] * x.row(i);
      counts[bmu[i]] += weights[i];
    }
    const double s2 = 2.0 * sigma[epoch] * sigma[epoch];
    const MatrixXd h = (-grid_dist2.array() / s2).exp().matrix();
    const MatrixXd num = h * sums;
    const VectorXd den = h * counts;
    for (int j = 0; j < m; ++j) {
      if (den[j] > 1e-12) w.row(j) = num.row(j) / den[j];
    }
  }
  // final assignment, with the second-best unit for the topographic error
  const MatrixXd d = squared_distances(x, x_norm2, w);
  Rcpp::IntegerVector unit(n), second(n);
  Rcpp::NumericVector error(n);
  for (int i = 0; i < n; ++i) {
    int b1 = 0, b2 = -1;
    for (int j = 1; j < m; ++j) {
      if (d(i, j) < d(i, b1)) {
        b2 = b1;
        b1 = j;
      } else if (b2 < 0 || d(i, j) < d(i, b2)) {
        b2 = j;
      }
    }
    unit[i] = b1 + 1;
    second[i] = b2 + 1;
    error[i] = std::sqrt(d(i, b1));
  }
  if (robust) weights = huber_weights(Eigen::Map<VectorXd>(error.begin(), n), huber_c);
  return Rcpp::List::create(Rcpp::Named("codebook") = w, Rcpp::Named("unit") = unit,
                            Rcpp::Named("second") = second, Rcpp::Named("qe") = error,
                            Rcpp::Named("weights") = weights);
}

// Best-matching unit and quantization error of each row of x.
// [[Rcpp::export]]
Rcpp::List som_map_cpp(const Eigen::Map<Eigen::MatrixXd> x, const Eigen::Map<Eigen::MatrixXd> codebook) {
  const int n = x.rows();
  const VectorXd x_norm2 = x.rowwise().squaredNorm();
  const MatrixXd d = squared_distances(x, x_norm2, codebook);
  Rcpp::IntegerVector unit(n);
  Rcpp::NumericVector error(n);
  for (int i = 0; i < n; ++i) {
    Eigen::Index best;
    error[i] = std::sqrt(d.row(i).minCoeff(&best));
    unit[i] = static_cast<int>(best) + 1;
  }
  return Rcpp::List::create(Rcpp::Named("unit") = unit, Rcpp::Named("qe") = error);
}
