#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

// [[Rcpp::export]]
Eigen::MatrixXd yGradientglswCpp(const Eigen::MatrixXd& X_diff, const Eigen::VectorXd& w_i, double alpha) {
  int n = X_diff.rows(), p = X_diff.cols();
  Eigen::MatrixXd W = Eigen::MatrixXd::Zero(n, n);

  // Create diagonal matrix W
  W.diagonal() = w_i;

  // Calculate C
  Eigen::MatrixXd W_squared = W.array().square().matrix();
  Eigen::MatrixXd C = X_diff.transpose() * W_squared * X_diff;

  // Compute SVD of C
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(C, Eigen::ComputeThinU | Eigen::ComputeThinV);
  Eigen::VectorXd S = svd.singularValues();
  Eigen::MatrixXd V = svd.matrixV();

  // Calculate D
  Eigen::VectorXd D = (S.array().square() / alpha + 1.0).sqrt();

  // Calculate G
  Eigen::MatrixXd G = V * D.asDiagonal().inverse() * V.transpose();

  return G;
}
