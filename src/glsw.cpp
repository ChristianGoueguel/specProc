#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]

Rcpp::NumericMatrix glsw_cpp(Rcpp::NumericMatrix X_diff, double alpha) {
  Eigen::Map<Eigen::MatrixXd> X_diff_map(as<Eigen::Map<Eigen::MatrixXd> >(X_diff));

  Eigen::MatrixXd C = X_diff_map.transpose() * X_diff_map;
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(C, Eigen::ComputeThinU | Eigen::ComputeThinV);

  Eigen::MatrixXd V = svd.matrixV();
  Eigen::VectorXd S = svd.singularValues();

  int n = X_diff_map.cols();
  Eigen::VectorXd D = ((S.array().square() / alpha) + Eigen::VectorXd::Constant(n, 1.0).array()).sqrt();

  Eigen::MatrixXd G = V * D.asDiagonal().inverse() * V.transpose();

  return Rcpp::wrap(G);
}
