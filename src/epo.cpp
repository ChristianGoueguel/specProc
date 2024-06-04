#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
Rcpp::List epo_cpp(Rcpp::NumericMatrix X, int ncomp) {
  Eigen::Map<Eigen::MatrixXd> X_map(as<Eigen::Map<Eigen::MatrixXd> >(X));
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(X_map, Eigen::ComputeThinU | Eigen::ComputeThinV);

  Eigen::MatrixXd U = svd.matrixU();
  Eigen::MatrixXd V = svd.matrixV();
  Eigen::VectorXd S = svd.singularValues();

  Eigen::MatrixXd clutter_direction = V.rightCols(ncomp);
  Eigen::MatrixXd I = Eigen::MatrixXd::Identity(X_map.cols(), X_map.cols());
  Eigen::MatrixXd Q = clutter_direction * clutter_direction.transpose();

  Eigen::MatrixXd X_corrected = X_map * (I - Q);
  Eigen::MatrixXd X_clutter = X_map * Q;

  return Rcpp::List::create(
    Rcpp::Named("correction") = Rcpp::wrap(X_corrected),
    Rcpp::Named("clutter") = Rcpp::wrap(X_clutter),
    Rcpp::Named("loadings") = Rcpp::wrap(clutter_direction)
  );
}
