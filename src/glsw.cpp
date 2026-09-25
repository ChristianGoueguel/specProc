// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include "glsw.h"

// GLSW filtering matrix from a matrix of (mean-centered) differences.
// [[Rcpp::export]]
Eigen::MatrixXd glsw_cpp(const Eigen::Map<Eigen::MatrixXd> X_diff, double alpha) {
  if (!(alpha > 0.0)) Rcpp::stop("'alpha' must be positive.");
  return glsw_filter(X_diff, alpha);
}
