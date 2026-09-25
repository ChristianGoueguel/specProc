// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include "glsw.h"

// y-gradient GLSW filtering matrix: C = X_diff' W^2 X_diff with W = diag(w_i).
// The weighting is applied by scaling the rows of X_diff, which avoids
// building the n x n diagonal matrix.
// [[Rcpp::export]]
Eigen::MatrixXd yGradientglswCpp(const Eigen::Map<Eigen::MatrixXd> X_diff,
                                 const Eigen::Map<Eigen::VectorXd> w_i,
                                 double alpha) {
  if (w_i.size() != X_diff.rows()) {
    Rcpp::stop("'w_i' must have one weight per row of 'X_diff'.");
  }
  if (!(alpha > 0.0)) Rcpp::stop("'alpha' must be positive.");
  const Eigen::MatrixXd Xw = w_i.asDiagonal() * X_diff;
  return glsw_filter(Xw, alpha);
}
