// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

// External parameter orthogonalization (Roger et al., 2003).
//
// The clutter subspace is spanned by the `ncomp` dominant right singular
// vectors P of the clutter matrix D. X is projected onto the orthogonal
// complement: X_epo = X - (X P) P'. The p x p projection matrix is never
// formed, so memory stays O(n p) even for spectra with many channels.
// [[Rcpp::export]]
Rcpp::List epo_cpp(const Eigen::Map<Eigen::MatrixXd> X,
                   const Eigen::Map<Eigen::MatrixXd> D,
                   int ncomp) {
  if (X.cols() != D.cols()) {
    Rcpp::stop("'x' and the clutter matrix must have the same number of columns.");
  }
  Eigen::BDCSVD<Eigen::MatrixXd> svd(D, Eigen::ComputeThinV);
  const Eigen::Index rank = svd.singularValues().size();
  if (ncomp < 1 || ncomp > rank) {
    Rcpp::stop("'ncomp' must be between 1 and min(dim(clutter)).");
  }

  const Eigen::MatrixXd P = svd.matrixV().leftCols(ncomp);
  const Eigen::MatrixXd XP = X * P;
  const Eigen::MatrixXd X_clutter = XP * P.transpose();
  const Eigen::MatrixXd X_corrected = X - X_clutter;

  return Rcpp::List::create(
    Rcpp::Named("correction") = X_corrected,
    Rcpp::Named("clutter") = X_clutter,
    Rcpp::Named("loadings") = P,
    Rcpp::Named("singular_values") = svd.singularValues().head(ncomp)
  );
}
