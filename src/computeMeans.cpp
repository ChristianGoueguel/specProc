#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix computeMeans(NumericMatrix data) {
  int ncols = data.ncol();
  NumericMatrix result(1, ncols);

  for (int j = 0; j < ncols; ++j) {
    NumericVector col = data(_, j);
    NumericVector::iterator it = std::remove(col.begin(), col.end(), NA_REAL);
    col.erase(it, col.end());
    double mean = Rcpp::mean(col);
    result(0, j) = mean;
  }

  return result;
}
