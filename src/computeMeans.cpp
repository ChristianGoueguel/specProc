#include <Rcpp.h>
using namespace Rcpp;
#include <numeric> // For std::accumulate

// Enable C++11
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
double computeMeans(Rcpp::NumericMatrix values) {
  int nrows = values.nrow();
  int ncols = values.ncol();
  double sum = 0.0;
  int count = nrows * ncols;

  // Sum all values in the matrix
  for (int i = 0; i < nrows; ++i) {
    for (int j = 0; j < ncols; ++j) {
      sum += values(i, j);
    }
  }

  // Calculate the mean
  double mean = sum / count;
  return mean;
}
