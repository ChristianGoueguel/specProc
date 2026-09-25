#include <Rcpp.h>
using namespace Rcpp;

// Column means of a numeric matrix, ignoring missing values (NA/NaN).
// Columns that contain only missing values return NA.
// [[Rcpp::export]]
NumericMatrix computeMeans(NumericMatrix data) {
  const int nrows = data.nrow();
  const int ncols = data.ncol();
  NumericMatrix result(1, ncols);

  for (int j = 0; j < ncols; ++j) {
    double sum = 0.0;
    int count = 0;
    for (int i = 0; i < nrows; ++i) {
      const double value = data(i, j);
      if (!ISNAN(value)) {
        sum += value;
        ++count;
      }
    }
    result(0, j) = (count > 0) ? sum / count : NA_REAL;
  }

  return result;
}
