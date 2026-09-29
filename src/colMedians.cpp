#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

// Column medians of a numeric matrix, by partial sorting (std::nth_element).
// With na_rm = false, a column with a missing value (NA/NaN) returns NA; with
// na_rm = true, missing values are dropped, and a column that has only
// missing values returns NA, as stats::median() does.
// [[Rcpp::export]]
NumericVector col_medians_cpp(NumericMatrix x, bool na_rm) {
  const int nrows = x.nrow();
  const int ncols = x.ncol();
  NumericVector result(ncols);
  std::vector<double> values;
  values.reserve(nrows);

  for (int j = 0; j < ncols; ++j) {
    values.clear();
    bool missing = false;
    for (int i = 0; i < nrows; ++i) {
      const double value = x(i, j);
      if (ISNAN(value)) {
        missing = true;
        if (!na_rm) break;
      } else {
        values.push_back(value);
      }
    }
    const int n = values.size();
    if ((missing && !na_rm) || n == 0) {
      result[j] = NA_REAL;
      continue;
    }
    const int half = n / 2;
    std::nth_element(values.begin(), values.begin() + half, values.end());
    double median = values[half];
    if (n % 2 == 0) {
      // the largest value of the lower half
      const double lower = *std::max_element(values.begin(), values.begin() + half);
      median = (lower + median) / 2.0;
    }
    result[j] = median;
  }
  return result;
}
