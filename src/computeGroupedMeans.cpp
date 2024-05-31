#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix computeGroupedMeans(NumericMatrix data, IntegerVector group) {
  int nrows = data.nrow();
  int ncols = data.ncol();
  int ngroups = Rcpp::max(group) + 1;  // Number of unique groups

  NumericVector group_means(ncols * ngroups);  // Initialize vector to store group means
  IntegerVector group_counts(ncols * ngroups); // Initialize vector to store group counts

  for (int i = 0; i < nrows; ++i) {
    int g = group[i] - 1;  // Adjust for 1-based indexing in R
    for (int j = 0; j < ncols; ++j) {
      int idx = g * ncols + j;
      double value = data(i, j);
      if (!NumericVector::is_na(value)) {
        group_means[idx] += value;
        group_counts[idx]++;
      }
    }
  }

  for (int i = 0; i < ngroups * ncols; ++i) {
    if (group_counts[i] > 0) {
      group_means[i] /= group_counts[i];
    } else {
      group_means[i] = NA_REAL;
    }
  }

  NumericMatrix result(ngroups, ncols);
  for (int i = 0; i < ngroups; ++i) {
    for (int j = 0; j < ncols; ++j) {
      result(i, j) = group_means[i * ncols + j];
    }
  }

  return result;
}
