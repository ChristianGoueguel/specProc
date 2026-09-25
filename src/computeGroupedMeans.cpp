#include <Rcpp.h>
using namespace Rcpp;

// Group-wise column means of a numeric matrix, ignoring missing values.
// `group` holds 1-based group codes (e.g. the integer codes of a factor);
// rows with a missing or out-of-range group code are skipped. The result has
// one row per group (1..ngroups); groups without data return NA.
// [[Rcpp::export]]
NumericMatrix computeGroupedMeans(NumericMatrix data, IntegerVector group, int ngroups) {
  const int nrows = data.nrow();
  const int ncols = data.ncol();

  if (group.size() != nrows) {
    stop("'group' must have one element per row of 'data'.");
  }
  if (ngroups < 1) {
    stop("'ngroups' must be at least 1.");
  }

  NumericMatrix sums(ngroups, ncols);
  IntegerMatrix counts(ngroups, ncols);

  for (int i = 0; i < nrows; ++i) {
    const int g = group[i];
    if (g == NA_INTEGER || g < 1 || g > ngroups) {
      continue;
    }
    for (int j = 0; j < ncols; ++j) {
      const double value = data(i, j);
      if (!ISNAN(value)) {
        sums(g - 1, j) += value;
        counts(g - 1, j) += 1;
      }
    }
  }

  NumericMatrix result(ngroups, ncols);
  for (int g = 0; g < ngroups; ++g) {
    for (int j = 0; j < ncols; ++j) {
      result(g, j) = (counts(g, j) > 0) ? sums(g, j) / counts(g, j) : NA_REAL;
    }
  }

  return result;
}
