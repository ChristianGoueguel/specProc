# Classical or Robust Z-Score

This function calculates classical or robust z-score (standardization)
for a numeric vector.

## Usage

``` r
zscore(x, cutoff = 3, robust = FALSE, drop.na = FALSE)
```

## Arguments

- x:

  A numeric vector.

- cutoff:

  A numeric value indicating the threshold above which data points are
  identified and flagged as potential outliers. By default,
  `cutoff = 3`.

- robust:

  A logical value indicating whether to calculate classical or robust
  z-score. If `FALSE` (the default), uses the classical approach. If
  `TRUE`, computes the robust method, i.e. the so-called Stahel-Donoho
  outlyingness.

- drop.na:

  A logical value indicating whether to remove missing values (`NA`)
  from the calculations. If `TRUE`, missing values will be removed. If
  `FALSE` (the default), missing values are kept in the output (with a
  missing score) and ignored when computing the location and scale.

## Value

A tibble with two columns:

- `data`: The original numeric values.

- `score`: The calculated z-scores.

- `flag`: `TRUE` if the corresponding data point is flagged as a
  potential outlier, and `FALSE` otherwise.

## Details

Z-scores are useful for comparing data points from different
distributions because they are dimensionless and standardized. A
positive z-score indicates that the data point is above the mean (or the
median in the robust approach), while a negative z-score indicates that
the data point is below the mean (or the median). One common rule to
detect outliers using z-scores is the "three-sigma rule", in which data
points with an absolute z-score greater than 3 (\|z\| \> 3) can be
considered potential outliers (default), as they fall outside the range
that covers 99.7% of the data points in a normal distribution. (Note
that a cutoff of \|z\| \> 2.5 is also often used).

## References

- Rousseeuw, P. J., and Croux, C. (1993). Alternatives to the median
  absolute deviation. Journal of the American Statistical Association,
  88(424), 1273-1283.

- Rousseeuw, P. J., and Hubert, M. (2011). Robust statistics for outlier
  detection. Wiley Interdisciplinary Reviews: Data Mining and Knowledge
  Discovery, 1(1), 73-79.

- Donoho, D., (1982). Breakdown properties of multivariate location
  estimators. Ph.D. Qualifying paper, Dept. Statistics, Harvard
  University, Boston.

- Stahel, W., (1981). Robuste Schätzungen: infinitesimale Optimalität
  und Schätzungen vonKovarianzmatrizen. PhD thesis, ETH Zürich.

## Author

Christian L. Goueguel

## Examples

``` r
x <- c(1:5, 100)
# Non-robust approach
zscore(x)
#> # A tibble: 6 × 3
#>    data  score flag 
#>   <dbl>  <dbl> <lgl>
#> 1   100  2.04  FALSE
#> 2     5 -0.358 FALSE
#> 3     4 -0.383 FALSE
#> 4     3 -0.408 FALSE
#> 5     2 -0.433 FALSE
#> 6     1 -0.458 FALSE

# Robust approach
zscore(x, robust = TRUE)
#> # A tibble: 6 × 3
#>    data  score flag 
#>   <dbl>  <dbl> <lgl>
#> 1   100 43.4   TRUE 
#> 2     5  0.674 FALSE
#> 3     4  0.225 FALSE
#> 4     3 -0.225 FALSE
#> 5     2 -0.674 FALSE
#> 6     1 -1.12  FALSE
```
