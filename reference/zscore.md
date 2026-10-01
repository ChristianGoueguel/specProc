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
  und Schätzungen von Kovarianzmatrizen. PhD thesis, ETH Zürich.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# iron contents (mg/kg): a few samples are far above the others
fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
head(zscore(fe))
#> # A tibble: 6 × 3
#>    data score flag 
#>   <int> <dbl> <lgl>
#> 1  2060  8.82 TRUE 
#> 2  1290  5.16 TRUE 
#> 3  1210  4.78 TRUE 
#> 4  1190  4.68 TRUE 
#> 5  1080  4.16 TRUE 
#> 6  1060  4.06 TRUE 
head(zscore(fe, robust = TRUE))
#> # A tibble: 6 × 3
#>    data score flag 
#>   <int> <dbl> <lgl>
#> 1  2060  24.5 TRUE 
#> 2  1290  14.7 TRUE 
#> 3  1210  13.7 TRUE 
#> 4  1190  13.4 TRUE 
#> 5  1080  12.0 TRUE 
#> 6  1060  11.7 TRUE 
```
