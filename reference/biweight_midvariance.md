# Biweight Midvariance

This function calculates the biweight midvariance of a numeric vector,
which is a robust measure of scale that can be used to estimate the
variability of the data while being resistant to the influence of
outliers.

## Usage

``` r
biweight_midvariance(x, drop.na = FALSE)
```

## Arguments

- x:

  A numeric vector.

- drop.na:

  A logical value indicating whether to remove missing values (`NA`)
  from the calculations. If `TRUE`, missing values will be removed. If
  `FALSE` (the default), the result is `NA` when `x` contains missing
  values.

## Value

The biweight midvariance of the input vector.

## Details

For scale estimators, the standard deviation (or variance) is the
optimal estimator for Gaussian data. However, it is not resistant and it
does not have robustness of efficiency. In robust statistics, the median
absolute deviation (MAD) is a resistant estimate, but it has only modest
robustness of efficiency, while the biweight midvariance estimator is
both resistant and robust of efficiency.

## References

- Wilcox, R., (1997). Introduction to Robust Estimation and Hypothesis
  Testing. Academic Press

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# iron contents (mg/kg): a few samples are far above the others
fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
c(variance = stats::var(fe), biweight = biweight_midvariance(fe))
#>  variance  biweight 
#> 44226.244  6756.178 
```
