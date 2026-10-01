# Biweight Midcovariance

This function computes the biweight midcovariance, a robust measure of
covariance between two numerical vectors. The biweight midcovariance is
less sensitive to outliers than the traditional covariance.

## Usage

``` r
biweight_midcovariance(x, y)
```

## Arguments

- x:

  A numeric vector.

- y:

  A numeric vector of the same length as `x`.

## Value

The biweight midcovariance between `x` and `y`.

## References

- Wilcox, R., (1997). Introduction to Robust Estimation and Hypothesis
  Testing. Academic Press

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# iron and manganese contents (mg/kg), with a few iron-rich samples
ok <- !is.na(forageLIBS$Fe)
c(covariance = stats::cov(forageLIBS$Fe[ok], forageLIBS$Mn[ok]),
  biweight = biweight_midcovariance(forageLIBS$Fe[ok], forageLIBS$Mn[ok]))
#> covariance   biweight 
#>   1146.068    322.688 
```
