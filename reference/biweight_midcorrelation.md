# Biweight Midcorrelation

This function computes the biweight midcorrelation between two numeric
vectors. The biweight midcorrelation is a robust measure of correlation
that is less sensitive to outliers than the traditional Pearson's
correlation coefficient.

## Usage

``` r
biweight_midcorrelation(x, y)
```

## Arguments

- x:

  A numeric vector.

- y:

  A numeric vector of the same length as `x`.

## Value

The biweight midcorrelation between `x` and `y`.

## Details

The biweight midcorrelation is calculated using the biweight
midvariances and biweight midcovariance, as described by Wilcox (1994).
It is bounded between -1 and 1.

## References

- Wilcox, R. R. (1994). The Biweight Midcorrelation: A Robust
  Correlation Technique for Two Samples. Journal of Statistical
  Computation and Simulation, 48(2):103-110.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
ok <- !is.na(forageLIBS$Fe)
c(pearson = stats::cor(forageLIBS$Fe[ok], forageLIBS$Mn[ok]),
  biweight = biweight_midcorrelation(forageLIBS$Fe[ok], forageLIBS$Mn[ok]))
#>   pearson  biweight 
#> 0.2186135 0.2112649 
```
