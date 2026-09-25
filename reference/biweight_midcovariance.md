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
# Example 1: Compute biweight midcovariance for two vectors
x <- c(1, 2, 3, 4, 5)
y <- c(2, 3, 4, 5, 6)
stats::cov(x, y)
#> [1] 2.5
biweight_midcovariance(x, y)
#> [1] 2.297064

# Example 2: Biweight midcovariance is robust to outliers
x <- c(1, 2, 3, 4, 100)  # An outlier at 100
y <- c(2, 3, 4, 5, 6)
stats::cov(x, y)
#> [1] 50
biweight_midcovariance(x, y)
#> [1] 1.689954
```
