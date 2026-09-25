# Biweight Scale

This function computes the biweight scale, a robust measure of scale or
dispersion for a numeric vector. The biweight scale is less sensitive to
outliers than the sample standard deviation.

## Usage

``` r
biweight_scale(x, loc = NULL, c = 9, reduced = FALSE, drop.na = FALSE)
```

## Arguments

- x:

  A numeric vector.

- loc:

  The location about which the scale is computed (default: median of
  `x`).

- c:

  A numeric value specifying the tuning constant for the biweight
  estimator (`c = 9` by default).

- reduced:

  A logical value specifying whether the sample size, *n*, should be
  reduced to the number of non-rejected values. If `TRUE`, *n* is
  reduced to the number of observations that pass a rejection criteria
  (\\\|u_i\| \< 1\\). If `FALSE` (default), *n* is equal to the length
  of `x` (the input data).

- drop.na:

  A logical value indicating whether to remove missing values (`NA`)
  from the calculations. If `TRUE`, missing values will be removed. If
  `FALSE` (the default), the result is `NA` when `x` contains missing
  values.

## Value

The biweight scale of `x`.

## Details

The biweight scale is the square root of the biweight midvariance:
\$\$\zeta = \frac{\sqrt{n \sum\_{\|u_i\|\<1} (x_i - M)^2 (1 - u_i^2)^4}}
{\left\| \sum\_{\|u_i\|\<1} (1 - u_i^2)(1 - 5u_i^2) \right\|}, \quad u_i
= \frac{x_i - M}{c \cdot \mathrm{MAD}}\$\$ where \\M\\ is the location
(the median by default) and MAD is the (unscaled) median absolute
deviation. For normally distributed data the biweight scale is a
consistent estimator of the standard deviation.

## References

- Mosteller, F., and Tukey, J. W. (1977). Data Analysis and Regression:
  A Second Course in Statistics. Addison-Wesley, pp. 203-209.

- Beers, T.C., Flynn, K., Gebhardt, K., (1990). Measures of location and
  scale for velocities in clusters of galaxies - A robust approach. The
  Astronomical Journal, 100:32-46.

## Author

Christian L. Goueguel

## Examples

``` r
# Example 1: Compute biweight scale for a vector
x <- c(seq(1,100))
tibble::tibble(
sd = stats::sd(x),
mad = stats::mad(x),
biscale = biweight_scale(x)
)
#> # A tibble: 1 × 3
#>      sd   mad biscale
#>   <dbl> <dbl>   <dbl>
#> 1  29.0  37.1    30.1

# Example 2: Biweight scale is robust to outliers
x <- c(seq(1,99), 1e3) # An outlier at 1000
tibble::tibble(
sd = stats::sd(x),
mad = stats::mad(x),
biscale = biweight_scale(x)
)
#> # A tibble: 1 × 3
#>      sd   mad biscale
#>   <dbl> <dbl>   <dbl>
#> 1  99.2  37.1    29.9
```
