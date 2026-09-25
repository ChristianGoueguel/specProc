# Biweight Location

This function computes the biweight location, a robust measure of
central tendency for a numeric vector. The biweight location is less
sensitive to outliers than the sample mean.

## Usage

``` r
biweight_location(
  x,
  loc = NULL,
  c = 6,
  tol = 1e-06,
  max_iter = 50,
  drop.na = FALSE
)
```

## Arguments

- x:

  A numeric vector.

- loc:

  Initial guess for the location (default: median of `x`).

- c:

  A numeric value specifying the tuning constant for the biweight
  estimator (`c = 6` by default).

- tol:

  Convergence tolerance for the iterative computation (default: 1e-6).

- max_iter:

  Maximum number of iterations (default: 50).

- drop.na:

  A logical value indicating whether to remove missing values (`NA`)
  from the calculations. If `TRUE`, missing values will be removed. If
  `FALSE` (the default), the result is `NA` when `x` contains missing
  values.

## Value

The biweight location of `x`.

## Details

Starting from the initial location \\M\\ (the median by default), the
biweight location is \$\$\zeta = M + \frac{\sum\_{\|u_i\|\<1} (x_i -
M)(1 - u_i^2)^2}{\sum\_{\|u_i\|\<1} (1 - u_i^2)^2}, \quad u_i =
\frac{x_i - M}{c \cdot \mathrm{MAD}}\$\$ where MAD is the (unscaled)
median absolute deviation. The estimate is iterated, replacing \\M\\ by
\\\zeta\\, until the change is smaller than `tol` or `max_iter`
iterations are reached.

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
# Example 1: Compute biweight location for a vector
x <- c(seq(1,100))
tibble::tibble(
mean = mean(x),
med = stats::median(x),
biloc = biweight_location(x)
)
#> # A tibble: 1 × 3
#>    mean   med biloc
#>   <dbl> <dbl> <dbl>
#> 1  50.5  50.5  50.5

# Example 2: Biweight location is robust to outliers
x <- c(seq(1,99), 1e3)  # An outlier at 1000
tibble::tibble(
mean = mean(x),
med = stats::median(x),
biloc = biweight_location(x)
)
#> # A tibble: 1 × 3
#>    mean   med biloc
#>   <dbl> <dbl> <dbl>
#> 1  59.5  50.5  50.0
```
