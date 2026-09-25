# Pareto Scaling

This function performs Pareto scaling on a numeric matrix or data frame.
Pareto scaling scales each variable (column) by dividing it by the
square root of its standard deviation.

## Usage

``` r
pareto_scale(x, drop.na = FALSE)
```

## Arguments

- x:

  A numeric matrix or data frame to be scaled.

- drop.na:

  A logical value indicating whether to ignore missing values (NA) when
  computing the standard deviations. Default is `FALSE`.

## Value

A numeric matrix (or a tibble if `x` is a data frame) with the same
dimensions as `x`, but with each variable scaled by the square root of
its standard deviation.

## Author

Christian L. Goueguel

## Examples

``` r
pareto_scale(matrix(c(1, 2, 3, 10, 20, 30), ncol = 2))
#>      [,1]     [,2]
#> [1,]    1 3.162278
#> [2,]    2 6.324555
#> [3,]    3 9.486833
```
