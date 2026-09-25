# Min-Max Normalization

This function rescales a numeric vector using the min-max normalization
technique, which linearly transforms the values to a new range specified
by the `a` and `b` arguments. The minimum value in the original vector
is mapped to `a`, and the maximum value is mapped to `b`.

## Usage

``` r
minmax(x, a = 0, b = 1, drop.na = TRUE)
```

## Arguments

- x:

  A numeric vector.

- a:

  The minimum value of the new range (default: 0).

- b:

  The maximum value of the new range (default: 1).

- drop.na:

  A logical value indicating whether to remove missing values (NA). If
  `TRUE` (the default), missing values are removed from the output. If
  `FALSE`, they are kept (as `NA`) and ignored when computing the range.

## Value

A numeric vector with values rescaled to the new range `[a, b]`.

## Author

Christian L. Goueguel

## Examples

``` r
minmax(c(2, 4, 6, 10))
#> [1] 0.00 0.25 0.50 1.00
minmax(c(2, 4, NA, 10), a = -1, b = 1, drop.na = FALSE)
#> [1] -1.0 -0.5   NA  1.0
```
