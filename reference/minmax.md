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
data(forageLIBS)
head(minmax(forageLIBS$K))
#> [1] 0.8983912 0.5709850 0.5512278 0.5088908 0.6697714 0.4693762
head(minmax(forageLIBS$S, a = -1, b = 1, drop.na = FALSE))
#> [1] -0.3333333 -0.4814815 -0.3333333 -0.3333333 -0.7037037 -0.4074074
```
