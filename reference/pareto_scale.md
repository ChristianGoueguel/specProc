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
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]  # the spectral channels
pareto_scale(spectra)[1:3, 1:4]
#> # A tibble: 3 × 4
#>   `199.3771616` `199.4644141` `199.5516666` `199.6389192`
#>           <dbl>         <dbl>         <dbl>         <dbl>
#> 1          221.          215.          133.          214.
#> 2          221.          218.          132.          209.
#> 3          219.          215.          133.          214.
```
