# Standard Normal Variate

This function performs Standard Normal Variate (SNV) scaling on the
input spectral data. SNV scaling scales each row of the input data to
have zero mean and unit standard deviation. This is equivalent to
autoscaling the transpose of the input data.

## Usage

``` r
snv(x, drop.na = TRUE)
```

## Arguments

- x:

  A numeric matrix or data frame.

- drop.na:

  A logical value indicating whether to remove spectra (rows) containing
  missing values. If `TRUE` (the default), such rows are removed.

## Value

A list with the following components:

- `correction`:

  The SNV-scaled data.

- `means`:

  A vector of row means.

- `stds`:

  A vector of row standard deviations.

## Author

Christian L. Goueguel

## Examples

``` r
x <- rbind(c(1, 2, 3, 4), c(10, 20, 30, 40))
snv(x)$correction
#> # A tibble: 2 × 4
#>      V1     V2    V3    V4
#>   <dbl>  <dbl> <dbl> <dbl>
#> 1 -1.16 -0.387 0.387  1.16
#> 2 -1.16 -0.387 0.387  1.16
```
