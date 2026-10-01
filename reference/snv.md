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
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]  # the spectral channels
corrected <- snv(spectra)$correction
corrected[1:3, 1:4]
#> # A tibble: 3 × 4
#>   `199.3771616` `199.4644141` `199.5516666` `199.6389192`
#>           <dbl>         <dbl>         <dbl>         <dbl>
#> 1        -0.386        -0.383        -0.370        -0.383
#> 2        -0.395        -0.389        -0.381        -0.397
#> 3        -0.402        -0.397        -0.384        -0.397
```
