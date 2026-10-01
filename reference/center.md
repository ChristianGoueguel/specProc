# Data Centering

Function to perform mean-centering or median-centering on a numeric
matrix or data frame.

## Usage

``` r
center(x, method = "mean", drop.na = FALSE)
```

## Arguments

- x:

  A numeric matrix or data frame.

- method:

  A character string specifying the centering method, either "mean" or
  "median".

- drop.na:

  A logical value indicating whether to ignore missing values (NA) when
  computing the column means or medians. Missing values are kept in the
  output.

## Value

A numeric matrix with the same dimensions as the input, with columns
centered according to the specified method. The column centers are
stored in the `"center"` attribute.

## Details

Mean-centering calculates the mean of each column and subtracts this
from the column. Median-centering is very similar to mean-centering
except that the reference point is the median of each column rather than
the mean.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]  # the spectral channels
center(spectra)[1:3, 1:4]
#>      199.3771616 199.4644141 199.5516666 199.6389192
#> [1,]   0.6929348    5.831522    22.22826   0.4918478
#> [2,]   0.6929348   16.831522    14.22826 -16.5081522
#> [3,]  -6.3070652    6.831522    22.22826   2.4918478
center(spectra, method = "median")[1:3, 1:4]
#>      199.3771616 199.4644141 199.5516666 199.6389192
#> [1,]           1           7           6           1
#> [2,]           1          18          -2         -16
#> [3,]          -6           8           6           3
```
