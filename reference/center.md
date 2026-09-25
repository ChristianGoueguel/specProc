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
m <- matrix(c(1, 2, 3, 10, 20, 30), ncol = 2)
center(m)
#>      [,1] [,2]
#> [1,]   -1  -10
#> [2,]    0    0
#> [3,]    1   10
#> attr(,"center")
#> [1]  2 20
center(m, method = "median")
#>      [,1] [,2]
#> [1,]   -1  -10
#> [2,]    0    0
#> [3,]    1   10
#> attr(,"center")
#> [1]  2 20
```
