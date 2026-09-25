# Fast Average for Large Spectral Dataset

This function efficiently averages the samples spectra in a large
dataset. It provides flexibility by computing either the overall mean
across all spectra or group-wise means based on the values of a
specified grouping column.

## Usage

``` r
average(x, .group_by = NULL)
```

## Arguments

- x:

  A data frame or tibble.

- .group_by:

  The column to group the data by (optional), given either unquoted or
  as a string. If not provided, the average of the overall data will be
  computed.

## Value

- If `.group_by = NULL`, a one-row tibble containing the mean of each
  column.

- If `.group_by` is provided, a tibble with one row per group: the first
  column holds the group labels and the remaining columns hold the group
  means.

## Details

The function leverages the power of `Rcpp` to perform the mean
calculations in C++. The underlying C++ implementation has a time
complexity of *O(n × m)*, where *n* is the number of rows and *m* is the
number of columns in the data. Missing values are ignored in the
computation of each mean.

## Author

Christian L. Goueguel

## Examples

``` r
spectra <- data.frame(
  sample = rep(c("a", "b"), each = 3),
  `200.1` = c(1, 2, 3, 10, 11, 12),
  `200.2` = c(2, 3, 4, 20, 21, 22),
  check.names = FALSE
)
average(spectra[, -1])
#> # A tibble: 1 × 2
#>   `200.1` `200.2`
#>     <dbl>   <dbl>
#> 1     6.5      12
average(spectra, sample)
#> # A tibble: 2 × 3
#>   sample `200.1` `200.2`
#>   <chr>    <dbl>   <dbl>
#> 1 a            2       3
#> 2 b           11      21
```
