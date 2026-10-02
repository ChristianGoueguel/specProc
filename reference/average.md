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
complexity of *O(n x m)*, where *n* is the number of rows and *m* is the
number of columns in the data. Missing values are ignored in the
computation of each mean.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# one mean spectrum per sample: three samples were measured twice
means <- average(forageLIBS[-c(1, 3:14)], Sample)
dim(means)
#> [1]  365 7153
```
