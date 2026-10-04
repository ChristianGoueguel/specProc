# Stability of a Self-Organizing Map

Fits a self-organizing map with
[`som()`](https://christiangoueguel.com/specProc/reference/som.md) on
resampled data several times, maps all the spectra on each map, and
measures how consistently the spectra keep their neighbors.

## Usage

``` r
som_stability(x, runs = 10, ...)
```

## Arguments

- x:

  A numeric matrix or data frame of spectra, one per row.

- runs:

  The number of resampled fits. Default is 10.

- ...:

  Further arguments passed to
  [`som()`](https://christiangoueguel.com/specProc/reference/som.md),
  such as `grid` or `robust`.

## Value

A list with `runs`, a tibble with the quantization and topographic
errors of each run, `stability`, the mean correlation between the runs,
and `correlations`, the matrix of the correlations between runs.

## Details

Each run fits the map on a bootstrap sample of the spectra (with the
same grid), and maps all of them. For each run, the grid distances
between the units of every pair of spectra form a matrix; the stability
is the mean correlation between these matrices over the pairs of runs.
Values close to 1 mean that the same spectra are neighbors on every map,
whatever the sample: the structure of the map is not an artifact of the
training data. Low values suggest a smaller grid, a larger final radius
or more epochs.

## See also

[`som()`](https://christiangoueguel.com/specProc/reference/som.md)

## Examples

``` r
# \donttest{
data(forageLIBS)
# the 380-430 nm window (Ca II H and K lines), faster than all the channels
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 380 & wl < 430)]
set.seed(1)
som_stability(spectra, runs = 5)$stability
#> [1] 0.9546112
# }
```
