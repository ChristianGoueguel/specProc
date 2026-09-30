# Flagged Regions of a MacroPCA Fit

Lists the wavelength regions where many observations have cells flagged
by
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md):
runs of adjacent channels (on the same detector segment) whose share of
flagged observations is at least `threshold`. These are the channels
that persistently deviate from the PCA fit, for example emission lines
affected by self-absorption, saturation or matrix effects, and
candidates to exclude or down-weight.
[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)
labels the regions with the largest share.

## Usage

``` r
flagged_regions(
  object,
  threshold = 0.1,
  rows = NULL,
  columns = NULL,
  lines = NULL,
  tol = 0.1
)
```

## Arguments

- object:

  An object returned by
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md).

- threshold:

  The share of flagged observations above which channels form a flagged
  region. Default is 0.1.

- rows, columns:

  Optional indices or names of the rows and columns to show. Default is
  all.

- lines:

  Optional line list returned by
  [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md),
  to label the regions with the emission lines they match.

- tol:

  The largest distance, in nm, between a region and a line it matches.
  Default is 0.1.

## Value

A tibble with one row per region, sorted by decreasing share, and
columns `start`, `end` and `peak` (the wavelength, or variable number,
of the largest share), `channels` (the number of channels), `share` (the
largest share of flagged observations), `mean_share` and `direction`.
With `lines`, it also has the columns `species` and `line_wavelength` of
the nearest line (`NA` when none) and `candidates`.

## Details

Runs separated by at most two channels below the threshold are merged. A
region is `"higher"` when at least two thirds of its flagged cells are
higher than the fit, `"lower"` when at most one third are, and `"mixed"`
otherwise. With `lines`, the lines between `start - tol` and `end + tol`
are the candidates, from the nearest to the peak.

## See also

[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md),
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
[`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md)

## Author

Christian L. Goueguel

## Examples

``` r
# spectra of two emitters, 0.1 nm apart, with a region (400.7-400.8 nm)
# that deviates in 15 of the 60 spectra
set.seed(21)
wl <- seq(400, by = 0.1, length.out = 20)
line <- function(center) exp(-(wl - center)^2 / 0.02)
x <- outer(rnorm(60, 10, 2), 50 * line(400.3) + 5) +
  outer(rnorm(60, 5, 1), 30 * line(401.5) + 3) +
  matrix(rnorm(60 * 20, sd = 0.5), 60)
colnames(x) <- wl
x[1:15, 8:9] <- x[1:15, 8:9] + 20
fit <- macropca(x, k = 2)
flagged_regions(fit, threshold = 0.2)
#> # A tibble: 1 × 7
#>   start   end  peak channels share mean_share direction
#>   <dbl> <dbl> <dbl>    <int> <dbl>      <dbl> <chr>    
#> 1  401.  401.  401.        2  0.25       0.25 higher   
```
