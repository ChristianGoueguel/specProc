# Peaks of the Loadings of a PCA

Finds the wavelengths that contribute most to the components of a PCA:
the largest positive and negative local extrema of the loadings, or the
largest maxima of the variance explained by wavelength, optionally
matched to emission lines.
[`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md)
labels the same peaks.

## Usage

``` r
loading_peaks(
  model,
  components = NULL,
  type = c("loadings", "contribution"),
  top = 10,
  lines = NULL,
  tol = 0.1,
  span = 5
)
```

## Arguments

- model:

  A [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit or an
  object returned by
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  or
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md).
  The variable names must be the wavelengths; otherwise, the variables
  are numbered.

- components:

  The components to show (numbers). Default is the first three (or all
  of them, if fewer).

- type:

  `"loadings"` (default) for one panel of loadings per component, or
  `"contribution"` for the variance of each wavelength explained by
  `components`.

- top:

  The number of peaks labeled per component (per panel). Default is 10;
  use 0 for no labels.

- lines:

  Optional line list returned by
  [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md),
  to label the peaks with the emission lines they match.

- tol:

  The largest distance, in nm, between a peak and a line it matches.
  Default is 0.1.

- span:

  The half-width, in channels, of the window in which a peak is a local
  extremum. Default is 5.

## Value

A tibble with one row per peak, sorted by component and by decreasing
absolute value, and columns `component`, `variable` (the column name),
`wavelength`, `value` (the loading, or the explained variance), `sign`
(`"positive"` or `"negative"`) and, for loadings, `derivative`. With
`lines`, it also has the columns `species` and `line_wavelength` of the
matched line (`NA` when none is within `tol`) and `candidates`, all the
lines within `tol`, from the nearest.

## Details

See
[`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md)
for the definitions of the peaks, of the explained variance, of the
orientation of the components and of the line matching. A peak is
flagged as part of a derivative shape (`derivative = TRUE`) when a local
extremum of the opposite sign, at least half as large, lies within
`2 * span` channels on the same detector segment: this usually means a
shift or a broadening of the line between spectra.

## See also

[`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md),
[`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md)

## Author

Christian L. Goueguel

## Examples

``` r
minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
spectra <- dplyr::select(forageLIBS, -Measurement, -Sample, -dplyr::all_of(minerals))
set.seed(1)
fit <- robpca(center(spectra))
loading_peaks(fit, top = 5)
#> # A tibble: 15 × 6
#>    component variable    wavelength  value sign     derivative
#>    <chr>     <chr>            <dbl>  <dbl> <chr>    <lgl>     
#>  1 PC1       399.2983211       399.  0.223 positive FALSE     
#>  2 PC1       280.2738304       280.  0.215 positive FALSE     
#>  3 PC1       387.525507        388.  0.213 positive FALSE     
#>  4 PC1       588.9148516       589.  0.212 positive FALSE     
#>  5 PC1       393.1932396       393.  0.191 positive FALSE     
#>  6 PC2       588.9148516       589.  0.465 positive FALSE     
#>  7 PC2       589.5059874       590.  0.437 positive FALSE     
#>  8 PC2       602.3070443       602.  0.213 positive FALSE     
#>  9 PC2       602.8928422       603.  0.147 positive FALSE     
#> 10 PC2       769.7510958       770. -0.102 negative FALSE     
#> 11 PC3       769.7510958       770.  0.275 positive FALSE     
#> 12 PC3       393.1932396       393. -0.211 negative FALSE     
#> 13 PC3       390.9190545       391. -0.199 negative FALSE     
#> 14 PC3       766.220248        766.  0.196 positive FALSE     
#> 15 PC3       396.6935378       397. -0.174 negative FALSE     
```
