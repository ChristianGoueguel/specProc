# Multiple Peaks Fitting

Fitting of multiple spectral lines by the same or different lineshape
functions with variable parameters.

## Usage

``` r
multipeak_fit(
  x,
  peaks,
  profiles,
  wL = NULL,
  wG = NULL,
  A = NULL,
  wlgth.min = NULL,
  wlgth.max = NULL,
  id = NULL,
  max.iter = 200,
  fixed = NULL,
  lock_centers = FALSE,
  share_widths = FALSE,
  area_ratios = NULL
)
```

## Arguments

- x:

  A data frame or tibble of spectra, one spectrum per row. Column names
  are the wavelengths; an optional identifier column can be given with
  `id`.

- peaks:

  A numeric vector of the (approximate) peak center wavelengths.

- profiles:

  A character vector of the lineshape functions for fitting, one per
  peak or a single one used for all peaks: "lorentzian", "gaussian",
  "voigt" (exact) or "pseudo_voigt" (case insensitive).

- wL:

  A numeric (single value or one per peak) of the Lorentzian full width
  at half maximum (initial guess)

- wG:

  A numeric (single value or one per peak) of the Gaussian full width at
  half maximum (initial guess)

- A:

  A numeric (single value or one per peak) of the peak area (initial
  guess)

- wlgth.min:

  A numeric of the lower bound of the wavelength subset

- wlgth.max:

  A numeric of the upper bound of the wavelength subset

- id:

  A character specifying the name of the column holding the spectra id
  (optional)

- max.iter:

  A numeric specifying the maximum number of iteration (200 by default)

- fixed:

  The widths held at their given values (`wG`, `wL`, one value or one
  per peak) instead of being fitted: `"wG"`, `"wL"` or both, for the
  peaks whose profile has that width. Default is `NULL` (all widths
  fitted). See
  [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)
  for holding the Gaussian width at the instrumental and Doppler width.

- lock_centers:

  Keep the separations of `peaks` (see details): `FALSE` (default),
  `TRUE`, a logical vector or group labels, one per peak.

- share_widths:

  Share the widths of the lines (see details): `FALSE` (default),
  `TRUE`, a logical vector or group labels, one per peak. The lines of a
  group must have the same profile.

- area_ratios:

  The fixed relative areas of the lines, one per peak (`NA` for a free
  area), or `NULL` (default) for free areas.

## Value

A tibble with one row per spectrum and the columns `id` (or `spectrum`),
`data`, `fit`, `tidied` and `augmented` (see
[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)).
`augmented` additionally contains one column per peak (`.peak_1`, ...)
with the contribution of each fitted line.

## Details

The function uses
[`minpack.lm::nlsLM`](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html),
which is based on the Levenberg-Marquardt algorithm for searching the
minimum value of the square of the sum of the residuals. All peaks are
fitted simultaneously with a common baseline offset: \$\$y = y_0 +
\sum\_{i} A_i \cdot f_i(x; x\_{c,i}, w_i)\$\$ The parameters of peak
\\i\\ are named with the suffix `_i` (e.g. `xc_1`, `wG_1`, `A_1`).
Initial values that are not supplied are estimated from the data around
each peak center. Each peak center is constrained to lie within the
fitted wavelength range.

The lines of a multiplet, or of a fine-structure doublet, are related,
and constraints make the fit of overlapping lines much more stable:

- `lock_centers`: the lines keep the separations of their `peaks`
  (accurate wavelengths, such as those of the NIST database) and move
  together by one fitted shift, which also absorbs an offset of the
  wavelength axis. The shift is listed in `tidied` as `shift` (or
  `shift_1_2`, ..., named by its lines, when there are several groups).

- `share_widths`: the lines share their Gaussian and Lorentzian widths,
  as lines of the same multiplet have nearly the same Stark width.

- `area_ratios`: the areas of the lines are in fixed ratios, with one
  fitted area. In an optically thin plasma in LTE, the ratio of two
  lines is \\g_k A\_{ki} \lambda^{-1} \exp(-E_k / k_B T)\\ (energy
  units), independent of the temperature for lines from the same upper
  level: the `relative_intensity` of
  [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md).
  Self-absorbed lines, such as resonance lines, break these ratios.

`lock_centers` and `share_widths` take `TRUE` (all lines), a logical
vector (the lines constrained together) or one group label per line
(`NA` for a line left free), so that, for example, a multiplet can be
constrained while an interfering line of another element is not.
`area_ratios` gives a ratio per line, `NA` for a line with a free area.

`tidied` lists the center, widths and area of every line (named with the
suffix `_i`), with standard errors propagated from the fitted
parameters: lines that share a parameter have the same width, or centers
that differ by a constant.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
mean_spectrum <- colMeans(forageLIBS[-(1:14)])
wl <- as.numeric(names(mean_spectrum))
# the overlapping lines of the K I doublet
doublet <- tibble::as_tibble(as.list(mean_spectrum[wl > 404.1 & wl < 405.0]))
res <- multipeak_fit(doublet, peaks = c(404.414, 404.721), profiles = "gaussian")
res$tidied[[1]]
#> # A tibble: 7 × 5
#>   term  estimate std.error statistic  p.value
#>   <chr>    <dbl>     <dbl>     <dbl>    <dbl>
#> 1 y0    1597.     92.5         17.3  6.60e- 5
#> 2 xc_1   404.      0.00896  45147.   1.44e-18
#> 3 wG_1     0.115   0.0144       7.99 1.33e- 3
#> 4 A_1    215.     35.3          6.08 3.69e- 3
#> 5 xc_2   405.      0.0126   32038.   5.70e-18
#> 6 wG_2     0.170   0.0368       4.61 9.99e- 3
#> 7 A_2    208.     47.2          4.41 1.16e- 2

# the same doublet with the NIST separation of its lines and a common width
res <- multipeak_fit(doublet, peaks = c(404.414, 404.721), profiles = "gaussian",
                     lock_centers = TRUE, share_widths = TRUE)
res$tidied[[1]]
#> # A tibble: 8 × 5
#>   term   estimate std.error statistic  p.value
#>   <chr>     <dbl>     <dbl>     <dbl>    <dbl>
#> 1 y0    1667.      82.4         20.2  9.49e- 7
#> 2 xc_1   404.       0.00637  63486.   1.03e-27
#> 3 wG_1     0.111    0.0168       6.62 5.72e- 4
#> 4 A_1    200.      36.4          5.50 1.51e- 3
#> 5 xc_2   405.       0.00637  63534.   1.03e-27
#> 6 wG_2     0.111    0.0168       6.62 5.72e- 4
#> 7 A_2    159.      29.0          5.46 1.57e- 3
#> 8 shift   -0.0593   0.00637     -9.31 8.72e- 5
```
