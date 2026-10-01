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
  max.iter = 200
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
```
