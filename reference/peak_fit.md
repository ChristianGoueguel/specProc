# Peak Fitting

Fitting of a single spectral line by lineshape functions with variable
parameters.

## Usage

``` r
peak_fit(
  x,
  profile = "voigt",
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
  are the wavelengths (e.g. `"396.15"`); an optional identifier column
  can be given with `id`.

- profile:

  A character specifying the lineshape function to be used:
  "lorentzian", "gaussian" or "voigt" (pseudo-Voigt).

- wL:

  A numeric specifying the Lorentzian full width at half maximum
  (initial guess)

- wG:

  A numeric specifying the Gaussian full width at half maximum (initial
  guess)

- A:

  A numeric specifying the peak area (initial guess)

- wlgth.min:

  A numeric specifying the lower bound of the wavelength subset

- wlgth.max:

  A numeric specifying the upper bound of the wavelength subset

- id:

  A character specifying the name of the column holding the spectra id
  (optional)

- max.iter:

  A numeric specifying the maximum number of iteration (200 by default)

## Value

A tibble with one row per spectrum and the columns:

- `id` column (or `spectrum`, the row number, when `id = NULL`),

- `data`: the fitted data (`x`, `y`),

- `fit`: the `nls` model object,

- `tidied`: the estimated parameters with their standard errors,

- `augmented`: the data with fitted values (`.fitted`) and residuals
  (`.resid`).

## Details

The function uses
[`minpack.lm::nlsLM`](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html),
which is based on the Levenberg-Marquardt algorithm for searching the
minimum value of the square of the sum of the residuals. Each spectrum
(row of `x`) is fitted separately with the model \$\$y = y_0 + A \cdot
f(x; x_c, w)\$\$ where \\f\\ is a unit-area Gaussian, Lorentzian or
pseudo-Voigt profile (see
[`gaussian_profile()`](https://christiangoueguel.com/specProc/reference/gaussian_profile.md),
[`lorentzian_profile()`](https://christiangoueguel.com/specProc/reference/lorentzian_profile.md)
and
[`pseudo_voigt_profile()`](https://christiangoueguel.com/specProc/reference/pseudo_voigt_profile.md)).
The fitted parameters are `y0`, `xc`, `A` and the width(s) `wG` and/or
`wL`.

Initial values that are not supplied are estimated from the data: the
peak center from the position of the maximum, the width from the full
width at half maximum, and the area from the integrated intensity above
the minimum. The center is constrained to the fitted wavelength range
and the widths and area to positive values. If the fit of a spectrum
fails, a warning is issued and the corresponding `fit`, `tidied` and
`augmented` entries are `NULL`.

## Author

Christian L. Goueguel

## Examples

``` r
wl <- seq(395, 397, by = 0.02)
set.seed(1)
spec <- 10 + gaussian_profile(wl, y0 = 0, xc = 396.15, wG = 0.2, A = 50) + rnorm(length(wl))
df <- as.data.frame(t(spec))
names(df) <- wl
res <- peak_fit(df, profile = "gaussian")
res$tidied[[1]]
#> # A tibble: 4 × 5
#>   term  estimate std.error statistic   p.value
#>   <chr>    <dbl>     <dbl>     <dbl>     <dbl>
#> 1 y0      10.1    0.102         98.4 5.32e- 99
#> 2 xc     396.     0.000169 2350805.  0        
#> 3 wG       0.200  0.000415     481.  1.14e-165
#> 4 A       50.1    0.0977       513.  2.49e-168
```
