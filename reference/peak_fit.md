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
  max.iter = 200,
  fixed = NULL
)
```

## Arguments

- x:

  A data frame or tibble of spectra, one spectrum per row. Column names
  are the wavelengths (e.g. `"396.15"`); an optional identifier column
  can be given with `id`.

- profile:

  A character specifying the lineshape function to be used:
  "lorentzian", "gaussian", "voigt" (exact) or "pseudo_voigt".

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

- fixed:

  The widths held at their given values (`wG`, `wL`) instead of being
  fitted: `"wG"`, `"wL"` or both. Default is `NULL` (all widths fitted).

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
f(x; x_c, w)\$\$ where \\f\\ is a unit-area Gaussian, Lorentzian, Voigt
or pseudo-Voigt profile (see
[`gaussian_profile()`](https://christiangoueguel.com/specProc/reference/gaussian_profile.md),
[`lorentzian_profile()`](https://christiangoueguel.com/specProc/reference/lorentzian_profile.md),
[`voigt_profile()`](https://christiangoueguel.com/specProc/reference/voigt_profile.md)
and
[`pseudo_voigt_profile()`](https://christiangoueguel.com/specProc/reference/pseudo_voigt_profile.md)).
The fitted parameters are `y0`, `xc`, `A` and the width(s) `wG` and/or
`wL` (full widths at half maximum). The Voigt profile is evaluated
exactly in C++; the pseudo-Voigt (Thompson-Cox-Hastings) approximation
is faster but accurate to about 1\\

Initial values that are not supplied are estimated from the data: the
peak center from the position of the maximum, the width from the full
width at half maximum, and the area from the integrated intensity above
the minimum. The center is constrained to the fitted wavelength range
and the widths and area to positive values. If the fit of a spectrum
fails, a warning is issued and the corresponding `fit`, `tidied` and
`augmented` entries are `NULL`.

The Gaussian and Lorentzian widths of a Voigt profile are strongly
correlated, so a free fit determines the Lorentzian (Stark) width poorly
when the line is narrow. In a LIBS plasma, the Gaussian width is the
instrumental width, combined with the Doppler width in quadrature, \\w_G
= \sqrt{w\_{inst}^2 + w_D^2}\\, and can be held fixed with
`fixed = "wG"` and the value of `wG`. The instrumental width is the
width of a line of a low-pressure lamp (mercury, argon), whose Stark and
Doppler widths are negligible, fitted with a Gaussian profile at a
nearby wavelength; the Doppler width is given by
[`doppler_width()`](https://christiangoueguel.com/specProc/reference/doppler_width.md).
A fixed width is listed in `tidied` without standard error.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
mean_spectrum <- colMeans(forageLIBS[-(1:14)])
wl <- as.numeric(names(mean_spectrum))
# the H-alpha line of the hydrogen of the samples, Stark-broadened
halpha <- tibble::as_tibble(as.list(mean_spectrum[wl > 653.5 & wl < 659.5]))
res <- peak_fit(halpha, profile = "lorentzian")
res$tidied[[1]]
#> # A tibble: 4 × 5
#>   term  estimate std.error statistic   p.value
#>   <chr>    <dbl>     <dbl>     <dbl>     <dbl>
#> 1 y0     2298.    96.1          23.9 1.54e- 34
#> 2 xc      656.     0.00935   70231.  2.78e-265
#> 3 wL        1.70   0.0459       37.1 2.27e- 46
#> 4 A     26000.   773.           33.6 1.12e- 43

# a Voigt line whose Gaussian width (instrument and Doppler) is known
x <- seq(395, 397, by = 0.01)
set.seed(1)
y <- voigt_profile(x, y0 = 10, xc = 396, wG = 0.08, wL = 0.05, A = 50) +
  rnorm(length(x), sd = 2)
line <- tibble::as_tibble(as.list(stats::setNames(y, x)))
peak_fit(line, profile = "voigt", wG = 0.08, fixed = "wG")$tidied[[1]]
#> # A tibble: 5 × 5
#>   term  estimate std.error statistic    p.value
#>   <chr>    <dbl>     <dbl>     <dbl>      <dbl>
#> 1 y0      9.95    0.150         66.4  7.66e-137
#> 2 xc    396.      0.000127 3123171.   0        
#> 3 wG      0.08   NA             NA   NA        
#> 4 wL      0.0511  0.000425     120.   2.93e-186
#> 5 A      50.3     0.159        315.   2.52e-268
```
