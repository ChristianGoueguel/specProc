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
  peak or a single one used for all peaks: "lorentzian", "gaussian" or
  "voigt" (case insensitive).

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
wl <- seq(395, 397, by = 0.02)
set.seed(1)
spec <- 5 + gaussian_profile(wl, 0, 395.8, 0.15, 20) + lorentzian_profile(wl, 0, 396.3, 0.2, 30) +
  rnorm(length(wl), sd = 0.5)
df <- as.data.frame(t(spec))
names(df) <- wl
res <- multipeak_fit(df, peaks = c(395.8, 396.3), profiles = c("gaussian", "lorentzian"))
res$tidied[[1]]
#> # A tibble: 7 × 5
#>   term  estimate std.error statistic   p.value
#>   <chr>    <dbl>     <dbl>     <dbl>     <dbl>
#> 1 y0       5.02   0.0686        73.2 1.15e- 84
#> 2 xc_1   396.     0.000138 2867987.  0        
#> 3 wG_1     0.150  0.000337     443.  6.85e-158
#> 4 A_1     20.0    0.0420       476.  9.40e-161
#> 5 xc_2   396.     0.000242 1637180.  0        
#> 6 wL_2     0.201  0.000808     249.  2.84e-134
#> 7 A_2     30.1    0.101        297.  1.57e-141
```
