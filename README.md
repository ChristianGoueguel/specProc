
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src='man/figures/logo.png' align="right" height="250" />

# specProc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/specProc)](https://CRAN.R-project.org/package=specProc)
<!-- badges: end -->

**Pre-release for feedback and experimenting.**

`specProc` package performs a number of pre-processing tasks commonly
used in laser-induced breakdown spectroscopy (LIBS). Collectively, these
are essential tools in LIBS calibration modeling. These include:

-   Baseline removal
-   Fitting single, multiple or overlapping peaks
-   Spectral-based normalization
-   Robust Box-Cox and Yeo-Johnson transformation
-   Pearson or Spearman peaks correlation
-   Descriptive statistics such as mean, standard deviation and higher
    central moments

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ChristianGoueguel/specProc")
```

## Exemples

Loading `specProc` package.

``` r
library(specProc)
```

### LIBS spectrum in the 375–510 nm wavelength range

The plot below shows a typical LIBS spectrum. Prominent atomic and ionic
emission lines of Ca, Mn and Ba were identified using the [NIST spectral
lines database](https://www.nist.gov/pml/atomic-spectra-database). The
spectrum show emission lines of Ca II 393.37 nm, Ca II 396.85 nm and Ca
I 422.67 nm, unresolved Mn triplet at 403.08, 403.31 and 403.45 nm, and
Ba ionic lines at 455.40 and 493.41 nm.

<img src="man/figures/README-unnamed-chunk-7-1.png" width="90%" height="90%" />

### Baseline removal

When analyzing LIBS spectra, it is often more effective to subtract an
estimated baseline from the data. The estimate is constructed by fitting
a low-order polynomial function to the spectrum baseline. Then the
resulting curve fit is subtracted from the data. Here we use the
function `baselinerm` to perform such a task.

``` r
baseline_fit <- Ca_Mn_spec %>%
  select(`390.03027`:`500.03397`) %>%
  baselinerm(degree = 7)
```

As one can see, the `baselinerm` function provides a list containing two
data frames, one for the baseline-subtracted spectrum or spectra `spec`
and the other for the fitted baseline `bkg`.

``` r
str(baseline_fit, list.len = 5) 
#> List of 2
#>  $ spec: tibble [1 x 808] (S3: tbl_df/tbl/data.frame)
#>   ..$ 390.03027: num 131
#>   ..$ 390.1666 : num 10.5
#>   ..$ 390.30292: num 0
#>   ..$ 390.43921: num 129
#>   ..$ 390.57553: num 197
#>   .. [list output truncated]
#>  $ bkg : tibble [1 x 808] (S3: tbl_df/tbl/data.frame)
#>   ..$ 390.03027: num 13831
#>   ..$ 390.1666 : num 13864
#>   ..$ 390.30292: num 13897
#>   ..$ 390.43921: num 13927
#>   ..$ 390.57553: num 13957
#>   .. [list output truncated]
```

We can then extract each data frame from the list using the
`purrr::pluck` function.

``` r
background <- baseline_fit %>% pluck("bkg")
corrected_spec <- baseline_fit %>% pluck("spec")
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="90%" height="90%" />

### Peaks fitting

Fitting of laser-induced breakdown spectroscopy (LIBS) spectral lines is
very important for accurate quantitative analysis. As such, the
Gaussian, Lorentzian and Voigt profile functions are often used to fit
these spectral lines. The idea of curve fitting is to find a
mathematical model that fits the spectral lines. It is assumed that you
have theoretical or experimental reasons for picking a function of a
certain profile. The `peakfit` function finds the specific parameters
which make that function match your data as closely as possible.

`peakfit` is based on the `minpack.lm::nlsLM` function that uses the
Levenberg-Marquardt algorithm for searching the minimum value of the
square of the sum of the residuals. The search process involves starting
with an initial guess at the parameters values.

Let’s start with the fit of a single peak first. Here we want to fit
separately the Ba II line at 455.40 nm, then the Ca I line at 422.67 nm,
using the Gaussian profile `profile = "Gaussian"`.The Gaussian profile
was chosen because we expect to have a relatively large spectral
linewidth due to the measurement conditions. Moreover, `wG` and `A` are
our initial guess and are respectively the Gaussian full width at half
maximum (FWHM) and peak area.

``` r
Ba455_fit <- corrected_spec %>% 
  peakfit(profile = "Gaussian", wG = 1, A =  15000, wlgth.min = 454.09, wlgth.max = 457.09)
```

``` r
Ca422_fit <- corrected_spec %>% 
  peakfit(profile = "Gaussian", wG = 1, A = 15000, wlgth.min = 421.65, wlgth.max = 424.30)
```

The `peakfit` function returns a list containing data stored in
`augmented` and in `tidied`. For instance, using results from the Ba
line fitting, we can see that `augmented` contains the original data `x`
(wavelength) and `y` (intensity) and the fitted results `.fitted` along
with the corresponding residuals `.resid`.

``` r
Ba455_fit %>% pluck("augmented")
#> [[1]]
#> # A tibble: 22 x 4
#>        x      y .fitted .resid
#>    <dbl>  <dbl>   <dbl>  <dbl>
#>  1  454.  3086.   3630. -544. 
#>  2  454.  3408.   3800. -392. 
#>  3  454.  3763.   4190. -426. 
#>  4  455.  4771.   4988. -217. 
#>  5  455.  6277.   6460. -183. 
#>  6  455.  8665.   8881. -215. 
#>  7  455. 12146.  12413. -267. 
#>  8  455. 16921.  16940.  -19.7
#>  9  455. 22226.  21939.  287. 
#> 10  455. 26762.  26507.  256. 
#> # ... with 12 more rows
```

While `tidied` contains the estimated parameters.

``` r
Ba455_fit %>% pluck("tidied")
#> [[1]]
#> # A tibble: 4 x 5
#>   term   estimate std.error statistic  p.value
#>   <chr>     <dbl>     <dbl>     <dbl>    <dbl>
#> 1 y0     3528.    161.           22.0 1.88e-14
#> 2 xc      456.      0.00346  131523.  2.65e-82
#> 3 wG        0.884   0.0101       87.7 3.80e-25
#> 4 A     29869.    426.           70.1 2.15e-23
```

Thus we can plot the obtained results, since a picture is worth a
thousand words.

<img src="man/figures/README-unnamed-chunk-20-1.png" width="90%" height="90%" />

In fact, we should rather look at the Voigt profile, especially if we
want to have an accurate measurement of the FWHM of the emission lines.
In this case, the `wL` estimate provides the FWHM of the spectral lines
while the `wG` estimate provides the instrumental broadening.

``` r
Ba455_fit2 <- corrected_spec %>% 
  peakfit(profile = "Voigt", wL = 0.1, wG = 0.9, A =  15000, wlgth.min = 454.09, wlgth.max = 457.09)
```

``` r
Ca422_fit2 <- corrected_spec %>% 
  peakfit(profile = "Voigt", wL = 0.1, wG = 0.9, A = 15000, wlgth.min = 421.65, wlgth.max = 424.30)
```

We can see below that, as expected, Gaussian broadening accounts for
most of the observed broadening of the 455.50 nm Ba emission line.

``` r
Ba455_fit2 %>% pluck("tidied")
#> [[1]]
#> # A tibble: 5 x 5
#>   term   estimate  std.error statistic  p.value
#>   <chr>     <dbl>      <dbl>     <dbl>    <dbl>
#> 1 y0     2714.     755.           3.60 2.23e- 3
#> 2 xc      456.       0.00343 132633.   4.51e-78
#> 3 wL        0.103    0.0897       1.15 2.65e- 1
#> 4 wG        0.400    0.0387      10.3  9.64e- 9
#> 5 A     33924.    3776.           8.98 7.26e- 8
```

<img src="man/figures/README-unnamed-chunk-26-1.png" width="90%" height="90%" />

On the other hand, it may sometimes be more advisable to fit multiple
peaks at the same time. Here, the `multipeakfit` function is used.
