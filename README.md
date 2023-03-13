
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" align="right" height="250"/>

# specProc

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/specProc)](https://CRAN.R-project.org/package=specProc)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

**Pre-release for feedback and experimenting.**

`specProc` package performs a number of pre-processing tasks commonly
used in laser-induced breakdown spectroscopy (LIBS). Collectively, these
are essential tools in LIBS calibration modeling. These include:

-   Baseline removal
-   Fitting single, multiple or overlapping peaks
-   Spectral-based normalization
-   Pearson or Spearman peaks correlation
-   Descriptive statistics such as mean, standard deviation and higher
    central moments
-   NIST interactive plot of LIBS emission lines
-   Orthogonal Signal Correction (OSC)
-   Generalized Least Squares Weighting (GLSW)
-   External Parameter Orthogonalization (EPO)
-   Cellwise & Rowwise outliers detection (CROD)
-   Robust skewness estimation (MC)
-   Robust regression and covariance estimation (LTS, MCD, DetMCD)
-   Robust PCA methods (RAPCA, ROBPCA)
-   Robust PCR method (RPCR)
-   Robust PLS method (RSIMPLS)
-   Robust classification (RDA, RSIMCA)
-   Clustering methods
-   Outlier detection and graphical methods (Skewness-adjusted boxplot, bagplot)
-   Robust Box-Cox and Yeo-Johnson transformation
-   Detecting deviating cells (DDC)


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

``` r
ssh = suppressPackageStartupMessages
ssh(library(tidyverse))
library(patchwork)
```

### LIBS spectrum in the 375–510 nm wavelength range

The plot below shows a typical LIBS spectrum. Prominent atomic and ionic
emission lines of Ca, Mn and Ba were identified using the [NIST spectral
lines database](https://www.nist.gov/pml/atomic-spectra-database). The
spectrum show emission lines of Ca II 393.37 nm, Ca II 396.85 nm and Ca
I 422.67 nm, unresolved Mn triplet at 403.08, 403.31 and 403.45 nm, and
Ba ionic lines at 455.40 and 493.41 nm.

``` r
data("Ca_Mn_spec")
```

``` r
Ca_Mn_spec %>% 
  plotSpec() + 
  theme_classic(base_size = 8) +
  annotate("text", x = 391, y = 4.2e4, angle = 90, size = 2, label = "Ca II 393.37 nm") +
  annotate("text", x = 396, y = 3e4, angle = 90, size = 2, label = "Ca II 396.85 nm") +
  annotate("text", x = 401, y = 3.8e4, angle = 90, size = 2, label = "Mn I 403-nm") +
  annotate("text", x = 420, y = 3.5e4, angle = 90, size = 2, label = "Ca I 422.67 nm") +
  annotate("text", x = 453, y = 3e4, angle = 90, size = 2, label = "Ba II 455.40 nm") +
  annotate("text", x = 491, y = 1.5e4, angle = 90, size = 2, label = "Ba II 493.41 nm")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="90%" height="90%" />

Note that we used the ggplot-based `plotSpec` function to generate the
above plot.

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

And then visualize the spectrum before and after baseline removal.

``` r
Ca_Mn_spec %>% 
  plotSpec() + 
  geom_line(
    data = background %>% 
      pivot_longer(cols = everything(), names_to = "wavelength", values_to = "intensity") %>%
      modify_at("wavelength", as.numeric), 
    aes(x = wavelength, y = intensity), 
    colour = "red"
    ) | 
  corrected_spec %>% 
  plotSpec() + 
  geom_hline(yintercept = 0, colour = "red")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="90%" height="90%" />

### Peak fitting

Fitting of laser-induced breakdown spectroscopy (LIBS) spectral lines is
very important for accurate quantitative analysis. As such, the
Gaussian, Lorentzian and Voigt profile functions are often used to fit
these spectral lines. The idea of curve fitting is to find a
mathematical model that fits the spectral lines. It is assumed that you
have theoretical or experimental reasons for picking a function of a
certain profile. The `peakfit` function finds the specific parameters
which make that function match your data as closely as possible.

The `peakfit` function uses the full width at half maximum (FWHM)
version of the Gaussian function implemented in `gaussian_func` and
given by the following expression:

<p align="center">
<img src="images/GaussianFun.png" width="269"/>
</p>

The Lorentzian function implemented in `lorentzian_func` has a much
wider tails than the Gaussian function and is given by:

<p align="center">
<img src="images/LorentzianFun.png" width="238"/>
</p>

`peakfit` uses the Pseudo-Voigt function, `voigt_func`, which is an
approximation of the Voigt function, defined as the convolution of
Gaussian and Lorentzian function. Thus the Pseudo-Voigt function can be
expressed as:

<p align="center">
<img src="images/VoigtFun.png" width="541"/>
</p>

`peakfit` is based on the `minpack.lm::nlsLM` function that uses the
Levenberg-Marquardt algorithm for searching the minimum value of the
square of the sum of the residuals. The search process involves starting
with an initial guess at the parameters values.

Let’s start with the fit of a single peak first. Here we want to fit
separately the Ba II line at 455.40 nm, then the Ca I line at 422.67 nm,
using the Gaussian profile `profile = "Gaussian"`.The Gaussian profile
was chosen because we expect to have a relatively large spectral
linewidth due to the measurement conditions. Moreover, `wG` and `A` are
our initial guess and are respectively the Gaussian FWHM and peak area.
There are two ways to limit the wavelength range of data for a given
fit. You can set the range of data of interest before using the
`peakfit` function, or you can use the arguments `wlgth.min` and
`wlgth.max` for setting up the wavelength range. Note that you can give
a value to one or both of the arguments.

``` r
Ba455_fit <- corrected_spec %>% 
  peakfit(profile = "Gaussian", wG = 1, A =  15000, wlgth.min = 454.09, wlgth.max = 457.09)
```

``` r
Ca422_fit <- corrected_spec %>% 
  peakfit(profile = "Gaussian", wG = 1, A = 15000, wlgth.min = 421.65, wlgth.max = 424.30)
```

After a successful fit, you can get a variety of results such as the
location of the peak, the height, the area and FWHM for each peak, as
well as the basic fitted coefficients and estimates of errors for each
quantity. The `peakfit` function returns a list containing data stored
in `augmented` and in `tidied`. For instance, using results from the Ba
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

While `tidied` contains the estimated parameters (FWHM, peak height, and
area for fitted peak).

``` r
Ba455_fit %>% pluck("tidied")
#> [[1]]
#> # A tibble: 4 x 5
#>   term  estimate std.error statistic  p.value
#>   <chr>    <dbl>     <dbl>     <dbl>    <dbl>
#> 1 y0     3528.   161.           22.0 1.88e-14
#> 2 xc      456.     0.00346  131523.  2.65e-82
#> 3 wG        1.04   0.0119       87.7 3.80e-25
#> 4 A     29869.   426.           70.1 2.15e-23
```

Thus, using the `plotFit` function, we can plot the obtained results,
since a picture is worth a thousand words. Individual peaks, the overall
fit and baseline are included on the plot. The residuals plot (at the
bottom) is very handy in deciding whether the fit is good or whether
there may be additional peaks lurking in the data. In the result shown
below the residuals are just noise indicating a good fit.

``` r
plotFit(data = Ba455_fit, title = "Ba II 455.40 nm") | 
  plotFit(data = Ca422_fit, title = "Ca I 422.67 nm")
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="90%" height="90%" />

In fact, we should rather look at the Voigt profile, especially if we
want to have an accurate measurement of the FWHM of the emission lines.
Notwithstanding self-absorption effect, the Lorentzian FWHM, `wL`,
estimate provides the broadening of the spectral lines while the
Gaussian FWHM, `wG`, estimate provides the instrumental broadening.

``` r
Ba455_fit2 <- corrected_spec %>% 
  peakfit(profile = "Voigt", wL = 0.1, wG = 0.9, A =  15000, wlgth.min = 454.09, wlgth.max = 457.09)
```

``` r
Ca422_fit2 <- corrected_spec %>% 
  peakfit(profile = "Voigt", wL = 0.1, wG = 0.9, A = 15000, wlgth.min = 421.65, wlgth.max = 424.30)
```

``` r
Ba455_fit2 %>% pluck("tidied")
#> [[1]]
#> # A tibble: 5 x 5
#>   term   estimate  std.error statistic  p.value
#>   <chr>     <dbl>      <dbl>     <dbl>    <dbl>
#> 1 y0     2714.     755.           3.60 2.23e- 3
#> 2 xc      456.       0.00343 132633.   4.51e-78
#> 3 wL        0.103    0.0897       1.15 2.65e- 1
#> 4 wG        0.942    0.0912      10.3  9.64e- 9
#> 5 A     33924.    3776.           8.98 7.26e- 8
```

``` r
plotFit(data = Ba455_fit2, title = "Ba II 455.40 nm") | 
  plotFit(data = Ca422_fit2, title = "Ca I 422.67 nm")
```

<img src="man/figures/README-unnamed-chunk-19-1.png" width="90%" height="90%" />

We can statistically compare (p-value) the results obtained with the
Gaussian profile with those obtained with the pseudo-Voigt profile, to
decide which of these lineshape functions gives the best estimate of
FWHM of the spectral line. Or we can calculate the *k* parameter which
describes the “Gaussness” or “Lorentzness” of a Voigt profile. A value
of 0 means a pure Lorentzian profile and of 1 means a pure Gaussian
profile. *k* is given by the following expression:

<p align="center">
<img src="images/MixingParam-01.png" width="129"/>
</p>

As expected, both methods show that Gaussian broadening accounts for
most of the observed broadening of the 455.50 nm Ba II emission line.

### Multiple peaks fitting

On the other hand, it may sometimes be more advisable to fit multiple
peaks at the same time, especially when they overlap. The `multipeakfit`
function has as input a vector of peaks center, `peaks`, and a vector of
lineshape functions associated with each of the peaks, `profiles`. It
worth noting that you can fit all peaks with a same fitting function or
fit each peak with a different function. In the example below, three
spectral lines, Ca II 393.37 nm, Ca II 396.85 nm and Mn I 403-nm were
fitted using the Gaussian profile.

``` r
mfit <- corrected_spec %>%
  multipeakfit(
    peaks = c(393.37, 396.85, 403.31), 
    profiles = c("Gaussian", "Gaussian", "Gaussian"), 
    wG = c(1, 1, 1), 
    A = c(15000, 8000, 10000), 
    wlgth.min = 392.0, wlgth.max = 405.5
    )
```

``` r
mfit
#> # A tibble: 3 x 6
#>   data               peak lineshape fit    tidied           augmented        
#>   <list>            <dbl> <chr>     <list> <list>           <list>           
#> 1 <tibble [99 x 2]>  393. Gaussian  <nls>  <tibble [4 x 5]> <tibble [99 x 4]>
#> 2 <tibble [99 x 2]>  397. Gaussian  <nls>  <tibble [4 x 5]> <tibble [99 x 4]>
#> 3 <tibble [99 x 2]>  403. Gaussian  <nls>  <tibble [4 x 5]> <tibble [99 x 4]>
```

#### Exemple

Before going any further, we are going to give here a typical example in
which we want to fit the Ca II 315.92 nm and Ca II 317.96 nm emission
lines. These spectral lines were acquired from a hundred samples, and
were identified by a unique name associated with each of the samples.
