
<!-- README.md is generated from README.Rmd. Please edit that file -->

# specProc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`specProc` package performs a number of pre-processing tasks commonly
used in laser-induced breakdown spectroscopy (LIBS). Collectively, these
are essential tools in LIBS calibration modeling, called chemometrics.
These include:

-   peak analysis
-   Spectral-based normalization
-   Smoothing and filtering
-   Robust or classical Box-Cox transformation
-   Pearson or Spearman peaks correlation
-   Descriptive statistics such as mean, standard deviation and higher
    central moments
-   Statistical tests
-   Rowwise and cellwise outliers detection
-   Robust or classical principal component analysis (PCA)
-   Orthogonal partial-least squares (OPLS)
-   PLS-based wavelength selection

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ChristianGoueguel/specProc")
```

## Exemples

Loading `specProc` package.

``` r
library(specProc)
```

    #> ℹ Loading specProc

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.4     ✓ dplyr   1.0.7
#> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
#> ✓ readr   2.0.1     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(patchwork)
```

### Peak analysis

The package peak analysis capabilities include:

-   Baseline removal
-   Peaks detection
-   Fitting single, multiple or overlapping peaks

#### Baseline removal

For instance, when analyzing LIBS spectra, it is often more effective to
subtract an estimated baseline from the data. The estimate is
constructed by fitting a low-order polynomial function to the spectrum
baseline. Then the resulting curve fit result is subtracted from the
data.

``` r
data(specData)
```

``` r
baseline_fit <- specData %>% 
  slice(1L) %>% 
  baselinerm(degree = 7)
```

``` r
str(baseline_fit, list.len = 5) 
#> List of 2
#>  $ spec: tibble [1 × 15,036] (S3: tbl_df/tbl/data.frame)
#>   ..$ 240.00205 : num 155
#>   ..$ 240.010279: num 172
#>   ..$ 240.018507: num 198
#>   ..$ 240.026736: num 213
#>   ..$ 240.034965: num 201
#>   .. [list output truncated]
#>  $ bkg : tibble [1 × 15,036] (S3: tbl_df/tbl/data.frame)
#>   ..$ 240.00205 : num 717
#>   ..$ 240.010279: num 717
#>   ..$ 240.018507: num 717
#>   ..$ 240.026736: num 717
#>   ..$ 240.034965: num 718
#>   .. [list output truncated]
```

``` r
corrected_spec <- pluck(baseline_fit, "spec")
background <- pluck(baseline_fit, "bkg")
```

``` r
plot1 <- specData %>% 
  select(where(is.numeric)) %>%
  slice(1L) %>%
  plotSpec()
```

``` r
plot2 <- corrected_spec %>% 
  select(where(is.numeric)) %>%
  slice(1L) %>%
  plotSpec()
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="90%" height="90%" />

#### Peaks detection

#### Peaks fitting

The idea of curve fitting is to find a mathematical model that fits your
data. We assume that you have theoretical reasons for picking a function
of a certain form. The curve fit finds the specific coefficients
(parameters) which make that function match your data as closely as
possible. For non-linear least-squares data fitting, Igor uses the
Levenberg-Marquardt algorithm to search for the minimum value of
chisquare. Chi-square defines a surface in a multidimensional error
space. The search process involves starting with an initial guess at the
coefficient values. Starting from the initial guesses, Igor searches for
the minimum value by travelling down hill from the starting point on the
chi-square surface.
