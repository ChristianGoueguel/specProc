
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

## Peak analysis

The package peak analysis capabilities include:

-   Baseline removal
-   Peaks detection
-   Fitting single, multiple or overlapping peaks

### Baseline removal

For instance, when analyzing LIBS spectra, it is often more effective to
subtract an estimated baseline from the data. The estimate is
constructed by fitting a low-order polynomial function to the spectrum
baseline. Then the resulting curve fit result is subtracted from the
data.

``` r
library(specProc)
```

``` r
avg_spec <- arrow::read_parquet("~/Documents/Laserag/R&D projects/Soil/Eurofins/SOC/processed data/avg_spec")
LIBS_spec <- avg_spec %>%
  filter(spectra_id %in% "142146") %>%
  select(where(is.numeric))
```

``` r
baseline_fit <- baseline(data = LIBS_spec)
str(baseline_fit, list.len = 5)  
#> List of 2
#>  $ spec: tibble [1 × 40,363] (S3: tbl_df/tbl/data.frame)
#>   ..$ 198.851958: num 0
#>   ..$ 198.858023: num 0
#>   ..$ 198.864087: num 0
#>   ..$ 198.870152: num 0
#>   ..$ 198.876217: num 0
#>   .. [list output truncated]
#>  $ bkg : tibble [1 × 40,363] (S3: tbl_df/tbl/data.frame)
#>   ..$ 198.851958: num 485
#>   ..$ 198.858023: num 485
#>   ..$ 198.864087: num 485
#>   ..$ 198.870152: num 485
#>   ..$ 198.876217: num 485
#>   .. [list output truncated]
```

``` r
corrected_spec <- pluck(baseline_fit, "spec")
```

``` r
background <- baseline_fit %>%
  pluck("bkg") %>%
  pivot_longer(
    cols = everything(), 
    names_to = "wavelength",
    values_to = "intensity"
    ) %>%
  modify_at("wavelength", as.numeric)
```

``` r
plot1 <- plotSpec(data = LIBS_spec)
plot2 <- plotSpec(data = corrected_spec)
```
