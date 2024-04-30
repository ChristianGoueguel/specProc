
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

# specProc <img src="man/figures/logo.png" align="right" height="160"/>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Codecov test
coverage](https://codecov.io/gh/ChristianGoueguel/specProc/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ChristianGoueguel/specProc?branch=main)
<!-- badges: end -->

**Pre-release for feedback and experimenting.**

`specProc` package performs a number of preprocessing tasks commonly
used in spectral data analysis. Some preprocessing methods are solely
based on mathematics concepts, others are inspired by the
chemical-physical background of the data and the problem.

-   Orthogonal signal correction
-   Orthogonal partial least-squares
-   Generalized least squares weighting
-   External parameter orthogonalization
-   Cellwise & rowwise outliers detection
-   Robust Box-Cox and Yeo-Johnson transformation
-   Skewness-adjusted boxplot
-   Baseline removal
-   Fitting single, multiple or overlapping peaks
-   Spectra normalization
-   Pearson, Spearman and Kendall correlation
-   Descriptive statistics such as mean, standard deviation and higher
    central moments

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("ChristianGoueguel/specProc")
```
