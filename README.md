
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

# specProc <img src="man/figures/logo.png" align="right" height="160"/>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

**Pre-release for feedback and experimenting.**

`specProc` package performs a number of preprocessing tasks commonly
used in laser-induced breakdown spectroscopy (LIBS). Collectively, these
are essential tools in LIBS calibration modeling. Some preprocessing
methods are solely based on mathematics concepts, others are inspired by
the chemical-physical background of the data and the problem. Available
methods:

-   Data transformations (log-log, power, Box-Cox, logit)
-   Column-wise transformations (mean-centering, scaling, autoscaling)
-   Robust measures
-   Row-wise transformations (normalizastion to a constant sum of the
    variables or to a constant maximum or constant vector length)
-   Transformation of compositional data

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ChristianGoueguel/specProc")
```
