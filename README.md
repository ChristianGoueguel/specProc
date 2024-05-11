
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

**Testing and experimenting.**

The `specProc` package performs a number of preprocessing tasks commonly
used in laser-induced breakdown spectroscopy (LIBS). Spectral
preprocessing tries to minimize the effect of different types of
distortions on the data. Some preprocessing methods are solely based on
mathematics concepts, others are inspired by the chemical-physical
background of the dataset.

**Functions currently available:**

| **Function**        | **Description**                                                                   |
|---------------------|-----------------------------------------------------------------------------------|
| `adjboxplot()`      | Adjusted boxplot for skewed distribution                                          |
| `baselineRemoval()` | Baseline/continuum removal                                                        |
| `corr()`            | Correlation for each column in a data frame with respect to a response variable   |
| `gaussian()`        | Gaussian lineshape function                                                       |
| `lorentzian()`      | Lorentzian lineshape function                                                     |
| `voigt()`           | Pseudo-Voigt lineshape function                                                   |
| `plotSpec()`        | Spectral data plot                                                                |
| `normSpec()`        | Normalize spectra with different normalization methods                            |
| `opls()`            | Orthogonal projections to latent structures                                       |
| `osc()`             | Orthogonal signal correction                                                      |
| `peakfit()`         | Fitting of a single spectral line by lineshape functions with variable parameters |
| `plotfit()`         | Plotting of spectral line fitting and residuals                                   |
| `multipeakfit()`    | Multiple peaks fitting                                                            |
| `robTransform()`    | Robust Box-Cox and Yeo-Johnson transformation                                     |
| `summaryStats()`    | Descriptive Statistics                                                            |

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("ChristianGoueguel/specProc")
```
