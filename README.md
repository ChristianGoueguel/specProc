
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
used in laser-induced breakdown spectroscopy (LIBS). Spectral
preprocessing try to minimise the effect of different types of
distortions on the data. Some preprocessing methods are solely based on
mathematics concepts, others are inspired by the chemical-physical
background of the data and the problem.

**Functions avaliable:**

| **Function**        | **Description**                                                                    |
|---------------------|------------------------------------------------------------------------------------|
| `adj_boxplot()`     | Adjusted boxplot for skewed distribution.                                          |
| `baselinerm()`      | Baseline correction.                                                               |
| `corr()`            | Correlation for each column in a data frame with respect to a response variable.   |
| `gaussian_func()`   | Gaussian lineshape.                                                                |
| `lorentzian_func()` | Lorentzian lineshape.                                                              |
| `voigt_func()`      | Voigt lineshape.                                                                   |
| `multipeakfit()`    | Multiple peaks fitting.                                                            |
| `normalize_spec()`  | Normalize spectra with different normalization methods.                            |
| `opls()`            | Orthogonal projections to latent structures.                                       |
| `osc()`             | Orthogonal signal correction.                                                      |
| `peakFit`           | Fitting of a single spectral line by lineshape functions with variable parameters. |
| `plotFit()`         | Plotting of spectral line fitting.                                                 |
| `plotspec()`        | Spectral data plot.                                                                |
| `robTransform()`    | Robust Box-Cox and Yeo-Johnson transformation.                                     |
| `summaryStat()`     | Descriptive Statistics.                                                            |

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("ChristianGoueguel/specProc")
```
