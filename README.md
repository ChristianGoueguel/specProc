
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" align="right" height="250"/>

# specProc

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

**Pre-release for feedback and experimenting.**

`specProc` package performs a number of pre-processing tasks commonly
used in laser-induced breakdown spectroscopy (LIBS). Collectively, these
are essential tools in LIBS calibration modeling. These include:

**Multivariate Filtering:**

-   Orthogonal Signal Correction (OSC)
-   Orthogonal partial least-squares (OPLS)
-   Generalized Least Squares Weighting (GLSW)
-   External Parameter Orthogonalization (EPO)

**Robust Methods:**

-   Cellwise & Rowwise outliers detection (CROD)
-   Robust Box-Cox and Yeo-Johnson transformation
-   Skewness-adjusted boxplot
-   Detecting deviating cells (DDC)
-   Robust PCA methods (ROBPCA)
-   Robust PLS method (RSIMPLS)
-   Robust soft independent modelling by class analogy (RSIMCA)

**Miscellaneous:**

-   Multivariate Cluster Elastic Net (MCEN)
-   NIST interactive plot of LIBS emission lines
-   Baseline removal
-   Fitting single, multiple or overlapping peaks
-   Spectra normalization
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
