
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
[![R-CMD-check](https://github.com/ChristianGoueguel/specProc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ChristianGoueguel/specProc/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

**Testing and experimenting.**

The `specProc` package performs a wide range of preprocessing tasks
essential for spectroscopic data analysis. Spectral preprocessing is
essential in ensuring accurate and reliable results by minimizing the
impact of various distortions and artifacts that can arise during data
acquisition or due to inherent characteristics of the sample or
instrument. Some of the techniques are purely based on mathematical
concepts, leveraging robust statistics and signal processing techniques.
Additionally, `specProc` incorporates preprocessing methods inspired by
the chemical-physical background of the dataset. These techniques
leverage domain knowledge and exploit the fundamental principles
governing the spectroscopic phenomenon under investigation.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("ChristianGoueguel/specProc")
```

## Examples

``` r
tbl <- data.frame(
  normal = stats::rnorm(100),
  skewed = stats::rgamma(100, shape = 1, scale = 1),
  heavy_tailed = stats::rcauchy(100, location = 0, scale = 1)
  )
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="90%" height="90%" />

### adjusted boxplot

``` r
specProc::adjboxplot(tbl) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.1)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="90%" height="90%" />
