
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
set.seed(123)
tbl <- data.frame(
  normal = stats::rnorm(100),
  skewed = stats::rgamma(100, shape = 2, scale = 1),
  heavy_tailed = stats::rcauchy(100, location = 0, scale = 1)
  )
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="90%" height="90%" />

### descriptive statistics

#### classical approach

``` r
specProc::summaryStats(tbl)
#> # A tibble: 3 × 14
#>   variable     mean  mode median   IQR    sd variance     cv     min   max range
#>   <chr>       <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>  <dbl>   <dbl> <dbl> <dbl>
#> 1 heavy_tail… -1.43  0.81  -0.1   1.87  9.28   86.0   -649.  -80.6    8.98 89.6 
#> 2 normal       0.09 -0.56   0.06  1.19  0.91    0.833 1011.   -2.31   2.19  4.50
#> 3 skewed       1.93  0.76   1.62  1.52  1.26    1.58    65.3   0.244  6.65  6.41
#> # ℹ 3 more variables: skewness <dbl>, kurtosis <dbl>, count <int>
```

#### robust approach

``` r
specProc::summaryStats(tbl, robust = TRUE)
#> # A tibble: 3 × 14
#>   variable    median   mad    Qn    Sn medcouple   LMC   RMC   rsd biloc biscale
#>   <chr>        <dbl> <dbl> <dbl> <dbl>     <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
#> 1 heavy_tail…  -0.1   1.4   1.78  1.61     -0.03  0.54  0.36  2.08  0.05    2.01
#> 2 normal        0.06  0.89  0.93  0.92      0.03  0.33  0.21  1.32  0.08    0.92
#> 3 skewed        1.62  1.1   1.05  1.16      0.25 -0.05  0.47  1.63  1.79    1.21
#> # ℹ 3 more variables: bivar <dbl>, rcv <dbl>, count <int>
```

### adjusted boxplot

``` r
specProc::adjboxplot(tbl) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.1)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="90%" height="90%" />
