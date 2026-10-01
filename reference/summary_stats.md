# Classical or Robust Descriptive Statistics

This function calculates various descriptive statistics (robust and
non-robust) for a specified variable or all variables in a given data
frame or tibble.

## Usage

``` r
summary_stats(x, var = NULL, digits = 2, robust = FALSE, drop.na = TRUE)
```

## Arguments

- x:

  A data frame or tibble.

- var:

  A character vector of names (or a numeric vector of column positions)
  specifying the variable(s) for which to calculate the summary
  statistics. If left as `NULL` (the default), summary statistics will
  be calculated for all numeric variables in the data frame/tibble.

- digits:

  An integer specifying the number of significant digits to display
  after the decimal point in the output.

- robust:

  A logical value indicating whether to compute robust descriptive
  statistics. If `FALSE` (the default), computes the classical
  descriptive statistics for describing the distribution of a univariate
  variable.

- drop.na:

  A logical value indicating whether to remove missing values (`NA`)
  from the calculations. If `TRUE` (the default), missing values will be
  removed. If `FALSE`, the statistics of variables containing missing
  values are `NA`.

## Value

A tibble with one row per variable. The classical statistics are the
mean, mode, median, IQR, standard deviation, variance, coefficient of
variation (`cv`, in \\ the median, MAD (scaled to be consistent at the
normal distribution), Qn and Sn estimators, medcouple, left/right
medcouples, biweight location, scale and midvariance, robust coefficient
of variation (`rcv` = MAD / median, in \\ Robust statistics that cannot
be computed (e.g. for a constant variable) are `NA`.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# mineral contents (%) of the forage samples
summary_stats(forageLIBS[c("Ca", "Mg", "P", "K", "S")])
#> # A tibble: 5 × 14
#>   variable  mean  mode median   IQR    sd variance    cv    min   max range
#>   <chr>    <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1 Ca        0.65  0.5    0.63  0.26  0.18     0.03  27.8 0.173  1.45  1.28 
#> 2 Mg        0.21  0.14   0.2   0.07  0.05     0     25.3 0.0527 0.365 0.312
#> 3 P         0.26  0.21   0.26  0.08  0.06     0     24.3 0.0751 0.522 0.447
#> 4 K         2.05  2.07   2.01  0.66  0.53     0.29  26.0 0.497  4.04  3.54 
#> 5 S         0.2   0.19   0.2   0.06  0.04     0     21.6 0.12   0.39  0.27 
#> # ℹ 3 more variables: skewness <dbl>, kurtosis <dbl>, count <int>
summary_stats(forageLIBS, var = c("Ca", "K"), robust = TRUE)
#> # A tibble: 2 × 13
#>   variable median   mad    Qn    Sn medcouple   LMC   RMC biloc biscale bivar
#>   <chr>     <dbl> <dbl> <dbl> <dbl>     <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>
#> 1 Ca         0.63  0.18  0.18  0.18      0.14  0.08  0.18  0.64    0.18  0.03
#> 2 K          2.01  0.51  0.51  0.49      0.06  0.29  0.17  2.02    0.53  0.28
#> # ℹ 2 more variables: rcv <dbl>, count <int>
```
