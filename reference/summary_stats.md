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
# Load the iris dataset
data(iris)

# Example1:
iris |> summary_stats()
#> # A tibble: 4 × 14
#>   variable      mean  mode median   IQR    sd variance    cv   min   max range
#>   <chr>        <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Sepal.Length  5.84   5     5.8    1.3  0.83     0.69  14.2   4.3   7.9   3.6
#> 2 Sepal.Width   3.06   3     3      0.5  0.44     0.19  14.3   2     4.4   2.4
#> 3 Petal.Length  3.76   1.4   4.35   3.5  1.77     3.12  47.0   1     6.9   5.9
#> 4 Petal.Width   1.2    0.2   1.3    1.5  0.76     0.58  63.6   0.1   2.5   2.4
#> # ℹ 3 more variables: skewness <dbl>, kurtosis <dbl>, count <int>

# Example2:
iris |> summary_stats(
  var = c("Sepal.Length", "Petal.Length"),
  robust = TRUE
  )
#> # A tibble: 2 × 13
#>   variable    median   mad    Qn    Sn medcouple   LMC   RMC biloc biscale bivar
#>   <chr>        <dbl> <dbl> <dbl> <dbl>     <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>
#> 1 Sepal.Leng…   5.8   1.04  0.87  0.83      0.06 -0.2   0.25  5.82    0.84  0.71
#> 2 Petal.Leng…   4.35  1.85  1.08  1.91     -0.4  -0.81  0.25  3.83    1.95  3.81
#> # ℹ 2 more variables: rcv <dbl>, count <int>
```
