# Generalized Boxplot

This function implements the generalized boxplot, a robust data
visualization technique designed to effectively represent skewed and
heavy-tailed distributions, as proposed by Bruffaerts *et al*. (2014).

## Usage

``` r
generalized_boxplot(
  x,
  alpha = 0.05,
  p = 0.9,
  plot = TRUE,
  xlabels.angle = 90,
  xlabels.vjust = 1,
  xlabels.hjust = 1,
  box.width = 0.5,
  notch = FALSE,
  notchwidth = 0.5,
  staplewidth = 0.5
)
```

## Arguments

- x:

  A numeric data frame or tibble.

- alpha:

  A scalar, between 0 and 1 that specifies the desired detection rate of
  atypical values.

- p:

  A scalar, between 0.5 and 1 that specifies the quantile order for
  estimating g and h.

- plot:

  Logical value indicating whether to plot the boxplot or return the
  boxplot statistics.

- xlabels.angle:

  A numeric value specifying the angle (in degrees) for x-axis labels
  (default is 90).

- xlabels.vjust:

  A numeric value specifying the vertical justification of x-axis labels
  (default is 1).

- xlabels.hjust:

  A numeric value specifying the horizontal justification of x-axis
  labels (default is 1).

- box.width:

  A numeric value specifying the width of the boxplot (default is 0.5).

- notch:

  A logical value indicating whether to display a notched boxplot
  (default is `FALSE`).

- notchwidth:

  A numeric value specifying the width of the notch relative to the body
  of the boxplot (default is 0.5).

- staplewidth:

  A numeric value specifying the width of staples at the ends of the
  whiskers.

## Value

- If `plot = TRUE`, returns a `ggplot2` object containing the
  generalized boxplot.

- If `plot = FALSE`, returns a list of tibbles: `stats`, with the
  whisker ends (`lower`, `upper`: the most extreme observations within
  the fences), quartiles, median, fences and the estimated g and h
  parameters of each variable, and `outliers`, with the potential
  outliers (`out` gives the tail).

## Details

This method extends the adjusted boxplot method by leveraging the
flexible Tukey's g-and-h parametric distribution to model the underlying
data structure, particularly for asymmetric or long-tailed datasets,
providing a more nuanced and informative summary of the data's central
tendency, spread, and potential outliers.

## References

- Bruffaerts, C., Verardi, V., Vermandele, C. (2014). A generalized
  boxplot for skewed and heavy-tailed distributions. Statistics and
  Probability Letters 95(C):110–117

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# mineral contents (%) of the forage samples
generalized_boxplot(forageLIBS[c("Ca", "Mg", "P", "K", "S")])

generalized_boxplot(forageLIBS[c("Ca", "Mg", "P", "K", "S")], plot = FALSE)
#> $stats
#> # A tibble: 5 × 10
#>   variable lower    q1 median    q3 upper lower_fence upper_fence       g      h
#>   <fct>    <dbl> <dbl>  <dbl> <dbl> <dbl>       <dbl>       <dbl>   <dbl>  <dbl>
#> 1 Ca       0.372 0.515  0.629 0.772 1.08        0.359       1.09   0.167  0     
#> 2 Mg       0.116 0.172  0.204 0.240 0.315       0.115       0.317  0.136  0.0636
#> 3 P        0.162 0.219  0.260 0.300 0.393       0.160       0.394  0.0976 0.0154
#> 4 K        1.04  1.72   2.01  2.38  3.04        1.02        3.10  -0.0309 0.0851
#> 5 S        0.14  0.17   0.2   0.23  0.29        0.132       0.290 -0.0961 0     
#> 
#> $outliers
#> # A tibble: 101 × 3
#>    variable out   value
#>    <fct>    <chr> <dbl>
#>  1 Ca       lower 0.173
#>  2 Ca       lower 0.292
#>  3 Ca       lower 0.313
#>  4 Ca       lower 0.323
#>  5 Ca       lower 0.337
#>  6 Ca       lower 0.342
#>  7 Ca       lower 0.342
#>  8 Ca       lower 0.347
#>  9 Ca       lower 0.353
#> 10 Ca       lower 0.354
#> # ℹ 91 more rows
#> 
```
