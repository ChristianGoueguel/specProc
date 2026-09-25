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
  fences, quartiles, median and the estimated g and h parameters of each
  variable, and `outliers`, with the potential outliers (`out` gives the
  tail).

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
set.seed(123)
data <- data.frame(
  normal = rnorm(100),
  skewed = rexp(100, rate = 0.5),
  heavy_tailed = rt(100, df = 3)
)

# Plot the generalized boxplot
generalized_boxplot(data)


# Retrieve the generalized boxplot statistics
generalized_boxplot(data, plot = FALSE)
#> $stats
#> # A tibble: 3 × 8
#>   variable       lower     q1 median    q3 upper      g     h
#>   <fct>          <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>
#> 1 normal       -1.65   -0.494 0.0618 0.692  1.86 0.0947 0.102
#> 2 skewed       -0.0666  0.685 1.43   2.99   6.23 0.0925 0    
#> 3 heavy_tailed -2.75   -0.569 0.146  0.835  3.54 0.153  0.250
#> 
#> $outliers
#> # A tibble: 18 × 3
#>    variable     out   value
#>    <fct>        <chr> <dbl>
#>  1 normal       lower -2.31
#>  2 normal       lower -1.97
#>  3 normal       lower -1.69
#>  4 normal       upper  2.05
#>  5 normal       upper  2.17
#>  6 normal       upper  2.19
#>  7 skewed       upper  6.60
#>  8 skewed       upper  6.93
#>  9 skewed       upper  7.21
#> 10 skewed       upper  7.49
#> 11 skewed       upper  8.73
#> 12 heavy_tailed lower -8.61
#> 13 heavy_tailed lower -4.24
#> 14 heavy_tailed lower -3.22
#> 15 heavy_tailed lower -3.09
#> 16 heavy_tailed upper  3.70
#> 17 heavy_tailed upper  4.51
#> 18 heavy_tailed upper  6.42
#> 
```
