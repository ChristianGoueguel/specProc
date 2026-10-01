# Adjusted Boxplot

This function generates the adjusted boxplot, which is a robust
graphical method for visualizing skewed data distributions. It provides
a more accurate representation of the data's spread and skewness
compared to standard boxplot, especially in the presence of outliers.

## Usage

``` r
adjusted_boxplot(
  x,
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

- plot:

  A logical value indicating whether to plot the adjusted boxplot
  (default is `TRUE`).

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

- If `plot = TRUE`, returns a `ggplot2` object containing the adjusted
  boxplot.

- If `plot = FALSE`, returns a list of tibbles with the adjusted boxplot
  statistics and potential outliers.

## Details

The function is based on the medcouple (MC) measure computed on the data
and which robustly measures skewness. This measure is bounded between −1
and 1. The medcouple is equal to zero when the observed distribution is
symmetric, whereas a positive (resp. negative) value of MC corresponds
to a right (resp. left) tailed distribution. It worth noting that this
method is more appropriate for distributions that are not excessively
skewed i.e., for \\\|\text{MC}\| \leq 0.6\\.

## References

The adjusted boxplot is based on the methodology described in:

- Brys, G., Hubert, M., Struyf, A., (2004). A Robust Measure of
  Skewness. Journal of Computational and Graphical Statistics,
  13(4):996-1017

- Hubert, M., Vandervieren, E., (2008). An adjusted boxplot for skewed
  distributions. Computational Statistics and Data Analysis,
  52(12):5186-5201

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# mineral contents (%) of the forage samples
adjusted_boxplot(forageLIBS[c("Ca", "Mg", "P", "K", "S")])

adjusted_boxplot(forageLIBS[c("Ca", "Mg", "P", "K", "S")], plot = FALSE)
#> $stats
#> # A tibble: 5 × 7
#>   variable  lower    q1 median    q3 upper medcouple
#>   <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl>     <dbl>
#> 1 Ca       0.292  0.514  0.629 0.772 1.2      0.137 
#> 2 Mg       0.0798 0.172  0.204 0.24  0.342    0.0168
#> 3 P        0.114  0.218  0.260 0.301 0.428    0.0143
#> 4 K        0.981  1.72   2.01  2.38  3.58     0.0645
#> 5 S        0.12   0.17   0.2   0.23  0.32     0     
#> 
#> $outliers
#> # A tibble: 20 × 2
#>    variable  value
#>    <fct>     <dbl>
#>  1 Ca       1.45  
#>  2 Ca       0.173 
#>  3 Mg       0.365 
#>  4 Mg       0.359 
#>  5 Mg       0.362 
#>  6 Mg       0.0527
#>  7 P        0.0751
#>  8 P        0.0847
#>  9 P        0.522 
#> 10 P        0.436 
#> 11 K        3.68  
#> 12 K        0.829 
#> 13 K        0.497 
#> 14 K        0.925 
#> 15 K        4.04  
#> 16 S        0.35  
#> 17 S        0.36  
#> 18 S        0.33  
#> 19 S        0.39  
#> 20 S        0.37  
#> 
```
