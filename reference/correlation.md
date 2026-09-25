# Correlation Coefficients: Pearson, Spearman, Kendall, Chatterjee, and Biweight Midcorrelation

Computes various correlation coefficients between a specified response
variable and each of the remaining variables in a given data frame or
tibble. The available correlation methods are Pearson's product-moment
correlation (parametric), Spearman's rank correlation, Kendall's tau
correlation (non-parametric), Chatterjee's new correlation coefficient,
and the biweight midcorrelation (a robust correlation measure).

## Usage

``` r
correlation(
  x,
  var,
  method = "pearson",
  plot = FALSE,
  color = "#111D71",
  interactive = FALSE
)
```

## Arguments

- x:

  A data frame or tibble containing the variables of interest.

- var:

  The response variable, given unquoted or as a string.

- method:

  A character string indicating the correlation method to use. Allowed
  values are "pearson", "spearman", "kendall", "chatterjee", or "bicor"
  (for biweight midcorrelation). The default is "pearson".

- plot:

  A logical value indicating whether to produce a visualization of the
  correlations. Default is FALSE (no plot).

- color:

  A character string specifying the color to use for the plot. Default
  is "#111D71".

- interactive:

  A logical value indicating whether to create an interactive plot using
  plotly. Default is FALSE (static ggplot2 plot).

## Value

- If `plot = FALSE`, a tibble with columns `variable`, `.correlation`
  and `method`, sorted by decreasing correlation.

- If `plot = TRUE`, a list containing the tibble (`correlation`) and a
  `ggplot2` object (`plot`).

- If `plot = TRUE` and `interactive = TRUE`, a `plotly` object.

## Details

The Pearson correlation coefficient measures the linear relationship
between two continuous variables and is suitable when the data follows a
bivariate normal distribution. The Spearman and Kendall correlations are
non-parametric measures of monotonic association, making them suitable
for non-linear relationships and when the data deviates from normality.
The Chatterjee correlation coefficient \\\xi_n(X, Y)\\ measures how much
the response `var` is a (possibly non-monotonic) function of each other
variable; it lies between 0 and 1 (asymptotically) and is not symmetric.
The biweight midcorrelation is a robust correlation measure that
downweights the influence of outliers and is recommended when the data
contains extreme values or deviates significantly from normality.

Missing values are handled pairwise: each coefficient uses the
observations where both the response and the other variable are
available.

## References

- Chatterjee, S. (2021). A new coefficient of correlation. Journal of
  the American Statistical Association, 116(536):2009-2022.

- Wilcox, R. (2012). Introduction to robust estimation and hypothesis
  testing (3rd ed.). Academic Press. (ISBN 978-0123869838).

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
df <- data.frame(y = rnorm(50))
df$a <- 2 * df$y + rnorm(50, sd = 0.5)
df$b <- -df$y + rnorm(50)
df$c <- rnorm(50)
correlation(df, y)
#> # A tibble: 3 × 3
#>   variable .correlation method 
#>   <chr>           <dbl> <chr>  
#> 1 a               0.959 pearson
#> 2 c              -0.272 pearson
#> 3 b              -0.668 pearson
correlation(df, "y", method = "bicor")
#> # A tibble: 3 × 3
#>   variable .correlation method
#>   <chr>           <dbl> <chr> 
#> 1 a               0.948 bicor 
#> 2 c              -0.282 bicor 
#> 3 b              -0.664 bicor 
```
