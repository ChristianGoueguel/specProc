# Univariate Representation of Multivariate Outliers

This function creates a visual representation of multivariate outliers
using a univariate plot.It uses robust covariance estimation methods to
identify outliers and provides options for displaying the results
through various plotting styles.

## Usage

``` r
plot_outliers(
  x,
  quan = 1/2,
  alpha = 0.025,
  show.outlier = TRUE,
  show.mahal = FALSE
)
```

## Arguments

- x:

  A matrix or data frame.

- quan:

  A numeric value, between 0.5 and 1, that specifies the amount of
  observations which are used for MCD estimations. Default is 0.5.

- alpha:

  A numeric value specifying the amount of observations used for
  calculating the adjusted quantile. Default is 0.025.

- show.outlier:

  A logical value, if `TRUE` (default), outliers are highlighted in the
  plot.

- show.mahal:

  A logical value, if `FALSE` (default), robust Mahalanobis distances
  are not color-coded in the plot.

## Value

Depending on the combination of `show.outlier` and `show.mahal`:

- A `ggplot` object with outliers highlighted (if `show.outlier = TRUE`)

- A `ggplot` object with Mahalanobis distances color-coded (if
  `show.mahal = TRUE`)

- A `ggplot` object combining both outlier highlighting and Mahalanobis
  distance color-coding (if both `show.outlier` and `show.mahal` are
  `TRUE`)

- A tibble containing standardized scores, outlier flags, and robust
  multivariate Mahalanobis distances (if both `show.outlier` and
  `show.mahal` are `FALSE`)

## Details

The function uses the Minimum Covariance Determinant (MCD) method to
compute robust estimates of location and scatter. It then applies an
adaptive reweighting step to further improve the outlier detection. The
results are visualized using `ggplot2`, with options to highlight
outliers and/or color-code points based on their Mahalanobis distances.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# mineral contents (%) of the forage samples
contents <- forageLIBS[c("Ca", "Mg", "P", "K")]
plot_outliers(contents)

plot_outliers(contents, show.outlier = FALSE, show.mahal = TRUE)

# a data frame instead of a plot
head(plot_outliers(contents, show.outlier = FALSE, show.mahal = FALSE))
#> # A tibble: 6 × 6
#>       Ca     Mg      P     K outlier mahalanobis
#>    <dbl>  <dbl>  <dbl> <dbl> <lgl>         <dbl>
#> 1  0.671  0.196  0.228 3.37  TRUE          4.37 
#> 2  0.647 -1.02   0.407 0.993 FALSE         2.40 
#> 3 -0.240 -1.21  -0.783 0.849 FALSE         2.03 
#> 4  1.54   1.58   1.55  0.541 FALSE         1.93 
#> 5  0.498 -0.401 -0.881 1.71  FALSE         3.16 
#> 6  0.207 -0.316 -0.343 0.254 FALSE         0.761
```
