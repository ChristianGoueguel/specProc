# Interquartile Range Method

This function identifies potential outliers in a numeric vector using
the interquartile range (IQR) method.

## Usage

``` r
iqr_outliers(x, k = 1.5, skew = FALSE, drop_na = FALSE, drop.na = deprecated())
```

## Arguments

- x:

  A numeric vector.

- k:

  A numeric value specifying the fence factor. `k = 1.5` is the default
  as it strikes a balance between sensitivity to mild outliers and
  robustness against extreme outliers. `k = 3` is more lenient and is
  sometimes used when a higher tolerance for outliers is desired.

- skew:

  A logical value indicating whether to calculate the version of the
  fences that accounts for moderate skewness in the underlying data
  distribution. By default, `skew = FALSE`, which calculates the fences
  assuming a symmetric distribution. However, if `skew = TRUE`, the
  lower and upper fences incorporate the medcouple, to account for
  potential asymmetry in the underlying data distribution. These
  formulas are explicitly derived and optimized for the scenario where
  `k = 1.5` (Hubert and Vandervieren, 2008). Consequently, if the user
  attempts to use a value of `k` other than 1.5, the code will issue a
  warning message indicating that the formula is only defined for
  `k = 1.5`. In such cases, the code will automatically reset `k` to 1.5
  and proceed with the calculations using the appropriate formulas and
  constants.

- drop_na:

  A logical value indicating whether to remove missing values (`NA`)
  from the calculations. If `TRUE`, missing values will be removed. If
  `FALSE` (the default), missing values are kept in the output (with a
  missing flag) and ignored when computing the fences.

- drop.na:

  **\[deprecated\]** Use `drop_na`.

## Value

A tibble with two columns:

- `data`: The original numeric values.

- `flag`: A logical vector indicating whether each value is a potential
  outlier or not.

## Details

For symmetric distributions, observations that fall outside the range
defined by the lower fence (Q1 - k \* IQR) and upper fence (Q3 + k \*
IQR) are considered as potential outliers, where Q1 and Q3 are the 25th
and 75th percentiles, respectively. The fence factor can be adjusted to
make the method more or less robust (often 1.5 or 3). Optionally, the
method can account for moderate skewness in data distributions by
incorporating the medcouple. In such a case, the lower and upper fences
are expressed in terms of the medcouple, adjusting the fences
asymmetrically to better accommodate skewed distributions. Note that the
implemented method does not explicitly account for tail heaviness. While
the medcouple can provide some robustness against heavy tails, the
method may still struggle to accurately identify potential outliers in
distributions with extreme kurtosis or long-tailed behavior.

## References

- Everitt, B. S., and Skrondal, A. (2010). The Cambridge Dictionary of
  Statistics. Cambridge University Press.

- Tukey, J. (1977). Exploratory Data Analysis. Addison-Wesley

- Hubert M., and Vandervieren E. (2008). An Adjusted Boxplot for Skewed
  Distributions. Computational Statistics & Data Analysis,
  52(12):5186-5201

## See also

[`medcouple()`](https://christiangoueguel.com/specProc/reference/medcouple.md),
[`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md),
[`directional_outlyingness()`](https://christiangoueguel.com/specProc/reference/directional_outlyingness.md)

## Author

Christian L. Goueguel

## Examples

``` r
# 200 right-skewed values (a gamma distribution shifted to start at 100),
# 3 low outliers below them and 3 high outliers
set.seed(1)
x <- c(100 + rgamma(200, shape = 2, scale = 10), 85, 87, 89, 220, 230, 240)
group <- rep(c("regular", "outlier"), c(200, 6))
ggplot2::ggplot(data.frame(x, group), ggplot2::aes(x)) +
  ggplot2::geom_density(fill = "#2166ac", alpha = 0.3) +
  ggplot2::geom_rug(ggplot2::aes(colour = group)) +
  ggplot2::scale_colour_manual(values = c(outlier = "#b2182b", regular = "#2166ac"))

medcouple(x)   # positive: skewed to the right
#> [1] 0.3088284

# symmetric fences: the upper fence flags regular values of the long tail,
# and the lower fence lies below the low outliers
subset(iqr_outliers(x), flag)
#> # A tibble: 6 × 2
#>    data flag 
#>   <dbl> <lgl>
#> 1  153. TRUE 
#> 2  157. TRUE 
#> 3  155. TRUE 
#> 4  220  TRUE 
#> 5  230  TRUE 
#> 6  240  TRUE 
# fences adjusted for the skewness: the 6 outliers, and no regular value
subset(iqr_outliers(x, skew = TRUE), flag)
#> # A tibble: 6 × 2
#>    data flag 
#>   <dbl> <lgl>
#> 1    85 TRUE 
#> 2    87 TRUE 
#> 3    89 TRUE 
#> 4   220 TRUE 
#> 5   230 TRUE 
#> 6   240 TRUE 

data(forageLIBS)
# iron contents (mg/kg): a few samples are far above the others
fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
iqr_outliers(fe)
#> # A tibble: 365 × 2
#>     data flag 
#>    <int> <lgl>
#>  1  1190 TRUE 
#>  2   391 TRUE 
#>  3   595 TRUE 
#>  4   591 TRUE 
#>  5  1030 TRUE 
#>  6   454 TRUE 
#>  7   424 TRUE 
#>  8  1080 TRUE 
#>  9  2060 TRUE 
#> 10   630 TRUE 
#> # ℹ 355 more rows
iqr_outliers(fe, skew = TRUE)   # adjusted for the skewness of the contents
#> # A tibble: 365 × 2
#>     data flag 
#>    <int> <lgl>
#>  1    64 TRUE 
#>  2  1190 TRUE 
#>  3  1030 TRUE 
#>  4    62 TRUE 
#>  5    63 TRUE 
#>  6  1080 TRUE 
#>  7  2060 TRUE 
#>  8    59 TRUE 
#>  9    50 TRUE 
#> 10    54 TRUE 
#> # ℹ 355 more rows
```
