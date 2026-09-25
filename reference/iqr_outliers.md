# Interquartile Range Method

This function identifies potential outliers in a numeric vector using
the interquartile range (IQR) method.

## Usage

``` r
iqr_outliers(x, k = 1.5, skew = FALSE, drop.na = FALSE)
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

- drop.na:

  A logical value indicating whether to remove missing values (`NA`)
  from the calculations. If `TRUE`, missing values will be removed. If
  `FALSE` (the default), missing values are kept in the output (with a
  missing flag) and ignored when computing the fences.

## Value

A tibble with two columns:

- `data`: The original numeric values.

- `flag`: A logical vector indicating whether each value is a potential
  outlier or not.

## Details

For symmetric distributions, observations that fall outside the range
defined by the lower fence (Q1 - k × IQR) and upper fence (Q3 + k × IQR)
are considered as potential outliers, where Q1 and Q3 are the 25th and
75th percentiles, respectively. The fence factor can be adjusted to make
the method more or less robust (often 1.5 or 3). Optionally, the method
can account for moderate skewness in data distributions by incorporating
the medcouple. In such a case, the lower and upper fences are expressed
in terms of the medcouple, adjusting the fences asymmetrically to better
accommodate skewed distributions. Note that the implemented method does
not explicitly account for tail heaviness. While the medcouple can
provide some robustness against heavy tails, the method may still
struggle to accurately identify potential outliers in distributions with
extreme kurtosis or long-tailed behavior.

## References

- Everitt, B. S., and Skrondal, A. (2010). The Cambridge Dictionary of
  Statistics. Cambridge University Press.

- Tukey, J. (1977). Exploratory Data Analysis. Addison-Wesley

- Hubert M., and Vandervieren E. (2008). An Adjusted Boxplot for Skewed
  Distributions. Computational Statistics & Data Analysis,
  52(12):5186-5201

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(3317)
x <- stats::rexp(7, rate = 0.5)
iqr_outliers(x)
#> # A tibble: 7 × 2
#>     data flag 
#>    <dbl> <lgl>
#> 1 7.50   FALSE
#> 2 0.466  FALSE
#> 3 0.315  FALSE
#> 4 6.94   FALSE
#> 5 0.0899 FALSE
#> 6 0.328  FALSE
#> 7 1.75   FALSE

iqr_outliers(x, skew = TRUE)
#> # A tibble: 7 × 2
#>     data flag 
#>    <dbl> <lgl>
#> 1 0.0899 TRUE 
#> 2 7.50   FALSE
#> 3 0.466  FALSE
#> 4 0.315  FALSE
#> 5 6.94   FALSE
#> 6 0.328  FALSE
#> 7 1.75   FALSE
```
