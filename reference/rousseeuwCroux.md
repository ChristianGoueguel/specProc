# Rousseeuw-Croux Scale Estimators

This function calculates either the Rousseeuw-Croux Sn or Qn scale
estimator.

## Usage

``` r
rousseeuwCroux(x, estimator = c("Sn", "Qn"), drop.na = FALSE)
```

## Arguments

- x:

  A numeric vector of data values.

- estimator:

  A character string indicating whether to calculate the "Sn" or "Qn"
  estimator.

- drop.na:

  A logical value indicating whether to remove missing values (`NA`)
  from the input vector. If `FALSE` (default) and `x` contains missing
  values, `NA` is returned.

## Value

A numeric value representing the calculated Sn or Qn scale estimator.

## Details

The Sn and Qn estimators, proposed by Rousseeuw and Croux (1993), are
robust measures of scale (or dispersion) that are designed to have a
high breakdown point, which is a desirable property for estimators in
the presence of outliers or heavy-tailed distributions. Specifically,
both estimators have a breakdown point of 50%. The Sn estimator is based
on the median, while the Qn estimator is based on a weighted combination
of order statistics. Both estimators are consistent estimators of the
population scale parameter under normality. Although, the function uses
the `robustbase` package for estimating Sn and Qn, the bias-correction
factors used in the calculations have been, however, refined according
to Akinshin, A., (2022).

## References

- Rousseeuw, P.J. and Croux, C. (1993). Alternatives to the median
  absolute deviation. Journal of the American Statistical Association,
  88(424):1273-1283.

- Akinshin, A., (2022). Finite-sample Rousseeuw-Croux scale estimators.
  arxiv:2209.12268v1.

## Author

Christian L. Goueguel

## Examples

``` r
# Example 1:
x <- c(seq(1,100))
tibble::tibble(
sd = stats::sd(x),
mad = stats::mad(x),
Sn = rousseeuwCroux(x, estimator = "Sn"),
Qn = rousseeuwCroux(x, estimator = "Qn")
)
#> # A tibble: 1 × 4
#>      sd   mad    Sn    Qn
#>   <dbl> <dbl> <dbl> <dbl>
#> 1  29.0  37.1  29.8  30.0

# Example 2:
x <- c(seq(1,99), 1e3) # An outlier at 1000
tibble::tibble(
sd = stats::sd(x),
mad = stats::mad(x),
Sn = rousseeuwCroux(x, estimator = "Sn"),
Qn = rousseeuwCroux(x, estimator = "Qn")
)
#> # A tibble: 1 × 4
#>      sd   mad    Sn    Qn
#>   <dbl> <dbl> <dbl> <dbl>
#> 1  99.2  37.1  31.0  30.0
```
