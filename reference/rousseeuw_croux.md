# Rousseeuw-Croux Scale Estimators

This function calculates either the Rousseeuw-Croux Sn or Qn scale
estimator.

## Usage

``` r
rousseeuw_croux(x, estimator = c("Sn", "Qn"), drop.na = FALSE)
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
data(forageLIBS)
# iron contents (mg/kg): a few samples are far above the others
fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
c(sd = stats::sd(fe), mad = stats::mad(fe),
  Sn = rousseeuw_croux(fe, estimator = "Sn"), Qn = rousseeuw_croux(fe, estimator = "Qn"))
#>        sd       mad        Sn        Qn 
#> 210.30037  78.57780  76.47013  75.12173 
```
