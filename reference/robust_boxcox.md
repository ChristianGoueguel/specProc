# Robust Box-Cox Transformation

Transforms each variable toward central normality with the Box-Cox
transformation, whose parameter \\\lambda\\ is fitted robustly
(Raymaekers and Rousseeuw, 2021), so that outlying observations do not
drive it.

## Usage

``` r
robust_boxcox(x, quantile = 0.99, nbsteps = 2, standardize = TRUE)
```

## Arguments

- x:

  A numeric vector, matrix, data frame or tibble, with one variable per
  column (such as line intensities, contents or spectral channels).

- quantile:

  The quantile of the absolute value of a standard normal variable
  beyond which the standardized transformed values are given weight zero
  in the re-weighting steps, strictly between 0 and 1. Default is 0.99.

- nbsteps:

  The number of re-weighting steps, a positive integer. Default is 2.

- standardize:

  A logical value: standardize the transformed variables by the mean and
  standard deviation of the inliers (`TRUE`, default).

## Value

An object of class `specproc_boxcox` (a list), which
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_transformation.md)
applies to new data, or with `inverse = TRUE` uses to transform values
back to the original scale (as a median or a mean), with the following
components:

- `transformation`: A tibble of the transformed variables.

- `summary`: A tibble with one row per variable: `variable`, `lambda`
  (`NA` for the variables left unchanged) and `objective` (the robust
  distance between the transformed values and the normal quantiles of
  step 2; lower is closer to normality).

- `fits`: The fitted parameters of each variable, used by
  [`predict()`](https://rdrr.io/r/stats/predict.html).

- `standardize`: The `standardize` argument.

## Details

The Box-Cox transformation (Box and Cox, 1964) of a strictly positive
value \\x\\ is \$\$x^{(\lambda)} = \frac{x^\lambda - 1}{\lambda}\$\$ for
\\\lambda \ne 0\\, and \\x^{(\lambda)} = \log x\\ for \\\lambda = 0\\. A
\\\lambda\\ below 1 compresses the upper tail, for right-skewed
variables (\\\lambda = 0\\ for log-normal ones), and a \\\lambda\\ above
1 compresses the lower tail, for left-skewed ones.

Each variable is first divided by its median, so that its transformed
median is 0 and \\\lambda\\ does not depend on its units. Variables with
zero or negative values are left unchanged, with a warning: use
[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md)
for them, as for baseline-corrected or SNV-scaled spectra. When
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_transformation.md)
is applied to new data, values that are zero or negative become `NA`.

## Robust fit

The parameter \\\lambda\\ of each variable is fitted as follows:

1.  The variable is pre-standardized (see Details).

2.  An initial \\\lambda\\ (between -4 and 6) minimizes a robust
    distance (Tukey's biweight) between the sorted, robustly
    standardized transformed values and the quantiles of the normal
    distribution. For this step, the transformation is continued
    linearly on the side that it compresses (the upper side for
    \\\lambda \< 1\\, the lower side for \\\lambda \> 1\\), beyond the
    point where the transformed value is 1.5 times that of the quartile,
    so that outliers on that side cannot dictate \\\lambda\\.

3.  The transformed values are standardized with Huber's M-estimates of
    location and scale, and those farther from the center than the
    `quantile` of the absolute value of a standard normal variable (2.58
    for the default 0.99) are given weight zero (first with the
    rectified transformation of step 2). \\\lambda\\ is then
    re-estimated by maximum likelihood on the others. This is repeated
    `nbsteps` times.

4.  With `standardize = TRUE`, the transformed variable is standardized
    by the mean and standard deviation of these inliers.

Variables with fewer than 5 values or no spread (zero MAD) are left
unchanged. Missing values are ignored by the fit, and stay missing.

## References

- Raymaekers, J., Rousseeuw, P.J. (2021). Transforming variables to
  central normality. Machine Learning,
  https://doi.org/10.1007/s10994-021-05960-5.

- Box, G.E.P., Cox, D.R. (1964). An analysis of transformations. Journal
  of the Royal Statistical Society, Series B, 26(2):211-252.

## See also

[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md)
for variables with zero or negative values,
[`step_robust_boxcox()`](https://christiangoueguel.com/specProc/reference/step_robust_boxcox.md)
to use it in a tidymodels recipe,
[`medcouple()`](https://christiangoueguel.com/specProc/reference/medcouple.md)
to measure the skewness before and after.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
contents <- forageLIBS[c("Ca", "K", "P", "Mn")]  # mg/kg
fit <- robust_boxcox(contents[1:300, ])
fit$summary
#> # A tibble: 4 × 3
#>   variable  lambda objective
#>   <chr>      <dbl>     <dbl>
#> 1 Ca        0.0291     0.540
#> 2 K         0.578      1.28 
#> 3 P         0.396      0.569
#> 4 Mn       -0.150      0.849
# the skewness of the contents, before and after
round(sapply(contents[1:300, ], medcouple), 2)
#>   Ca    K    P   Mn 
#> 0.12 0.09 0.07 0.16 
round(sapply(fit$transformation, medcouple), 2)
#>    Ca     K     P    Mn 
#>  0.00  0.04  0.01 -0.02 
# new samples, with the transformation of the first 300
head(predict(fit, contents[301:368, ]))
#> # A tibble: 6 × 4
#>       Ca      K      P      Mn
#>    <dbl>  <dbl>  <dbl>   <dbl>
#> 1 -1.35   0.546 -0.400  0.465 
#> 2  0.993 -0.139 -0.779  2.27  
#> 3  0.903  1.22   1.07  -0.867 
#> 4  1.00   0.269 -0.342 -0.408 
#> 5  1.49   0.389 -0.284  2.74  
#> 6 -2.41  -0.787 -0.862  0.0961
```
