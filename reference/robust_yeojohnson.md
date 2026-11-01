# Robust Yeo-Johnson Transformation

Transforms each variable toward central normality with the Yeo-Johnson
transformation, whose parameter \\\lambda\\ is fitted robustly
(Raymaekers and Rousseeuw, 2021), so that outlying observations do not
drive it. Unlike the Box-Cox transformation
([`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md)),
it accepts zero and negative values.

## Usage

``` r
robust_yeojohnson(x, quantile = 0.99, nbsteps = 2, standardize = TRUE)
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

An object of class `specproc_yeojohnson` (a list), which
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_transformation.md)
applies to new data, with the following components:

- `transformation`: A tibble of the transformed variables.

- `summary`: A tibble with one row per variable: `variable`, `lambda`
  (`NA` for the variables left unchanged) and `objective` (the robust
  distance between the transformed values and the normal quantiles of
  step 2; lower is closer to normality).

- `fits`: The fitted parameters of each variable, used by
  [`predict()`](https://rdrr.io/r/stats/predict.html).

- `standardize`: The `standardize` argument.

## Details

The Yeo-Johnson transformation (Yeo and Johnson, 2000) of a value \\x
\ge 0\\ is \$\$\psi\_\lambda(x) = \frac{(1 + x)^\lambda -
1}{\lambda}\$\$ (\\\log(1 + x)\\ for \\\lambda = 0\\), and of a value
\\x \< 0\\ \$\$\psi\_\lambda(x) = -\frac{(1 - x)^{2 - \lambda} - 1}{2 -
\lambda}\$\$ (\\-\log(1 - x)\\ for \\\lambda = 2\\). \\\lambda = 1\\
leaves the values unchanged; a \\\lambda\\ below 1 compresses the upper
tail, for right-skewed variables, and a \\\lambda\\ above 1 compresses
the lower tail, for left-skewed ones.

Each variable is first centered by its median and divided by its MAD, so
that \\\lambda\\ does not depend on its location or units. It describes
the shape of the distribution on this robust standard scale, and cannot
be compared with the \\\lambda\\ of
[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md):
on the same data, it is often much closer to 1.

It suits the variables with zero or negative values, such as
baseline-corrected, derivative or SNV-scaled spectra. Strictly positive
variables, such as raw intensities or contents, can be transformed with
either transformation.

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

- Yeo, I.-K., Johnson, R.A. (2000). A new family of power
  transformations to improve normality or symmetry. Biometrika,
  87(4):954-959.

## See also

[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md),
[`step_robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/step_robust_yeojohnson.md)
to use it in a tidymodels recipe,
[`medcouple()`](https://christiangoueguel.com/specProc/reference/medcouple.md)
to measure the skewness before and after.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
spectra <- snv(forageLIBS[-(1:14)])$correction
wl <- as.numeric(names(spectra))
# the Mg II lines at 279.55 and 280.27 nm: after SNV, most channels have
# negative values in some spectra, which Box-Cox cannot transform
mg <- spectra[which(wl > 279 & wl < 281)]
sum(vapply(mg, min, numeric(1)) < 0)
#> [1] 14
fit <- robust_yeojohnson(mg[1:300, ])
summary(fit$summary$lambda)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.5844  0.6350  0.6868  0.8031  0.8858  1.4372 
# the skewness of the channels, before and after
summary(medcouple(mg[1:300, ]))
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -0.11400  0.04476  0.08922  0.06141  0.11576  0.16650 
summary(medcouple(fit$transformation))
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -0.092838 -0.030278 -0.007856 -0.002970  0.030357  0.086419 
# new spectra, with the transformation of the first 300
dim(predict(fit, mg[301:368, ]))
#> [1] 68 23
```
