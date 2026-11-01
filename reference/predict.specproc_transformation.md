# Apply a Robust Transformation to New Data, or Back-Transform

Transforms new data with the \\\lambda\\ (and, with
`standardize = TRUE`, the mean and standard deviation of the inliers)
estimated by
[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md)
or
[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md).
With `inverse = TRUE`, it transforms values back to the original scale,
such as the predictions of a model fitted to the transformed variables.

## Usage

``` r
# S3 method for class 'specproc_transformation'
predict(
  object,
  newdata,
  inverse = FALSE,
  estimate = c("median", "mean"),
  residuals = NULL,
  ...
)
```

## Arguments

- object:

  An object returned by
  [`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md)
  or
  [`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md).

- newdata:

  A numeric vector, matrix or data frame with some or all of the
  variables of the fit: new data, or with `inverse = TRUE`, values on
  the transformed scale.

- inverse:

  A logical value: transform the values back to the original scale
  (`TRUE`), or transform new data (`FALSE`, default).

- estimate:

  With `inverse = TRUE`, the estimate on the original scale: `"median"`
  (default) or `"mean"`, which needs `residuals`.

- residuals:

  With `estimate = "mean"`, the residuals of the model on the
  transformed scale: a numeric vector, used for every variable of
  `newdata`, or a matrix or data frame with a column for each of them,
  matched by name (without column names, in the order of the columns of
  `newdata`). Missing values are ignored.

- ...:

  Not used.

## Value

A tibble with the variables of `newdata`, in the order of the fit.
Missing values stay missing, and with the Box-Cox transformation, new
values that are zero or negative become `NA`.

## Details

`newdata` can hold any of the variables of the fit, matched by name. A
matrix or vector without column names must have all of them, in their
order.

The back-transformation undoes, in reverse order, the standardization,
the transformation and the pre-standardization of each variable. When a
model predicts \\\hat{z}\\ on the transformed scale with symmetric
errors, the back-transformed value \\g^{-1}(\hat{z})\\ estimates the
median of the variable on the original scale (`estimate = "median"`, the
default). It needs no other assumption, and suits the prediction of one
sample.

On the original scale, the mean is higher than the median when \\\lambda
\< 1\\ (the transformation compresses the upper tail), and lower when
\\\lambda \> 1\\. `estimate = "mean"` gives the smearing estimate of
Duan (1983), \$\$\hat{x} = \frac{1}{n} \sum\_{k=1}^{n} g^{-1}(\hat{z} +
e_k),\$\$ where \\e_1, \dots, e_n\\ are the `residuals` of the model on
the transformed scale (preferably cross-validated, since the residuals
of the fit underestimate the errors of prediction). It assumes no
distribution of the errors, only that they do not depend on \\\hat{z}\\.
The mean matters when the predictions are averaged or summed, such as
for the mean content of a lot or a mass balance. It is not robust: an
outlying residual pulls it, so pass the residuals of the inliers of a
robust model (for example, of the spectra that
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
does not flag).

The back-transformation is not defined everywhere, because some
transformations are bounded: the Box-Cox transformation above for
\\\lambda \< 0\\, and the Yeo-Johnson transformation above for \\\lambda
\< 0\\ and below for \\\lambda \> 2\\. Values beyond the bound (with
`estimate = "mean"`, values that pass it with one of the residuals)
become `NA`, with a warning.

## References

- Duan, N. (1983). Smearing estimate: a nonparametric retransformation
  method. Journal of the American Statistical Association,
  78(383):605-610.

## See also

[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md),
[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md)

## Examples

``` r
data(forageLIBS)
contents <- forageLIBS[c("Ca", "K", "P", "Mn")]
fit <- robust_boxcox(contents[1:300, ])
new <- predict(fit, contents[301:368, ])
# back to mg/kg
head(predict(fit, new, inverse = TRUE))
#> # A tibble: 6 × 4
#>      Ca     K     P    Mn
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.431  2.25 0.229  56  
#> 2 0.805  1.91 0.21  112  
#> 3 0.786  2.61 0.312  35.0
#> 4 0.807  2.11 0.232  41  
#> 5 0.919  2.17 0.235 136  
#> 6 0.323  1.61 0.206  49  
# Box-Cox is not defined for zero or negative values
x <- contents[301:302, ]
x$Ca[2] <- 0
predict(fit, x)
#> # A tibble: 2 × 4
#>      Ca      K      P    Mn
#>   <dbl>  <dbl>  <dbl> <dbl>
#> 1 -1.35  0.546 -0.400 0.465
#> 2 NA    -0.139 -0.779 2.27 

# a calibration of the transformed Mn contents on the Mn II line at
# 257.61 nm, back to mg/kg
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
line <- forageLIBS[[which.min(abs(wl - 257.61))]]
fit <- robust_boxcox(forageLIBS[1:300, "Mn", drop = FALSE])
model <- lm(Mn ~ log(line), data = data.frame(Mn = fit$transformation$Mn,
                                               line = line[1:300]))
pred <- data.frame(Mn = predict(model, data.frame(line = line[301:368])))
med <- predict(fit, pred, inverse = TRUE)
avg <- predict(fit, pred, inverse = TRUE, estimate = "mean",
               residuals = residuals(model))
summary(avg$Mn / med$Mn)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.039   1.042   1.044   1.045   1.047   1.060 
# the mean content of the 68 samples
c(median = mean(med$Mn), mean = mean(avg$Mn),
  reference = mean(forageLIBS$Mn[301:368]))
#>    median      mean reference 
#>  50.30665  52.65768  57.33824 
```
