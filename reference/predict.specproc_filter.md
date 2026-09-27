# Apply an Orthogonalization Filter to New Spectra

Corrects new spectra with a filter estimated by
[`epo()`](https://christiangoueguel.com/specProc/reference/epo.md),
[`osc()`](https://christiangoueguel.com/specProc/reference/osc.md),
[`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md),
[`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md),
[`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
or
[`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md).
The new spectra are preprocessed with the centers and scales of the
calibration data, and the components estimated on the calibration data
are removed. Estimating the filter on calibration data and applying it
to validation data with
[`predict()`](https://rdrr.io/r/stats/predict.html) avoids the
optimistic bias that arises when a supervised filter is fitted to all
the data before validation.

## Usage

``` r
# S3 method for class 'specproc_epo'
predict(object, newdata, ...)

# S3 method for class 'specproc_osc'
predict(object, newdata, ...)

# S3 method for class 'specproc_direct_orthogonal'
predict(object, newdata, ...)

# S3 method for class 'specproc_direct_osc'
predict(object, newdata, ...)

# S3 method for class 'specproc_projected_osc'
predict(object, newdata, ...)

# S3 method for class 'o2pls'
predict(object, newdata, ...)
```

## Arguments

- object:

  A filter returned by
  [`epo()`](https://christiangoueguel.com/specProc/reference/epo.md),
  [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md),
  [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md),
  [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md),
  [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
  or
  [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md).

- newdata:

  A numeric matrix or data frame of new spectra, with the same variables
  as the calibration data. If both have column names, the columns of
  `newdata` are matched by name.

- ...:

  Not used.

## Value

A tibble of corrected spectra, with the same columns as `newdata`.

## Details

For [`epo()`](https://christiangoueguel.com/specProc/reference/epo.md)
the spectra are not centered, and the correction is
\\\textbf{X}\_{new} - \textbf{X}\_{new}\textbf{VV}^T\\. For the other
methods, the corrected spectra are centered (and scaled, if
`scale = TRUE` was used), like the `correction` component of the fitted
object:

- [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md)
  with `method = "wold"` or `"sjoblom"`, and
  [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md),
  remove the orthogonal components one at a time, because their weights
  refer to the deflated matrix.

- [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md)
  with `method = "fearn"`,
  [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md),
  [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md)
  and
  [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
  remove all components at once: \\\textbf{X}\_{new} -
  \textbf{X}\_{new}\textbf{WP}^T\\.

Applied to the calibration spectra,
[`predict()`](https://rdrr.io/r/stats/predict.html) returns the
`correction` component of the fitted object.

## See also

The recipe steps
[`step_osc()`](https://christiangoueguel.com/specProc/reference/step_osc.md)
and related functions, which apply these filters within a tidymodels
workflow.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(40 * 30), 40, 30)
y <- x[, 1] + rnorm(40, sd = 0.1)
cal <- 1:30

fit <- osc(x[cal, ], y[cal], method = "fearn", ncomp = 2)
corrected <- predict(fit, x[-cal, ])
dim(corrected)
#> [1] 10 30

# applied to the calibration spectra, predict() gives the correction
all.equal(predict(fit, x[cal, ]), fit$correction)
#> [1] TRUE
```
