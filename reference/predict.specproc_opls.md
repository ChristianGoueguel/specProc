# Predict with an OPLS Model

Applies an OPLS model fitted by
[`opls()`](https://christiangoueguel.com/specProc/reference/opls.md) to
new data: removes the orthogonal components (the OPLS filter), or
predicts the response or the scores.

## Usage

``` r
# S3 method for class 'specproc_opls'
predict(object, newdata, type = c("correction", "response", "scores"), ...)
```

## Arguments

- object:

  A model returned by
  [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md).

- newdata:

  A numeric matrix or data frame of new data, with the same variables as
  the calibration data. If both have column names, the columns of
  `newdata` are matched by name.

- type:

  The prediction: `"correction"` (default) the filtered data, like the
  `correction` component of the model; `"response"` the predicted
  response; `"scores"` the predictive (`p1`) and orthogonal (`o1`, ...)
  scores.

- ...:

  Not used.

## Value

A tibble of filtered data or of scores, or a numeric vector of predicted
responses.

## Details

The new data are preprocessed with the centers and scales of the
calibration data, and the orthogonal components are removed one at a
time (\\\textbf{t}\_o = \textbf{X}\textbf{w}\_o\\, \\\textbf{X}
\leftarrow \textbf{X} - \textbf{t}\_o\textbf{p}\_o^T\\). The predicted
response is \\\textbf{X}\textbf{w}c\\, of the filtered data, back in the
units of y.

## See also

[`opls()`](https://christiangoueguel.com/specProc/reference/opls.md),
[`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(40 * 30), 40, 30)
y <- x[, 1] + rnorm(40, sd = 0.1)
fit <- opls(x[1:30, ], y[1:30], ncomp.ortho = 2, permutation = 0)
predict(fit, x[31:40, ], type = "response")
#>  [1]  0.93915990 -0.27818754 -0.82242784  0.07516710 -0.47520406 -1.14038325
#>  [7] -0.46482140  0.08335247  0.27417042  0.98150423
```
