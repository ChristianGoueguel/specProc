# Influence Plot of a PCA Model

Plots the Q residual of each sample against its Hotelling's \\T^2\\,
with their 95% (dashed) and 99% (solid) limits, from
[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md).
The samples beyond a 99% limit are colored by type and labeled.

## Usage

``` r
plot_influence(x, label = NULL, log = FALSE, title = NULL)
```

## Arguments

- x:

  The result of
  [`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md).

- label:

  The labels of the samples: a vector with one value per sample. By
  default, their row numbers.

- log:

  A logical: logarithmic axes (`FALSE`, default), useful when a few
  samples are far beyond the limits.

- title:

  The plot title.

## Value

A ggplot object.

## See also

[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md),
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
