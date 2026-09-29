# Influence Plot of a PCA Model

Plots the residual distance of each sample to a PCA model (Q residual or
DModX) against its Hotelling's \\T^2\\, with their limits at each
confidence level, from
[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md)
or
[`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md).
The samples beyond a limit at the highest confidence level are colored
by type and labeled.

## Usage

``` r
plot_influence(x, label = NULL, log = FALSE, title = NULL)
```

## Arguments

- x:

  The result of
  [`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md)
  or
  [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md).

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

## Details

The limit at the highest confidence level is drawn as a solid line, the
others as dashed, dotted, ... lines.

## See also

[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md),
[`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md),
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
