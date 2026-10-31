# Plot a Control Chart

Draws a control chart returned by
[`control_chart()`](https://christiangoueguel.com/specProc/reference/control_chart.md):
the monitored statistic in time order, the center line, the control
limits and the signals, with the end of the baseline marked.

## Usage

``` r
plot_control_chart(
  object,
  title = NULL,
  xlab = "Order",
  ylab = NULL,
  base_size = 11,
  point_size = 1.3,
  point_alpha = 1
)
```

## Arguments

- object:

  The result of
  [`control_chart()`](https://christiangoueguel.com/specProc/reference/control_chart.md).

- title:

  The plot title. By default, the method and the number of signals.

- xlab, ylab:

  The axis titles. Default are `"Order"` and the name of the statistic.

- base_size:

  The size of the text, in points. Default is 11.

- point_size:

  The size of the points. Default is 1.3.

- point_alpha:

  The opacity of the points, between 0 and 1. Default is 1.

## Value

A ggplot object.

## See also

[`control_chart()`](https://christiangoueguel.com/specProc/reference/control_chart.md)

## Examples

``` r
set.seed(1)
ratio <- 1 + c(rep(0, 60), seq(0, 0.06, length.out = 40)) + rnorm(100, sd = 0.02)
plot_control_chart(control_chart(ratio, method = "cusum", baseline = 40))
```
