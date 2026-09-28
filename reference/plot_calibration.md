# Plot a Calibration Curve

Draws the calibration standards, the fitted curve with its confidence
band, and the limits of detection and quantification.

## Usage

``` r
plot_calibration(object, level = 0.95, title = NULL)
```

## Arguments

- object:

  A curve fitted with
  [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md).

- level:

  The confidence level of the band. Default is 0.95.

- title:

  The plot title. By default, the model and the figures of merit.

## Value

A ggplot object.

## See also

[`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md)
