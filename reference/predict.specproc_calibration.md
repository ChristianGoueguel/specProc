# Predict Concentrations from a Calibration Curve

Computes the concentrations of new samples from their signal, by inverse
prediction from a curve fitted with
[`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md).

## Usage

``` r
# S3 method for class 'specproc_calibration'
predict(object, newdata, replicates = 1, level = 0.95, ...)
```

## Arguments

- object:

  A curve fitted with
  [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md).

- newdata:

  The signals of the new samples: a numeric vector, or a data frame with
  the signal column used for the calibration.

- replicates:

  The number of replicate measurements averaged in each signal. Default
  is 1.

- level:

  The confidence level of the intervals. Default is 0.95.

- ...:

  Not used.

## Value

A tibble with the `signal`, the predicted `concentration`, its standard
error `se`, the confidence interval (`lower`, `upper`) and whether it is
below the limit of detection (`below_lod`) or of quantification
(`below_loq`).

## See also

[`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md)
