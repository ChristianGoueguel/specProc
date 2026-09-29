# Predict from a Calibration Curve

Computes the concentrations of new samples from their signal, by inverse
prediction from a curve fitted with
[`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md),
or the expected signal at given concentrations, with confidence or
prediction intervals.

## Usage

``` r
# S3 method for class 'specproc_calibration'
predict(
  object,
  newdata,
  replicates = 1,
  level = 0.95,
  type = "concentration",
  interval = "prediction",
  ...
)
```

## Arguments

- object:

  A curve fitted with
  [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md).

- newdata:

  For `type = "concentration"`, the signals of the new samples: a
  numeric vector, or a data frame with the signal column used for the
  calibration. For `type = "signal"`, their concentrations: a numeric
  vector or a data frame with the concentration column.

- replicates:

  The number of replicate measurements averaged in each signal. Default
  is 1.

- level:

  The confidence level of the intervals. Default is 0.95.

- type:

  `"concentration"` (default, inverse prediction) or `"signal"`.

- interval:

  For `type = "signal"`: `"prediction"` (default) or `"confidence"`.

- ...:

  Not used.

## Value

For `type = "concentration"`, a tibble with the `signal`, the predicted
`concentration`, its standard error `se`, the interval (`lower`,
`upper`) and whether it is below the limit of detection (`below_lod`) or
of quantification (`below_loq`). For `type = "signal"`, a tibble with
the `concentration`, the predicted `signal`, its standard error `se` and
the interval (`lower`, `upper`).

## Details

With `type = "concentration"` (default), the interval of each
concentration combines the uncertainty of the curve with the noise of
the new signal, the mean of `replicates` measurements (see
[`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md)).

With `type = "signal"`, `newdata` holds concentrations, and the interval
is either the confidence interval of the mean signal
(`interval = "confidence"`) or the prediction interval of a new signal,
the mean of `replicates` measurements (`interval = "prediction"`,
default). For a weighted curve, the noise of a new signal follows the
weights at its concentration (numeric weights give new samples a weight
of 1).

## See also

[`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md),
[`plot_calibration()`](https://christiangoueguel.com/specProc/reference/plot_calibration.md)

## Examples

``` r
set.seed(1)
standards <- data.frame(concentration = rep(c(0, 0.5, 1, 2, 4, 8), each = 3))
standards$intensity <- 50 + 1000 * standards$concentration + rnorm(18, sd = 30)
cal <- calibration_curve(standards, intensity, concentration)
predict(cal, c(800, 3000), replicates = 3)
#> # A tibble: 2 × 7
#>   signal concentration     se lower upper below_lod below_loq
#>    <dbl>         <dbl>  <dbl> <dbl> <dbl> <lgl>     <lgl>    
#> 1    800         0.746 0.0188 0.706 0.786 FALSE     FALSE    
#> 2   3000         2.95  0.0183 2.91  2.98  FALSE     FALSE    
predict(cal, c(1, 5), type = "signal")
#> # A tibble: 2 × 5
#>   concentration signal    se lower upper
#>           <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1             1  1054.  30.3  990. 1118.
#> 2             5  5054.  30.7 4989. 5119.
predict(cal, c(1, 5), type = "signal", interval = "confidence")
#> # A tibble: 2 × 5
#>   concentration signal    se lower upper
#>           <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1             1  1054.  7.96 1037. 1071.
#> 2             5  5054.  9.19 5034. 5073.
```
