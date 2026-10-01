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
data(forageLIBS)
# the K I 769.90 nm line, normalized to the C I 247.86 nm line of the matrix
lines <- line_intensities(forageLIBS[-(1:14)], c(C = 247.856, K = 769.896), baseline = TRUE)
standards <- data.frame(
  K = forageLIBS$K,
  signal = lines$intensity[lines$line == "K"] / lines$intensity[lines$line == "C"]
)
cal <- calibration_curve(standards[1:300, ], signal, K)
predict(cal, standards$signal[301:305])
#> # A tibble: 5 × 7
#>   signal concentration    se   lower upper below_lod below_loq
#>    <dbl>         <dbl> <dbl>   <dbl> <dbl> <lgl>     <lgl>    
#> 1  0.459          1.36  1.03 -0.665   3.38 TRUE      TRUE     
#> 2  0.543          1.97  1.03 -0.0538  3.99 TRUE      TRUE     
#> 3  0.580          2.23  1.03  0.212   4.25 TRUE      TRUE     
#> 4  0.492          1.59  1.03 -0.427   3.62 TRUE      TRUE     
#> 5  0.481          1.52  1.03 -0.507   3.54 TRUE      TRUE     
# the signal expected at given contents (% K)
predict(cal, c(1, 2, 3), type = "signal")
#> # A tibble: 3 × 5
#>   concentration signal    se lower upper
#>           <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1             1  0.409 0.143 0.128 0.690
#> 2             2  0.548 0.142 0.268 0.827
#> 3             3  0.686 0.143 0.405 0.967
predict(cal, c(1, 2, 3), type = "signal", interval = "confidence")
#> # A tibble: 3 × 5
#>   concentration signal      se lower upper
#>           <dbl>  <dbl>   <dbl> <dbl> <dbl>
#> 1             1  0.409 0.0178  0.374 0.444
#> 2             2  0.548 0.00819 0.531 0.564
#> 3             3  0.686 0.0174  0.652 0.720
```
