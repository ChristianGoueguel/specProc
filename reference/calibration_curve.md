# Univariate Calibration Curve

Fits a univariate calibration curve (signal against concentration), with
its figures of merit, limits of detection and quantification, and tests
of linearity. The curve predicts the concentration of new samples from
their signal, with confidence intervals.

## Usage

``` r
calibration_curve(
  data,
  signal,
  concentration,
  model = "linear",
  weights = NULL,
  blank = NULL,
  lod_method = NULL,
  level = 0.95
)
```

## Arguments

- data:

  A data frame with the calibration samples.

- signal, concentration:

  The columns of `data` holding the signal and the reference
  concentration, unquoted or as strings.

- model:

  `"linear"` (default) or `"quadratic"`.

- weights:

  `NULL` (default, unweighted), `"1/x"`, `"1/x2"`, or a numeric vector
  of weights, one per row of `data`.

- blank:

  An optional numeric vector of replicate signals of a blank sample.

- lod_method:

  The estimate of the blank standard deviation: `"residual"`,
  `"intercept"` or `"blank"` (see details). By default, `"blank"` when
  `blank` is given, `"residual"` otherwise.

- level:

  The confidence level of the intervals of the coefficients. Default is
  0.95.

## Value

An object of class `specproc_calibration`, a list with

- `coefficients`: a tibble of the estimates, standard errors and
  confidence intervals (`lower`, `upper`) at `level`;

- `figures_of_merit`: a tibble with the number of standards `n`, the
  `sensitivity`, `r_squared`, the residual standard deviation `sigma`,
  the blank standard deviation used (`sigma_blank`), `lod` and `loq` (in
  concentration units);

- `linearity`: a tibble with the statistic, degrees of freedom and
  p-value of Mandel's test and of the lack-of-fit test (when they
  apply);

- `fit`: the `lm` fit, and `data`, the calibration data.

Use
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_calibration.md)
for new samples and
[`plot_calibration()`](https://christiangoueguel.com/specProc/reference/plot_calibration.md)
to draw the curve.

## Details

The signal, such as a line intensity from
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)
or a line ratio, is modeled as a straight line or a second-degree
polynomial of the concentration, fitted by (weighted) least squares.
Weights `"1/x"` or `"1/x2"` suit signals whose variance grows with
concentration, which is common in LIBS.

**Figures of merit.** The sensitivity is the slope of the curve at zero
concentration. The limits of detection and quantification are \\LOD =
3.3\\\sigma / S\\ and \\LOQ = 10\\\sigma / S\\ (ICH, 2005), where \\S\\
is the sensitivity and \\\sigma\\ the standard deviation of the signal
of a blank, estimated from

- `"residual"`: the residual standard deviation of the fit (default
  without `blank`);

- `"intercept"`: the standard error of the intercept;

- `"blank"`: the standard deviation of replicate `blank` signals
  (default when given).

With weights, the residual standard deviation depends on the scale of
the weights, so prefer `"blank"` or `"intercept"`.

**Linearity.** For a straight line, Mandel's test compares its residual
variance with that of a quadratic fit; a small p-value means that the
curvature is significant (as with self-absorption or detector
saturation). When concentrations are replicated, the lack-of-fit test
compares the residuals with the pure error of the replicates.

**Intervals.** The confidence band of the curve shows where the mean
signal lies at each concentration; the prediction band, wider, where a
single new measurement is expected to fall, since it adds the noise of
the measurement.
[`plot_calibration()`](https://christiangoueguel.com/specProc/reference/plot_calibration.md)
draws either or both, and
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_calibration.md)
computes them for given concentrations (`type = "signal"`).

**Inverse prediction.** The concentration of a sample is the solution of
the calibration equation for its signal (for a quadratic curve, the root
within or nearest to the calibration range). Its standard error, by the
delta method, combines the uncertainty of the curve with that of the new
signal, the mean of `replicates` measurements.

## References

- ICH (2005). Validation of analytical procedures: text and methodology
  Q2(R1). International Conference on Harmonisation.

- Mandel, J. (1964). The Statistical Analysis of Experimental Data.
  Interscience, New York.

- Miller, J.N., Miller, J.C. (2018). Statistics and Chemometrics for
  Analytical Chemistry, 7th ed. Pearson, Harlow.

## See also

[`predict.specproc_calibration()`](https://christiangoueguel.com/specProc/reference/predict.specproc_calibration.md),
[`plot_calibration()`](https://christiangoueguel.com/specProc/reference/plot_calibration.md),
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md),
[`nas()`](https://christiangoueguel.com/specProc/reference/nas.md)

## Author

Christian L. Goueguel

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
cal
#> Linear calibration curve (300 standards)
#> 
#>       term estimate std_error  lower  upper
#>  intercept   0.2708   0.03240 0.2070 0.3345
#>      slope   0.1384   0.01556 0.1078 0.1690
#> 
#> R-squared:    0.20976
#> Sensitivity:  0.1384
#> LOD:          3.38  (residual)
#> LOQ:          10.2
#> Mandel:        F = 4.03, p = 0.0457
#> Lack of fit:   F = 1.13, p = 0.239
# the concentrations of new samples, from their signals
head(predict(cal, standards$signal[301:368]))
#> # A tibble: 6 × 7
#>   signal concentration    se   lower upper below_lod below_loq
#>    <dbl>         <dbl> <dbl>   <dbl> <dbl> <lgl>     <lgl>    
#> 1  0.459          1.36  1.03 -0.665   3.38 TRUE      TRUE     
#> 2  0.543          1.97  1.03 -0.0538  3.99 TRUE      TRUE     
#> 3  0.580          2.23  1.03  0.212   4.25 TRUE      TRUE     
#> 4  0.492          1.59  1.03 -0.427   3.62 TRUE      TRUE     
#> 5  0.481          1.52  1.03 -0.507   3.54 TRUE      TRUE     
#> 6  0.501          1.67  1.03 -0.355   3.69 TRUE      TRUE     
```
