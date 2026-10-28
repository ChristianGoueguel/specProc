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
  level = 0.95,
  regression = "ols",
  concentration_se = NULL,
  signal_se = NULL,
  variance_ratio = NULL,
  lod_convention = "ich",
  alpha = 0.05,
  beta = 0.05,
  replicates = 1
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

- regression:

  `"ols"` (default, least squares), `"deming"` or `"york"` (see
  details).

- concentration_se, signal_se:

  For `regression = "york"` or `"deming"`: the columns of `data` with
  the standard uncertainties of the concentration and of the signal of
  each standard, unquoted or as strings.

- variance_ratio:

  For `regression = "deming"`: the ratio of the error variance of the
  signal to that of the concentration. By default, it is computed from
  `signal_se` and `concentration_se`.

- lod_convention:

  The convention of the limits of detection and quantification: `"ich"`
  (default, \\3.3\\\sigma/S\\ and \\10\\\sigma/S\\) or `"iso11843"` (ISO
  11843-2 and IUPAC; see details).

- alpha, beta:

  For `lod_convention = "iso11843"`: the probabilities of false
  positives and of false negatives. Default is 0.05.

- replicates:

  For `lod_convention = "iso11843"`: the number of replicate
  measurements averaged for a sample. Default is 1.

## Value

An object of class `specproc_calibration`, a list with

- `coefficients`: a tibble of the estimates, standard errors and
  confidence intervals (`lower`, `upper`) at `level`;

- `figures_of_merit`: a tibble with the number of standards `n`, the
  `sensitivity`, `r_squared`, the residual standard deviation `sigma`,
  the blank standard deviation used (`sigma_blank`), the
  `critical_value` (ISO 11843 only, `NA` otherwise), `lod` and `loq` (in
  concentration units);

- `linearity`: a tibble with the statistic, degrees of freedom and
  p-value of Mandel's test and of the lack-of-fit test (when they
  apply);

- `fit`: the least-squares `lm` fit, `eiv`, the Deming or York fit
  (`NULL` for least squares), with its `chi_squared`, and `data`, the
  calibration data.

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

With `lod_convention = "iso11843"`, the limits follow ISO 11843-2 and
IUPAC (Currie, 1999), and account for the uncertainty of the curve and
for the false positive and false negative risks \\\alpha\\ and
\\\beta\\. The standard deviation of a net signal near zero is
\\\sigma_0 = \sqrt{\sigma^2 / K + s_a^2}\\, where \\\sigma\\ is the
standard deviation of the signal (`"residual"` or `"blank"`), \\K\\ the
number of `replicates` averaged for a sample and \\s_a\\ the standard
error of the intercept. The critical value (decision limit), above which
an element is detected, is \\x_C = t\_{1-\alpha,\nu}\\\sigma_0 / S\\;
the limit of detection, the content detected with probability \\1 -
\beta\\, is \\x_D = \delta\_{\nu,\alpha,\beta}\\\sigma_0 / S\\, where
\\\delta\\ is the noncentrality parameter of the noncentral t
distribution (about \\2 t\_{1-\alpha,\nu}\\ for \\\alpha = \beta\\) and
\\\nu\\ the degrees of freedom of \\\sigma\\; and the limit of
quantification (IUPAC) is \\x_Q = 10\\\sigma_0 / S\\. For a weighted
curve, the standard deviation must come from `blank` signals.

**Errors in the concentrations.** Least squares assume exact
concentrations. When those of the standards are uncertain, such as the
certified values of reference materials, the slope is biased toward zero
and the intervals too narrow. `regression = "york"` fits the line by the
method of York et al. (2004), with the standard uncertainties of the
concentration (`concentration_se`) and of the signal (`signal_se`) of
each standard; its standard errors are multiplied by the square root of
the reduced chi-squared when it exceeds 1 (the points scatter more than
their uncertainties explain). `regression = "deming"` (Deming
regression) assumes constant uncertainties, whose ratio `variance_ratio`
(variance of the signal over variance of the concentration, in their
units) is given, or computed from the mean squared `signal_se` and
`concentration_se`; its standard errors come from the scatter of the
points. Both fit straight lines without weights; the tests of linearity
are those of the least-squares fit.

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

- Currie, L.A. (1999). Detection and quantification limits: origins and
  historical overview. Analytica Chimica Acta, 391(2):127-134.

- ICH (2005). Validation of analytical procedures: text and methodology
  Q2(R1). International Conference on Harmonisation.

- ISO 11843-2 (2000). Capability of detection, Part 2: Methodology in
  the linear calibration case. International Organization for
  Standardization, Geneva.

- Mandel, J. (1964). The Statistical Analysis of Experimental Data.
  Interscience, New York.

- Miller, J.N., Miller, J.C. (2018). Statistics and Chemometrics for
  Analytical Chemistry, 7th ed. Pearson, Harlow.

- York, D., Evensen, N.M., López Martínez, M., De Basabe Delgado, J.
  (2004). Unified equations for the slope, intercept, and standard
  errors of the best straight line. American Journal of Physics,
  72(3):367-375.

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
#> # A tibble: 6 × 8
#>   signal concentration    se   lower upper below_lod below_loq outside_range
#>    <dbl>         <dbl> <dbl>   <dbl> <dbl> <lgl>     <lgl>     <lgl>        
#> 1  0.459          1.36  1.03 -0.665   3.38 TRUE      TRUE      FALSE        
#> 2  0.543          1.97  1.03 -0.0538  3.99 TRUE      TRUE      FALSE        
#> 3  0.580          2.23  1.03  0.212   4.25 TRUE      TRUE      FALSE        
#> 4  0.492          1.59  1.03 -0.427   3.62 TRUE      TRUE      FALSE        
#> 5  0.481          1.52  1.03 -0.507   3.54 TRUE      TRUE      FALSE        
#> 6  0.501          1.67  1.03 -0.355   3.69 TRUE      TRUE      FALSE        

# limits of detection by ISO 11843-2
calibration_curve(standards[1:300, ], signal, K, lod_convention = "iso11843")
#> Linear calibration curve (300 standards)
#> 
#>       term estimate std_error  lower  upper
#>  intercept   0.2708   0.03240 0.2070 0.3345
#>      slope   0.1384   0.01556 0.1078 0.1690
#> 
#> R-squared:    0.20976
#> Sensitivity:  0.1384
#> Critical:     1.73  (ISO 11843, alpha = 0.05)
#> LOD:          3.47  (beta = 0.05; residual)
#> LOQ:          10.5  (IUPAC, 10 sigma)
#> Mandel:        F = 4.03, p = 0.0457
#> Lack of fit:   F = 1.13, p = 0.239

# reference materials with certified values known within 3%, and
# signals within 2% (simulated)
set.seed(1)
crm <- data.frame(certified = c(0.5, 1, 2, 4, 6, 8))
crm$signal <- 0.05 + 0.2 * crm$certified * (1 + rnorm(6, sd = 0.02)) +
  0.2 * rnorm(6, sd = 0.03 * crm$certified)
crm$certified_se <- 0.03 * crm$certified
crm$signal_se <- 0.02 * crm$signal
calibration_curve(crm, signal, certified, regression = "york",
                  concentration_se = certified_se, signal_se = signal_se)
#> Linear calibration curve (6 standards, York regression)
#> 
#>       term estimate std_error   lower  upper
#>  intercept  0.04832  0.005072 0.03424 0.0624
#>      slope  0.20442  0.004422 0.19214 0.2167
#> 
#> R-squared:    0.99835
#> Chi-squared:  0.372 (reduced)
#> Sensitivity:  0.2044
#> LOD:          0.454  (residual)
#> LOQ:          1.38
#> Mandel:        F = 1.86, p = 0.266
```
