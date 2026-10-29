# Tidy and Glance Methods for Fits, Calibrations and Plasma Diagnostics

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) summarizes
the estimates of a fit, a calibration or a plasma diagnostic in a
tibble, one row per estimate, and
[`glance()`](https://generics.r-lib.org/reference/glance.html)
summarizes the fit in a one-row tibble, following the conventions of the
broom package. The tibbles of several spectra or samples can then be
bound together.

## Usage

``` r
# S3 method for class 'specproc_peak_fit'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'specproc_peak_fit'
glance(x, ...)

# S3 method for class 'specproc_calibration'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'specproc_calibration'
glance(x, ...)

# S3 method for class 'specproc_wavelength_calibration'
tidy(x, ...)

# S3 method for class 'specproc_wavelength_calibration'
glance(x, ...)

# S3 method for class 'specproc_slope_bias'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'specproc_slope_bias'
glance(x, ...)

# S3 method for class 'specproc_boltzmann'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'specproc_boltzmann'
glance(x, ...)

# S3 method for class 'specproc_cflibs'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'specproc_cflibs'
glance(x, ...)

# S3 method for class 'specproc_uncertainty'
tidy(x, ...)

# S3 method for class 'specproc_uncertainty'
glance(x, ...)

# S3 method for class 'specproc_curve_of_growth'
tidy(x, ...)

# S3 method for class 'specproc_curve_of_growth'
glance(x, ...)
```

## Arguments

- x:

  A peak fit, or an object returned by
  [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md),
  [`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md),
  [`slope_bias()`](https://christiangoueguel.com/specProc/reference/slope_bias.md),
  [`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md),
  [`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md),
  [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md),
  [`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md)
  or
  [`curve_of_growth()`](https://christiangoueguel.com/specProc/reference/curve_of_growth.md).

- conf.int:

  A logical: add the confidence interval (`FALSE`, default).

- conf.level:

  The confidence level of the interval. Default is 0.95.

- ...:

  Not used.

## Value

A tibble.

## Details

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns the
columns `term`, `estimate` and `std.error`, with the `statistic` and
`p.value` of a t test where one applies, and, with `conf.int = TRUE`,
the bounds `conf.low` and `conf.high` of the confidence interval at
`conf.level`:

- a peak fit (an element of the `fit` column of
  [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)
  or
  [`multipeak_fit()`](https://christiangoueguel.com/specProc/reference/multipeak_fit.md)):
  the parameters of each line and the common shifts, as in `tidied`; the
  test is that of a zero value;

- [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md):
  the `intercept`, `slope` and `quadratic` term, with the test of a zero
  value;

- [`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md):
  the coefficients of the polynomial correction of each `segment`, in
  powers of the wavelength minus the `center` of the segment (nm),
  without standard errors;

- [`slope_bias()`](https://christiangoueguel.com/specProc/reference/slope_bias.md):
  the `bias` and `slope`, with the test of no correction (a bias of 0, a
  slope of 1);

- [`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)
  and
  [`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md):
  the `temperature` (K) and the `intercept` of the plot; the interval of
  the temperature is that of the slope, transformed, so it is not
  symmetric (its upper bound is infinite when the interval of the slope
  reaches zero);

- [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md):
  the `temperature` (with an interval as above), and the
  `atomic_fraction` and `mass_fraction` of each `element`, without
  standard errors (see
  [`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md));

- [`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md):
  the same quantities with their Monte Carlo standard uncertainties, and
  always their intervals, at the `level` of the object;

- [`curve_of_growth()`](https://christiangoueguel.com/specProc/reference/curve_of_growth.md):
  the column density of each species (`term`), with its number of lines
  (`n.lines`) and its largest optical depth (`max.optical.depth`).

[`glance()`](https://generics.r-lib.org/reference/glance.html) returns:

- a peak fit: `sigma`, the root mean square error `rmse`, `isConv`,
  `finTol`, `logLik`, `AIC`, `BIC`, `deviance`, `df.residual` and
  `nobs`, as for `nls` fits in broom (with `rmse`);

- [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md):
  `nobs`, `r.squared`, `sigma`, `df.residual`, the `sensitivity`, the
  `critical.value` (ISO 11843 only), `lod`, `loq`, the reduced
  `chi.squared` of a Deming or York fit, the p-values of Mandel's test
  (`mandel.p.value`) and of the lack-of-fit test
  (`lack.of.fit.p.value`), the `regression` and the `model`;

- [`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md):
  the number of reference lines `n.lines`, of lines used `nobs`, of
  `n.segments`, the `degree`, and the `rmse` and largest absolute
  residual `max.residual` of the lines used (nm);

- [`slope_bias()`](https://christiangoueguel.com/specProc/reference/slope_bias.md):
  `nobs`, the root mean square differences to the reference before
  (`rmse.before`) and after (`rmse.after`) the correction, and whether
  the slope was fitted (`slope.fitted`);

- [`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)
  and
  [`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md):
  `r.squared`, `sigma`, `nobs`, `df.residual`, the range of the abscissa
  `energy.range` (eV), the `electron.density` and number of `iterations`
  (Saha-Boltzmann only), the `method` and the `units`;

- [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md):
  `r.squared` (about the mean), `sigma`, `nobs`, `df.residual` (`NA`
  when the temperature is given), the numbers of `n.species` and
  `n.elements`, the `electron.density`, the number of `iterations`, the
  `method` and the `units`;

- [`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md):
  the number of draws `n.draws`, of `failed` draws, the reduced
  `chi.squared` of the fit, the `level` and the `method`;

- [`curve_of_growth()`](https://christiangoueguel.com/specProc/reference/curve_of_growth.md):
  the `temperature`, the `rmse` of the fit (on the log scale), `nobs`,
  the `scale` and the `units`.

## See also

[specproc-tidy-models](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
for the chemometric models,
[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md),
[`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md),
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md),
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md),
[`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
mean_spectrum <- colMeans(forageLIBS[-(1:14)])
atomic <- data.frame(
  species = c(rep("Ca I", 6), rep("Ca II", 4)),
  stage = c(rep(1, 6), rep(2, 4)),
  wavelength = c(428.301, 430.253, 431.865, 445.478, 612.222, 616.217,
                 315.887, 317.933, 370.603, 373.690),
  Aki = c(4.34e7, 1.36e8, 7.40e7, 8.70e7, 2.87e7, 4.77e7, 3.10e8, 3.60e8, 8.80e7, 1.70e8),
  gk = c(5, 5, 3, 7, 3, 3, 4, 6, 2, 2),
  Ek = c(4.780, 4.780, 4.769, 4.681, 3.910, 3.910, 7.047, 7.050, 6.468, 6.468)
)
lines <- line_intensities(mean_spectrum, atomic, baseline = TRUE)
fit <- saha_boltzmann(lines, ionization_energy = 6.113, electron_density = 1.9e17)
tidy(fit, conf.int = TRUE)
#> # A tibble: 2 × 5
#>   term        estimate std.error conf.low conf.high
#>   <chr>          <dbl>     <dbl>    <dbl>     <dbl>
#> 1 temperature 8092.      124.    7815.    8389.    
#> 2 intercept     -0.500     0.195   -0.950   -0.0505
glance(fit)
#> # A tibble: 1 × 9
#>   r.squared sigma  nobs df.residual energy.range electron.density iterations
#>       <dbl> <dbl> <int>       <int>        <dbl>            <dbl>      <int>
#> 1     0.998 0.288    10           8         9.25           1.9e17         10
#> # ℹ 2 more variables: method <chr>, units <chr>

# the K I 769.90 nm line, normalized to the C I 247.86 nm line of the matrix
k <- line_intensities(forageLIBS[-(1:14)], c(C = 247.856, K = 769.896), baseline = TRUE)
standards <- data.frame(K = forageLIBS$K,
                        signal = k$intensity[k$line == "K"] / k$intensity[k$line == "C"])
cal <- calibration_curve(standards, signal, K)
tidy(cal)
#> # A tibble: 2 × 5
#>   term      estimate std.error statistic  p.value
#>   <chr>        <dbl>     <dbl>     <dbl>    <dbl>
#> 1 intercept    0.257    0.0280      9.17 3.58e-18
#> 2 slope        0.144    0.0132     10.9  3.16e-24
glance(cal)
#> # A tibble: 1 × 13
#>    nobs r.squared sigma df.residual sensitivity critical.value   lod   loq
#>   <int>     <dbl> <dbl>       <int>       <dbl>          <dbl> <dbl> <dbl>
#> 1   368     0.246 0.135         366       0.144             NA  3.09  9.36
#> # ℹ 5 more variables: chi.squared <dbl>, mandel.p.value <dbl>,
#> #   lack.of.fit.p.value <dbl>, regression <chr>, model <chr>
```
