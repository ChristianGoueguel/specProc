# Monte Carlo Uncertainty of Plasma Diagnostics

Propagates the uncertainties of the line intensities, of the transition
probabilities and of the electron density to the temperature of a
Boltzmann or Saha-Boltzmann plot, or to the temperature and the
composition of a calibration-free (CF-LIBS) analysis, by Monte Carlo
simulation.

## Usage

``` r
plasma_uncertainty(
  object,
  n = 1000,
  level = 0.95,
  aki_rsd = NULL,
  electron_density_se = 0,
  temperature_se = 0
)
```

## Arguments

- object:

  A fit returned by
  [`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md),
  [`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md)
  or
  [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md).

- n:

  The number of draws. Default is 1000.

- level:

  The confidence level of the intervals. Default is 0.95.

- aki_rsd:

  The relative standard uncertainties of the transition probabilities,
  such as 0.1 for 10%: one value, or one per line. By default, they are
  taken from the column `accuracy` of the lines.

- electron_density_se:

  The standard uncertainty of the electron density of the fit, in
  cm\\^{-3}\\. Default is 0.

- temperature_se:

  For a
  [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md)
  fit given the temperature: the standard uncertainty of the
  temperature, in K. Default is 0.

## Value

An object of class `specproc_uncertainty`, a list with

- `estimates`: a tibble with one row per quantity: the `quantity`
  (`"temperature"`, `"atomic_fraction"` or `"mass_fraction"`), the
  `element` (CF-LIBS), the `estimate` of the fit, the standard
  uncertainty `se` (the standard deviation of the draws), and the
  `lower` and `upper` bounds of the interval;

- `draws`: a tibble of the draws, with the `draw` number, the
  `temperature` and, for CF-LIBS, the `element`, `atomic_fraction` and
  `mass_fraction`;

- `inputs`: a tibble with the relative standard uncertainties of the
  intensity (`intensity_rsd`) and of the transition probability
  (`aki_rsd`) of each line;

- `electron_density_rsd` and `temperature_rsd`: the relative standard
  uncertainties of the electron density and of a given temperature
  (`NULL` when not used by the fit);

- `chi_squared`: the reduced chi-squared of the fit (`NA` when the
  temperature is given, or when a point has no stated uncertainty);

- `temperature_se`: the standard error of the temperature of the fit;

- `n`, `level`, the number of `failed` draws and the `method`.

## Details

Each of the `n` draws perturbs the inputs of the fit within their
uncertainties and repeats the fit. The standard deviation of the results
is their standard uncertainty, and their quantiles give the interval at
`level`. The perturbations are lognormal (multiplicative), with the
relative standard uncertainties of the inputs, so that intensities,
transition probabilities and densities stay positive. The sources are:

- the intensities: the column `intensity_se` of the lines (standard
  errors, in the units of `intensity`), as returned by
  [`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md);

- the transition probabilities: `aki_rsd`, or else the accuracy grades
  of the NIST Atomic Spectra Database in the column `accuracy` (see
  [`aki_uncertainty()`](https://christiangoueguel.com/specProc/reference/aki_uncertainty.md));

- the electron density, for Saha-Boltzmann plots and CF-LIBS:
  `electron_density_se`, for example from
  [`electron_density_uncertainty()`](https://christiangoueguel.com/specProc/reference/electron_density_uncertainty.md);

- the temperature, for a
  [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md)
  fit given the temperature: `temperature_se`.

A source that is not given is taken as exact. The errors of different
lines are taken as independent, and the partition functions and the
ionization energies as exact. Lines corrected by
[`correct_self_absorption()`](https://christiangoueguel.com/specProc/reference/correct_self_absorption.md)
are not independent: their corrected intensities are computed from their
reference line.

The interval reflects the stated uncertainties of the inputs, whereas
the standard error of the fit (`temperature_se` of the fit) reflects the
scatter of the points about the fitted line. The reduced chi-squared
compares the two: \\\chi^2\_\nu = \sum_i (r_i / \sigma_i)^2 / \nu\\,
where \\r_i\\ are the residuals of the fit, \\\sigma_i\\ the standard
uncertainties of the ordinates of the points (from those of the
intensities and of the transition probabilities) and \\\nu\\ the
residual degrees of freedom. Near 1, the stated uncertainties explain
the scatter. Well above 1, other errors scatter the points
(self-absorption, spectral response, departures from LTE, or understated
uncertainties) and the interval is too narrow. Well below 1, the
uncertainties are overstated; the NIST grades are upper bounds.

A draw whose Boltzmann plot has a non-negative slope has no finite
temperature: it counts as an infinite temperature in the interval, whose
upper bound can then be infinite, and is left out of the standard
deviation. A CF-LIBS draw that fails is left out, with a warning.

Use [`set.seed()`](https://rdrr.io/r/base/Random.html) for reproducible
results.

## References

- JCGM 101:2008. Evaluation of measurement data - Supplement 1 to the
  "Guide to the expression of uncertainty in measurement" - Propagation
  of distributions using a Monte Carlo method. Joint Committee for
  Guides in Metrology.

## See also

[`aki_uncertainty()`](https://christiangoueguel.com/specProc/reference/aki_uncertainty.md),
[`electron_density_uncertainty()`](https://christiangoueguel.com/specProc/reference/electron_density_uncertainty.md),
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md),
[`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md),
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md),
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
mean_spectrum <- colMeans(forageLIBS[-(1:14)])
# calcium lines, with their atomic data and accuracy grades from the NIST
# database
atomic <- data.frame(
  species = c(rep("Ca I", 6), rep("Ca II", 4)),
  stage = c(rep(1, 6), rep(2, 4)),
  wavelength = c(428.301, 430.253, 431.865, 445.478, 612.222, 616.217,
                 315.887, 317.933, 370.603, 373.690),
  Aki = c(4.34e7, 1.36e8, 7.40e7, 8.70e7, 2.87e7, 4.77e7, 3.10e8, 3.60e8, 8.80e7, 1.70e8),
  accuracy = c("C+", "C+", "C+", "C", "C", "C", "C", "C", "C", "C"),
  gk = c(5, 5, 3, 7, 3, 3, 4, 6, 2, 2),
  Ek = c(4.780, 4.780, 4.769, 4.681, 3.910, 3.910, 7.047, 7.050, 6.468, 6.468)
)
lines <- line_intensities(mean_spectrum, atomic, baseline = TRUE)
fit <- saha_boltzmann(lines, ionization_energy = 6.113, electron_density = 1.9e17)

# with a 15% standard uncertainty of the electron density
set.seed(1)
u <- plasma_uncertainty(fit, electron_density_se = 0.15 * 1.9e17)
u
#> Monte Carlo uncertainty of a Saha-Boltzmann plot
#> 1000 draws, 95% intervals
#> 
#> Intensities:              0.39% to 4.5%
#> Transition probabilities: 18% to 25%
#> Electron density:         15%
#> Reduced chi-squared:      1.53
#> 
#>  quantity    estimate se  lower upper
#>  temperature 8092     124 7862  8353 
#> 
#> Standard error of the fit (scatter of the points): 124 K
u$estimates
#> # A tibble: 1 × 6
#>   quantity    element estimate    se lower upper
#>   <chr>       <chr>      <dbl> <dbl> <dbl> <dbl>
#> 1 temperature NA         8092.  124. 7862. 8353.
```
