# Intensities of Emission Lines on a Curve of Growth

Computes the integrated intensities and optical depths of emission lines
of a homogeneous plasma in LTE, given the column density of their
species, by the model of
[`curve_of_growth()`](https://christiangoueguel.com/specProc/reference/curve_of_growth.md),
for simulations and for checking a fit.

## Usage

``` r
curve_of_growth_model(
  lines,
  temperature,
  scale,
  optical_depth,
  units = "energy"
)
```

## Arguments

- lines:

  A data frame with one row per line and columns `species` (such as
  `"K I"`), `intensity` (integrated intensity, corrected for the
  spectral response), `wavelength` (nm), `Aki` (s\\^{-1}\\), `gk`, `Ek`
  and `Ei` (upper and lower level energies, eV) and `stark_width` (the
  Stark full width at half maximum, nm).

- temperature:

  The plasma temperature, in K.

- scale:

  The factor \\F\\ of the instrument and geometry.

- optical_depth:

  The largest optical depth at the line center of the lines of each
  species, which sets its column density: one value, or one per species
  named by species.

- units:

  The units of `intensity`: `"energy"` (default) or `"photons"`, as in
  [`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md).

## Value

A tibble with, for each line, its `wavelength`, `optical_depth`,
self-absorption coefficient `SA`, `intensity` and the intensity of an
optically thin plasma (`thin_intensity`).

## See also

[`curve_of_growth()`](https://christiangoueguel.com/specProc/reference/curve_of_growth.md)

## Examples

``` r
lines <- data.frame(species = "Ca I", wavelength = c(400, 520), Aki = c(1.5e8, 3e7),
                    gk = c(4, 6), Ei = c(0, 2), stark_width = 0.02)
lines$Ek <- lines$Ei + 1239.842 / lines$wavelength
curve_of_growth_model(lines, temperature = 9000, scale = 1, optical_depth = 5)
#> # A tibble: 2 × 5
#>   wavelength optical_depth    SA intensity thin_intensity
#>        <dbl>         <dbl> <dbl>     <dbl>          <dbl>
#> 1        400         5     0.472  1.40e-16       2.96e-16
#> 2        520         0.310 0.927  1.21e-17       1.30e-17
```
