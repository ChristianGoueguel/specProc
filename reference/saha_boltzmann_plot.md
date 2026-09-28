# Plasma Temperature from a Saha-Boltzmann Plot

Estimates the temperature of a plasma in LTE from lines of the neutral
atom and of the singly charged ion of the same element, by a
Saha-Boltzmann plot.

## Usage

``` r
saha_boltzmann_plot(
  lines,
  ionization_energy,
  electron_density,
  units = "energy",
  max_iter = 100,
  tol = 1e-08
)
```

## Arguments

- lines:

  A data frame as for
  [`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md),
  with an additional column `stage`: 1 for lines of the neutral atom, 2
  for lines of the singly charged ion.

- ionization_energy:

  The ionization energy of the neutral atom, in eV.

- electron_density:

  The electron density, in cm\\^{-3}\\, for example from
  [`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md).

- units:

  The units of `intensity`: `"energy"` (default) or `"photons"`.

- max_iter:

  The maximum number of iterations. Default is 100.

- tol:

  The relative tolerance on the temperature. Default is 1e-8.

## Value

An object of class `specproc_boltzmann`, as for
[`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md),
with the number of iterations in `iterations`.

## Details

Lines of the ion are placed on the Boltzmann plot of the neutral atom
with the Saha equation. For ionic lines, the abscissa becomes \\E_k +
E\_{ion}\\ and the ordinate \$\$y^\* = \ln\frac{I\lambda}{g_k A\_{ki}} -
\ln\left(\frac{2}{N_e}\left(\frac{2\pi m_e k_B
T}{h^2}\right)^{3/2}\right)\$\$ where \\E\_{ion}\\ is the ionization
energy of the neutral atom and \\N_e\\ the electron density (Aguilera
and Aragón, 2004). Because the correction depends on \\T\\, the fit is
iterated until the temperature converges. The lowering of the ionization
energy in the plasma is neglected. The much wider energy range than in a
Boltzmann plot of a single species gives a more precise temperature.

## References

- Aguilera, J.A., Aragón, C. (2004). Characterization of a laser-induced
  plasma by spatially resolved spectroscopy of neutral atom and ion
  emissions: comparison of local and spatially integrated measurements.
  Spectrochimica Acta Part B, 59(12):1861-1876.

## See also

[`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md),
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md),
[`plot_boltzmann()`](https://christiangoueguel.com/specProc/reference/plot_boltzmann.md)

## Examples

``` r
kB <- 8.617333262e-5
T <- 12000; ne <- 1e17; E_ion <- 6.11
lines <- data.frame(
  stage = c(1, 1, 1, 2, 2, 2),
  wavelength = c(420, 445, 560, 390, 395, 850),
  Aki = c(2e8, 8e7, 5e7, 1.5e8, 1.4e8, 1e7),
  gk = c(3, 5, 7, 4, 2, 6),
  Ek = c(2.9, 4.7, 5.0, 3.1, 3.2, 3.2)
)
saha <- 2 * (2 * pi * 9.1093837015e-31 * 1.380649e-23 * T / 6.62607015e-34^2)^1.5 * 1e-6 / ne
lines$intensity <- with(lines, gk * Aki / wavelength *
  exp(-(Ek + (stage == 2) * E_ion) / (kB * T)) * ifelse(stage == 2, saha, 1))
saha_boltzmann_plot(lines, ionization_energy = E_ion, electron_density = ne)
#> Saha-Boltzmann plot (6 lines)
#> 
#> Temperature:  12000 +/- 0 K
#> R-squared:    1
#> Ne:           1e+17 cm-3
```
