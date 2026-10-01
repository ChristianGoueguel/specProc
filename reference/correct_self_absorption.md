# Self-Absorption Correction with an Internal Reference Line

Corrects the intensities of emission lines for self-absorption by the
internal reference method of Sun and Yu (2009): for each species, the
self-absorption coefficient of every line is measured against a line of
the same species that is little self-absorbed.

## Usage

``` r
correct_self_absorption(
  lines,
  temperature = NULL,
  electron_density = NULL,
  ionization_energy = NULL,
  reference = NULL,
  units = "energy",
  tol = 1e-08,
  timeout = 120
)
```

## Arguments

- lines:

  A data frame with one row per line and columns `species`, `intensity`,
  `wavelength` (nm), `Aki` (s\\^{-1}\\), `gk`, `Ek` (upper-level energy,
  eV) and, for the default reference, `Ei` (lower-level energy, eV).

- temperature:

  The plasma temperature, in K. Without it, it is estimated from the
  corrected lines, which needs `electron_density`.

- electron_density:

  The electron density, in cm\\^{-3}\\, to estimate the temperature (see
  details).

- ionization_energy:

  The ionization energies of the neutral atoms, in eV, named by element,
  to estimate the temperature. By default, they are taken from the NIST
  database (see
  [`nist_ionization_energy()`](https://christiangoueguel.com/specProc/reference/nist_ionization_energy.md)).

- reference:

  The reference lines: row numbers of `lines`, one per species, or
  `NULL` (default) to choose them by optical depth.

- units:

  The units of `intensity`: `"energy"` (default) or `"photons"`, as in
  [`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md).

- tol:

  The relative tolerance on the estimated temperature. Default is 1e-8.

- timeout:

  The timeout of the NIST downloads, in seconds.

## Value

`lines`, as a tibble, with the corrected `intensity`, the
`measured_intensity`, the self-absorption coefficient `SA` and a logical
`reference`, ready for
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)
or
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md).
The temperature used is in the attribute `temperature`.

## Details

In an optically thin plasma in LTE, the intensity ratio of two lines of
the same species depends only on their atomic data and on the
temperature. With a reference line \\r\\ assumed optically thin, the
self-absorption coefficient of a line \\j\\ is \$\$SA_j =
\frac{I_j}{I_r} \frac{g_r A_r \lambda_j}{g_j A_j \lambda_r}
\exp\left(\frac{E_j - E_r}{k_B T}\right)\$\$ (for intensities in energy
units; without the wavelengths for photon counts), where \\g\\, \\A\\
and \\E\\ are the statistical weight, transition probability and energy
of the upper level. The corrected intensity is \\I_j / SA_j\\.

Self-absorption grows with the optical depth of the line, roughly
proportional to \\g_k A\_{ki} \lambda^4 \exp(-E_i / k_B T)\\, where
\\E_i\\ is the energy of the lower level: resonance lines (\\E_i = 0\\)
and strong lines are the most absorbed. By default, the reference of
each species is the line of smallest optical depth, which needs the
lower-level energies (column `Ei`, as returned by
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md));
otherwise, give the reference lines with `reference`. The reference line
must be well measured: its noise propagates to every coefficient.

The corrected lines of a species lie, by construction, on the Boltzmann
plot of its reference line at the temperature used, so they cannot give
the temperature by themselves. Give `temperature`, measured on optically
thin lines (for example with
[`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md)
or
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md)),
or give `electron_density`: the temperature is then the one at which the
corrected lines of the neutral atom and of the ion of each element lie
on a common Saha-Boltzmann plot (as in
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md)),
which needs lines of both stages for at least one element. This is the
iterative scheme of Sun and Yu (2009), solved directly for the
temperature.

Coefficients above 1 mean that the line is stronger than expected from
the reference, because of noise, errors of the atomic data, or a
self-absorbed reference; they are kept, with a warning.

## References

- Sun, L., Yu, H. (2009). Correction of self-absorption effect in
  calibration-free laser-induced breakdown spectroscopy by an internal
  reference method. Talanta, 79(2):388-395.

## See also

[`self_absorption()`](https://christiangoueguel.com/specProc/reference/self_absorption.md)
for the coefficient from line widths,
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md),
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
mean_spectrum <- colMeans(forageLIBS[-(1:14)])
# potassium lines: the resonance doublet at 766.49 and 769.90 nm ends on
# the ground state (Ei = 0) and is strongly reabsorbed
k_lines <- data.frame(
  species = "K I", wavelength = c(404.414, 404.721, 691.108, 693.877, 766.490, 769.896),
  Aki = c(1.150e6, 1.070e6, 2.500e6, 4.956e6, 3.779e7, 3.734e7),
  gk = c(4, 2, 2, 2, 4, 2),
  Ek = c(3.065, 3.063, 3.403, 3.403, 1.617, 1.610),
  Ei = c(0, 0, 1.610, 1.617, 0, 0)
)
corrected <- correct_self_absorption(line_intensities(mean_spectrum, k_lines), temperature = 8000)
#> Warning: 1 line(s) have SA > 1: check the reference lines, the atomic data or the intensities.
corrected[c("wavelength", "measured_intensity", "SA", "reference")]
#> # A tibble: 6 × 4
#>   wavelength measured_intensity     SA reference
#>        <dbl>              <dbl>  <dbl> <lgl>    
#> 1       404.               449. 0.531  FALSE    
#> 2       405.               394. 1      TRUE     
#> 3       691.               407. 1.24   FALSE    
#> 4       694.               424. 0.651  FALSE    
#> 5       766.              5418. 0.0452 FALSE    
#> 6       770.              7178. 0.121  FALSE    
```
