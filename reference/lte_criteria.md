# Criteria of Local Thermodynamic Equilibrium in Transient Plasmas

Checks the local thermodynamic equilibrium (LTE) of a laser-induced
plasma by the McWhirter criterion and by the temporal and spatial
criteria of Cristoforetti et al. (2010) for transient and inhomogeneous
plasmas.

## Usage

``` r
lte_criteria(
  temperature,
  electron_density,
  delta_e,
  oscillator_strength,
  mass,
  gaunt = 0.2,
  time = NULL,
  length_scale = NULL,
  tolerance = 0.1
)
```

## Arguments

- temperature:

  The plasma temperature, in K.

- electron_density:

  The electron density, in cm\\^{-3}\\.

- delta_e:

  The energy gap of the transition, in eV.

- oscillator_strength:

  The absorption oscillator strength of the transition.

- mass:

  The atomic mass of the species, in u, or its element symbol (such as
  `"Ca"`).

- gaunt:

  The effective Gaunt factor. Default is 0.2.

- time:

  Optional times of the measurements (one per temperature), in s, for
  the temporal criterion.

- length_scale:

  Optional length over which the temperature and density change, in mm,
  for the spatial criterion.

- tolerance:

  The largest relative change (or ratio of lengths) for a criterion to
  be satisfied. Default is 0.1.

## Value

A tibble with one row per measurement (the arguments are recycled to a
common length): the `temperature`, `electron_density` and `delta_e`; the
McWhirter `minimum_density` and whether it is reached (`mcwhirter`); the
`relaxation_time` (s) and `diffusion_length` (mm); with `time`, the
`time`, the relative changes of the temperature and of the density
during the relaxation time (`temperature_change`, `density_change`) and
whether the `temporal` criterion is satisfied; with `length_scale`, the
`length_ratio` of the diffusion length to it and whether the `spatial`
criterion is satisfied.

## Details

The McWhirter criterion (see
[`mcwhirter_criterion()`](https://christiangoueguel.com/specProc/reference/mcwhirter_criterion.md))
gives the minimum electron density for collisions to dominate the
population of the levels in a stationary, homogeneous plasma. It is
necessary but not sufficient in a laser-induced plasma, whose
temperature and density change in time and in space (Cristoforetti et
al., 2010):

- Temporal criterion: the plasma must change little during the time
  \\\tau\_{rel}\\ it takes to relax to equilibrium, \\\|T(t +
  \tau\_{rel}) - T(t)\| / T(t) \ll 1\\, and likewise for \\N_e\\, with
  \$\$\tau\_{rel} \approx \frac{6.3 \times 10^4}{N_e f \langle g
  \rangle} \Delta E\\ (k_B T)^{1/2} \exp\left(\frac{\Delta E}{k_B
  T}\right)\\ \mathrm{s}\$\$

- Spatial criterion: the temperature and density must change little over
  the distance \\\lambda\\ an atom diffuses during \\\tau\_{rel}\\,
  \$\$\lambda \approx \frac{1.4 \times 10^{12}}{N_e} (k_B T)^{3/4}
  \left(\frac{\Delta E}{M f \langle g \rangle}\right)^{1/2}
  \exp\left(\frac{\Delta E}{2 k_B T}\right)\\ \mathrm{cm}\$\$ which must
  be much shorter than the length over which they change, such as the
  size of the plasma.

Here \\N_e\\ is in cm\\^{-3}\\, \\\Delta E\\ and \\k_B T\\ in eV, \\f\\
is the absorption oscillator strength of the transition across the gap
\\\Delta E\\, \\M\\ the atomic mass of the species (u) and \\\langle g
\rangle\\ the effective Gaunt factor (Van Regemorter, 1962). The slowest
transition to equilibrate is usually the resonance transition between
the ground state and the first excited level of the species, such as Ca
II 393.37 nm (\\\Delta E\\ = 3.15 eV, \\f\\ = 0.68);
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)
gives the oscillator strengths (`fik`). The effective Gaunt factor
depends on the transition and on \\\Delta E / k_B T\\; the default, 0.2,
errs toward long relaxation times. The criteria are order-of-magnitude
checks.

The temporal criterion needs the temperature and density at several
times, `time` (such as acquisition delays): their relative changes
during \\\tau\_{rel}\\ are computed from their rates of change between
the measurements (finite differences of their logarithms). The spatial
criterion needs the length over which they change, `length_scale`. A
criterion is satisfied when the relative change, or the ratio of the
diffusion length to `length_scale`, is at most `tolerance`.

## References

- Cristoforetti, G., De Giacomo, A., Dell'Aglio, M., Legnaioli, S.,
  Tognoni, E., Palleschi, V., Omenetto, N. (2010). Local thermodynamic
  equilibrium in laser-induced breakdown spectroscopy: beyond the
  McWhirter criterion. Spectrochimica Acta Part B, 65(1):86-95.

- Van Regemorter, H. (1962). Rate of collisional excitation in stellar
  atmospheres. The Astrophysical Journal, 136:906-915.

## See also

[`mcwhirter_criterion()`](https://christiangoueguel.com/specProc/reference/mcwhirter_criterion.md),
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md),
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md)

## Author

Christian L. Goueguel

## Examples

``` r
# Ca II resonance transition (393.37 nm), in a plasma of 1 mm
lte_criteria(temperature = 9000, electron_density = 1e17, delta_e = 3.15,
             oscillator_strength = 0.68, mass = "Ca", length_scale = 1)
#> # A tibble: 1 × 9
#>   temperature electron_density delta_e minimum_density mcwhirter relaxation_time
#>         <dbl>            <dbl>   <dbl>           <dbl> <lgl>               <dbl>
#> 1        9000             1e17    3.15         4.74e15 TRUE             7.46e-10
#> # ℹ 3 more variables: diffusion_length <dbl>, length_ratio <dbl>, spatial <lgl>

# a plasma cooling from 1 to 5 microseconds
lte_criteria(temperature = c(11000, 9500, 8500, 7800),
             electron_density = c(3e17, 1.5e17, 8e16, 5e16), delta_e = 3.15,
             oscillator_strength = 0.68, mass = "Ca", time = c(1, 2, 3, 5) * 1e-6)
#> # A tibble: 4 × 11
#>   temperature electron_density delta_e minimum_density mcwhirter relaxation_time
#>         <dbl>            <dbl>   <dbl>           <dbl> <lgl>               <dbl>
#> 1       11000           3  e17    3.15         5.25e15 TRUE             1.31e-10
#> 2        9500           1.5e17    3.15         4.87e15 TRUE             4.13e-10
#> 3        8500           8  e16    3.15         4.61e15 TRUE             1.15e- 9
#> 4        7800           5  e16    3.15         4.42e15 TRUE             2.60e- 9
#> # ℹ 5 more variables: diffusion_length <dbl>, time <dbl>,
#> #   temperature_change <dbl>, density_change <dbl>, temporal <lgl>
```
