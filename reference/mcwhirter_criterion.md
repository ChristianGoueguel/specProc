# McWhirter Criterion for Local Thermodynamic Equilibrium

Computes the minimum electron density required for local thermodynamic
equilibrium (LTE) by the McWhirter criterion, and checks a measured
density against it.

## Usage

``` r
mcwhirter_criterion(temperature, delta_e, electron_density = NULL)
```

## Arguments

- temperature:

  The plasma temperature, in K.

- delta_e:

  The largest energy gap between adjacent levels, in eV.

- electron_density:

  An optional measured electron density, in cm\\^{-3}\\.

## Value

A tibble with the `temperature`, `delta_e`, `minimum_density`
(cm\\^{-3}\\) and, if `electron_density` is given, the density and
whether the criterion is `satisfied`. The inputs are recycled to a
common length.

## Details

\$\$N_e \geq 1.6 \times 10^{12}\\ T^{1/2} (\Delta E)^3\\
\mathrm{cm^{-3}}\$\$ with \\T\\ in K and \\\Delta E\\ (eV) the largest
energy gap between adjacent levels of interest. The criterion is
necessary but not sufficient: in transient and inhomogeneous plasmas
such as those of LIBS, LTE also requires that equilibration be faster
than the plasma evolution and diffusion (Cristoforetti et al., 2010).

## References

- McWhirter, R.W.P. (1965). Spectral intensities. In Huddlestone, R.H.,
  Leonard, S.L. (eds.), Plasma Diagnostic Techniques, Academic Press,
  New York, pp. 201-264.

- Cristoforetti, G., et al. (2010). Local thermodynamic equilibrium in
  laser-induced breakdown spectroscopy: beyond the McWhirter criterion.
  Spectrochimica Acta Part B, 65(1):86-95.

## Examples

``` r
mcwhirter_criterion(temperature = 10000, delta_e = 3.12, electron_density = 1e17)
#> # A tibble: 1 × 5
#>   temperature delta_e minimum_density electron_density satisfied
#>         <dbl>   <dbl>           <dbl>            <dbl> <lgl>    
#> 1       10000    3.12         4.86e15             1e17 TRUE     
```
