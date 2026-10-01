# Ionization Energies from the NIST Atomic Spectra Database

Retrieves ionization energies from the NIST Atomic Spectra Database, for
example the ionization energy of the neutral atom needed by
[`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md).

## Usage

``` r
nist_ionization_energy(species, timeout = 120)
```

## Arguments

- species:

  A character vector of emitters in spectroscopic notation: `"Ca I"`
  gives the energy needed to ionize neutral calcium, `"Ca II"` that
  needed to ionize Ca\\^{+}\\.

- timeout:

  The download timeout, in seconds. Default is 120.

## Value

A named numeric vector of ionization energies, in eV.

## See also

[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md),
[`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md)

## Examples

``` r
# \donttest{
# needs an internet connection
try(nist_ionization_energy(c("Ca I", "Mg I")))
#>     Ca I     Mg I 
#> 6.113155 7.646236 
# }
```
