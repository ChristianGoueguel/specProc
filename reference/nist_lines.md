# Atomic Line Data from the NIST Atomic Spectra Database

Retrieves the lines of an atom or ion in a wavelength range from the
NIST Atomic Spectra Database (ASD): wavelengths, transition
probabilities, level energies and statistical weights, as needed by
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)
and
[`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md).
The data are downloaded on demand; they are not distributed with
specProc.

## Usage

``` r
nist_lines(species, wavelength, with_aki = TRUE, timeout = 120)
```

## Arguments

- species:

  A character string naming the emitter in spectroscopic notation, such
  as `"Ca I"` or `"Ca II"`.

- wavelength:

  A numeric vector of length 2: the wavelength range, in nm.

- with_aki:

  A logical: keep only the lines with a transition probability (`TRUE`,
  default).

- timeout:

  The download timeout, in seconds. Default is 120.

## Value

A tibble with one row per line and columns `species`, `wavelength` (nm),
`Aki` (s\\^{-1}\\), `fik` (oscillator strength), `accuracy`, `Ei` and
`Ek` (lower and upper level energies, eV), `gi` and `gk` (statistical
weights), `lower` and `upper` (configuration, term and J of the levels)
and `intensity` (the relative intensity listed by the ASD, as text).

## Details

Wavelengths are in air between 200 and 2000 nm, and in vacuum outside
this range, as in the ASD. The observed wavelength is used when there is
one, the Ritz wavelength otherwise. Energies in brackets or with other
qualifiers are returned as they can be read as numbers; check the ASD
for such levels. The accuracy grade of the transition probability
(`accuracy`: AAA, AA, A+, A, B+, B, C+, C, D+, D, E) matters for plasma
diagnostics: prefer lines of grade B or better.

The results of a query are cached for the R session. Cite the database
when you use these data:

- Kramida, A., Ralchenko, Yu., Reader, J., and NIST ASD Team. NIST
  Atomic Spectra Database, <https://physics.nist.gov/asd>. National
  Institute of Standards and Technology, Gaithersburg, MD.

## See also

[`nist_ionization_energy()`](https://christiangoueguel.com/specProc/reference/nist_ionization_energy.md),
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md),
[`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md),
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md)

## Author

Christian L. Goueguel

## Examples

``` r
# \donttest{
# needs an internet connection
ca <- try(nist_lines("Ca II", wavelength = c(310, 400)))
if (!inherits(ca, "try-error")) ca
#> # A tibble: 7 × 12
#>   species wavelength      Aki   fik accuracy    Ei    Ek    gi    gk lower upper
#>   <chr>        <dbl>    <dbl> <dbl> <chr>    <dbl> <dbl> <dbl> <dbl> <chr> <chr>
#> 1 Ca II         316.   3.10e8 0.93  C         3.12  7.05     2     4 3p6.… 3p6.…
#> 2 Ca II         318.   3.60e8 0.82  C         3.15  7.05     4     6 3p6.… 3p6.…
#> 3 Ca II         318.   5.80e7 0.088 C         3.15  7.05     4     4 3p6.… 3p6.…
#> 4 Ca II         371.   8.80e7 0.18  C         3.12  6.47     2     2 3p6.… 3p6.…
#> 5 Ca II         374.   1.7 e8 0.18  C         3.15  6.47     4     2 3p6.… 3p6.…
#> 6 Ca II         393.   1.47e8 0.682 C         0     3.15     2     4 3p6.… 3p6.…
#> 7 Ca II         397.   1.4 e8 0.33  C         0     3.12     2     2 3p6.… 3p6.…
#> # ℹ 1 more variable: intensity <chr>
# }
```
