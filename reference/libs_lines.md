# Candidate Emission Lines for LIBS Spectra

Lists the lines of the selected atoms and ions that are expected to be
the strongest in a plasma at a given temperature, from the NIST Atomic
Spectra Database, to help identify the emission lines of a spectrum.

## Usage

``` r
libs_lines(
  species,
  wavelength = c(200, 900),
  temperature = 10000,
  top = 20,
  min_relative = 0,
  timeout = 120
)
```

## Arguments

- species:

  A character vector of emitters in spectroscopic notation, such as
  `c("Ca I", "Ca II", "Mg I")`.

- wavelength:

  A numeric vector of length 2: the wavelength range, in nm. Default is
  200 to 900 nm.

- temperature:

  The plasma temperature, in K. Default is 10000.

- top:

  The maximum number of lines kept per species. Default is 20.

- min_relative:

  The minimum relative intensity of the lines kept, between 0 and 1.
  Default is 0.

- timeout:

  The download timeout, in seconds. Default is 120.

## Value

A tibble with one row per line, sorted by wavelength, and columns
`species`, `element`, `stage` (1 for neutral atoms, 2 for singly charged
ions, ...), `wavelength` (nm), `relative_intensity`, `Aki`, `Ek`, `gk`,
`accuracy`, `lower` and `upper`. Species without lines in the range
contribute no rows.

## Details

For each species, the lines with a transition probability are retrieved
with
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)
(and cached for the R session). Their relative intensities in a plasma
in local thermodynamic equilibrium at temperature \\T\\ are \$\$I
\propto \frac{g_k A\_{ki}}{\lambda} \exp\left(-\frac{E_k}{k_B
T}\right)\$\$ normalized to the strongest line of the species in the
range. The intensities are relative within each species: comparing
species would need their concentrations and, between ionization stages,
the Saha equation. Self-absorption, which weakens resonance lines, is
ignored.

The `top` strongest lines of each species with a relative intensity of
at least `min_relative` are kept.
[`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md)
overlays them on a spectrum, and
[`line_finder()`](https://christiangoueguel.com/specProc/reference/line_finder.md)
does so interactively.

## See also

[`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md),
[`line_finder()`](https://christiangoueguel.com/specProc/reference/line_finder.md),
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)

## Author

Christian L. Goueguel

## Examples

``` r
# \donttest{
# needs an internet connection
lines <- try(libs_lines(c("Ca I", "Ca II"), wavelength = c(390, 450), top = 5))
if (!inherits(lines, "try-error")) lines
#> # A tibble: 10 × 11
#>    species element stage wavelength relative_intensity       Aki    Ek    gk
#>    <chr>   <chr>   <int>      <dbl>              <dbl>     <dbl> <dbl> <dbl>
#>  1 Ca II   Ca          2       393.         1          147000000  3.15     4
#>  2 Ca II   Ca          2       397.         0.487      140000000  3.12     2
#>  3 Ca II   Ca          2       410.         0.0000123    9900000 10.5      4
#>  4 Ca II   Ca          2       411.         0.0000224   12000000 10.5      6
#>  5 Ca II   Ca          2       422.         0.00000564   8500000 10.5      2
#>  6 Ca I    Ca          1       423.         1          218000000  2.93     3
#>  7 Ca I    Ca          1       430.         0.120      136000000  4.78     5
#>  8 Ca I    Ca          1       432.         0.0394      74000000  4.77     3
#>  9 Ca I    Ca          1       443.         0.0642      67000000  4.68     5
#> 10 Ca I    Ca          1       445.         0.116       87000000  4.68     7
#> # ℹ 3 more variables: accuracy <chr>, lower <chr>, upper <chr>
# }
```
