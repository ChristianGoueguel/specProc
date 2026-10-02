# Stark Broadening Parameters from the STARK-B Database

Retrieves the Stark broadening parameters (full widths at half maximum
and shifts) of the lines of an atom or ion from STARK-B, the database of
Stark broadening parameters of isolated lines of atoms and ions in the
impact approximation (Sahal-Bréchot, Dimitrijević and Moreau,
Observatoire de Paris and Astronomical Observatory of Belgrade). The
data are downloaded on demand from the database's Virtual Atomic and
Molecular Data Centre (VAMDC) service; they are not distributed with
specProc.

## Usage

``` r
starkb_lines(species, wavelength = NULL, perturber = NULL, timeout = 120)
```

## Arguments

- species:

  A character string naming the emitter in spectroscopic notation, such
  as `"Ca II"` (singly ionized calcium) or `"Na I"` (neutral sodium).

- wavelength:

  An optional numeric vector of length 2: the wavelength range to keep,
  in nm.

- perturber:

  An optional character vector of perturbers to keep, such as
  `"electron"`, `"H II"` (protons) or `"He II"`. By default all are
  kept.

- timeout:

  The download timeout, in seconds. Default is 120.

## Value

A tibble with one row per transition, perturber, temperature and
density, and columns:

- `species`, `wavelength` (nm, as given by STARK-B), `upper`, `lower`
  (configuration and term of the upper and lower levels);

- `perturber`, `temperature` (K), `density` (perturber density,
  cm\\^{-3}\\);

- `width` (Stark full width at half maximum, nm) and `shift` (nm;
  positive towards the red);

- `source`: the publications the data come from.

## Details

For each transition, STARK-B tabulates the Stark width \\w\\ (full width
at half maximum) and shift \\d\\ for collisions with electrons and with
ions (protons, singly ionized helium, ...), at several temperatures and
perturber densities. In the impact approximation, the width and shift
are proportional to the perturber density. Many entries are multiplets,
given at the mean wavelength of the multiplet; the `upper` and `lower`
configurations and terms identify them.

The results of a query are cached for the R session, so repeated calls
with the same `species` do not download the data again.

When you use these data, cite the STARK-B database and the original
publications, listed in the `source` column:

- Sahal-Bréchot, S., Dimitrijević, M.S., Moreau, N. STARK-B database,
  `stark-b.obspm.fr`. Observatoire de Paris and Astronomical Observatory
  of Belgrade (see the References).

## References

- Sahal-Bréchot, S., Dimitrijević, M.S., Moreau, N., Ben Nessib, N.
  (2015). The STARK-B database VAMDC node: a repository for spectral
  line broadening and shifts due to collisions with charged particles.
  Physica Scripta, 90(5):054008.
  [doi:10.1088/0031-8949/90/5/054008](https://doi.org/10.1088/0031-8949/90/5/054008)

## See also

[`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)
to interpolate the width of a line at a given temperature and density,
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md),
[`read_starkb()`](https://christiangoueguel.com/specProc/reference/read_starkb.md)
for saved STARK-B files, and
[`stark_table()`](https://christiangoueguel.com/specProc/reference/stark_table.md)
for data from other sources.

## Author

Christian L. Goueguel

## Examples

``` r
# \donttest{
# needs an internet connection
ca <- try(starkb_lines("Ca II", wavelength = c(390, 400), perturber = "electron"))
if (!inherits(ca, "try-error")) head(ca)
#> # A tibble: 6 × 10
#>   species wavelength upper  lower perturber temperature density   width    shift
#>   <chr>        <dbl> <chr>  <chr> <chr>           <dbl>   <dbl>   <dbl>    <dbl>
#> 1 Ca II         395. 3p6.4… 3p6.… electron         5000    1e13 2.96e-6 -5.18e-7
#> 2 Ca II         395. 3p6.4… 3p6.… electron        10000    1e13 2.28e-6 -4.23e-7
#> 3 Ca II         395. 3p6.4… 3p6.… electron        20000    1e13 1.88e-6 -3.27e-7
#> 4 Ca II         395. 3p6.4… 3p6.… electron        30000    1e13 1.77e-6 -2.78e-7
#> 5 Ca II         395. 3p6.4… 3p6.… electron        50000    1e13 1.71e-6 -2.57e-7
#> 6 Ca II         395. 3p6.4… 3p6.… electron       100000    1e13 1.66e-6 -2.14e-7
#> # ℹ 1 more variable: source <chr>
# }
```
