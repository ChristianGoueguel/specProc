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
  <https://stark-b.obspm.fr>. Observatoire de Paris and Astronomical
  Observatory of Belgrade.

## References

- Sahal-Bréchot, S., Dimitrijević, M.S., Moreau, N., Ben Nessib, N.
  (2015). The STARK-B database VAMDC node: a repository for spectral
  line broadening and shifts due to collisions with charged particles.
  Physica Scripta, 90(5):054008.

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
#> Error : Could not download STARK-B data for Ca II (downloaded length 0 != reported length 160). Check the internet connection, or try again later.
if (!inherits(ca, "try-error")) head(ca)
# }
```
