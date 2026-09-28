# Energy Levels and Partition Functions from the NIST Atomic Spectra Database

`nist_levels()` retrieves the energy levels of an atom or ion from the
NIST Atomic Spectra Database, and `partition_function()` computes the
internal partition function of a species from its levels, as needed by
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md).

## Usage

``` r
nist_levels(species, timeout = 120)

partition_function(levels, temperature, max_energy = Inf)
```

## Arguments

- species:

  A character vector of species in spectroscopic notation, such as
  `"Ca I"`.

- timeout:

  The download timeout, in seconds. Default is 120.

- levels:

  A data frame of levels with columns `species`, `g` and `energy` (eV),
  such as returned by `nist_levels()`.

- temperature:

  The temperature(s), in K.

- max_energy:

  The highest level energy included, in eV: one value, or a vector named
  by species. Default is `Inf` (all levels).

## Value

`nist_levels()`: a tibble with one row per level and columns `species`,
`configuration`, `term`, `J`, `g` (statistical weight) and `energy`
(eV).

`partition_function()`: a tibble with the `species`, the `temperature`
and the `partition` function, for each species and temperature.

## Details

All the levels listed by the ASD are returned, including the
autoionizing levels above the first ionization limit, as in the
partition functions computed by the ASD; levels without a statistical
weight are dropped. The partition function \$\$U(T) = \sum_i g_i
\exp\left(-\frac{E_i}{k_B T}\right)\$\$ is summed over the levels given,
up to `max_energy`. In a plasma, the ionization energy is lowered by the
surrounding charges, and the levels above the lowered limit do not
exist: truncating the sum there matters for atoms with many high Rydberg
levels close to the limit, such as the alkali metals (see
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md),
which does it). Without truncation, the result equals the partition
function computed by the ASD.

The results of a query are cached for the R session. Cite the database
when you use these data (see
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)).

## See also

[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md),
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md),
[`nist_ionization_energy()`](https://christiangoueguel.com/specProc/reference/nist_ionization_energy.md)

## Author

Christian L. Goueguel

## Examples

``` r
# \donttest{
# needs an internet connection
ca <- try(nist_levels("Ca I"))
if (!inherits(ca, "try-error")) partition_function(ca, temperature = c(8000, 10000))
#> # A tibble: 2 × 3
#>   species temperature partition
#>   <chr>         <dbl>     <dbl>
#> 1 Ca I           8000      2.60
#> 2 Ca I          10000      5.69
# }
```
