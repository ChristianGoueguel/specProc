# Build a Table of Stark Broadening Parameters

Builds a table of Stark widths and shifts from user-supplied values, in
the format of
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md),
so that
[`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)
and
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md)
can use Stark parameters from any source: values copied from the STARK-B
website, from a publication, or from the fitted temperature laws that
STARK-B provides.

## Usage

``` r
stark_table(
  wavelength,
  temperature,
  width = NULL,
  units,
  density = 1e+17,
  shift = 0,
  perturber = "electron",
  species = NA_character_,
  upper = "",
  lower = "",
  coefficients = NULL,
  shift_coefficients = NULL,
  log_base = 10,
  source = "user-supplied"
)
```

## Arguments

- wavelength:

  The wavelength of the line (or multiplet), in `units`.

- temperature:

  The temperatures, in K.

- width:

  The Stark full widths at half maximum at each temperature, in `units`.
  Leave `NULL` when `coefficients` are given.

- units:

  The units of `wavelength`, `width` and `shift`: `"nm"` or `"A"`
  (Ångström). Required.

- density:

  The perturber density of the widths, in cm\\^{-3}\\. Default is
  \\10^{17}\\.

- shift:

  The Stark shifts, in `units`. Default is 0 (unknown).

- perturber:

  The perturber. Default is `"electron"`.

- species, upper, lower:

  Optional labels of the emitter and of the upper and lower levels.

- coefficients:

  Optional coefficients \\(a_0, a_1, a_2)\\ of the fitted width law.

- shift_coefficients:

  Optional coefficients \\(b_0, b_1, b_2)\\ of the fitted law for the
  ratio \\d/w\\; used with `coefficients`.

- log_base:

  The base of the logarithms of the fitted laws. Default is 10.

- source:

  A description of where the data come from.

## Value

A tibble with the columns of
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md).

## Details

Give either the tabulated `width` at each `temperature`, or the
coefficients of a fitted temperature law. STARK-B fits the widths and
shifts at a given perturber density as \$\$\log w = a_0 + a_1 \log T +
a_2 (\log T)^2\$\$ \$\$d / w = b_0 + b_1 \log T + b_2 (\log T)^2\$\$
with decimal logarithms and \\w\\ in the units of its tables (Å); the
fits are only valid within the tabulated temperature range
(Sahal-Bréchot, Dimitrijević and Ben Nessib, 2011). With `coefficients`,
the table is evaluated at `temperature`, which should span that range.
Check the base of the logarithm and the units of any other source.

`units` is required, because Stark widths are reported in both Å and nm,
and a wrong guess changes the electron density tenfold. It applies to
`wavelength`, `width` and `shift` (and to the fitted widths); the table
is returned in nm.

Arguments of length 1 are recycled. Tables for several lines or
perturbers can be combined with
[`rbind()`](https://rdrr.io/r/base/cbind.html) or
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).

## References

- Sahal-Bréchot, S., Dimitrijević, M.S., Ben Nessib, N. (2011). Widths
  and shifts of isolated lines of neutral and ionized atoms perturbed by
  collisions with electrons and ions: an outline of the semiclassical
  perturbation (SCP) method and of the approximations used for the
  calculations. Baltic Astronomy, 20:523-530.

## See also

[`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md),
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md),
[`read_starkb()`](https://christiangoueguel.com/specProc/reference/read_starkb.md)

## Examples

``` r
# made-up widths of a hypothetical line, in Angstrom at 1e17 cm-3
x <- stark_table(
  wavelength = 5000, temperature = c(5000, 10000, 20000),
  width = c(0.20, 0.16, 0.13), units = "A", species = "X II"
)
x
#> # A tibble: 3 × 10
#>   species wavelength upper lower perturber temperature density width shift
#>   <chr>        <dbl> <chr> <chr> <chr>           <dbl>   <dbl> <dbl> <dbl>
#> 1 X II           500 ""    ""    electron         5000    1e17 0.02      0
#> 2 X II           500 ""    ""    electron        10000    1e17 0.016     0
#> 3 X II           500 ""    ""    electron        20000    1e17 0.013     0
#> # ℹ 1 more variable: source <chr>
electron_density(0.012, stark = x, wavelength = 500, temperature = 8000)
#> [1] 6.980125e+16

# the same line from the coefficients of a fitted temperature law
# log10(w) = a0 + a1 log10(T) + a2 log10(T)^2 (here fitted to the table above)
lt <- log10(c(5000, 10000, 20000))
a <- unname(coef(lm(log10(c(0.20, 0.16, 0.13)) ~ lt + I(lt^2))))
stark_table(wavelength = 5000, temperature = c(5000, 10000, 20000),
            coefficients = a, units = "A")
#> # A tibble: 3 × 10
#>   species wavelength upper lower perturber temperature density  width shift
#>   <chr>        <dbl> <chr> <chr> <chr>           <dbl>   <dbl>  <dbl> <dbl>
#> 1 NA             500 ""    ""    electron         5000    1e17 0.0200     0
#> 2 NA             500 ""    ""    electron        10000    1e17 0.016      0
#> 3 NA             500 ""    ""    electron        20000    1e17 0.013      0
#> # ℹ 1 more variable: source <chr>
```
