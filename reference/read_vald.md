# Stark Widths from VALD3 and Kurucz Line Lists

Reads the quadratic Stark damping constants of a line list saved from
the Vienna Atomic Line Database (VALD3) or from the line lists of R. L.
Kurucz, as a table of Stark widths in the format of
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md),
for
[`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)
and
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md).
These line lists give a width for many lines that STARK-B does not have,
but a cruder one.

## Usage

``` r
read_vald(
  file,
  species = NULL,
  wavelength = NULL,
  temperature = c(2500, 5000, 10000, 20000, 40000)
)

read_kurucz(
  file,
  species = NULL,
  wavelength = NULL,
  temperature = c(2500, 5000, 10000, 20000, 40000)
)
```

## Arguments

- file:

  The path to the line list: an extract saved from VALD3 for
  `read_vald()`, a Kurucz line list for `read_kurucz()`.

- species:

  An optional character vector of the species to keep, in spectroscopic
  notation, such as `c("Ca II", "Mg I")`. By default all the species of
  the file are kept.

- wavelength:

  An optional numeric vector of length 2: the wavelength range to keep,
  in nm.

- temperature:

  The temperatures (K) at which the widths are tabulated. Default is
  2500 to 40 000 K;
  [`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)
  interpolates between them.

## Value

A tibble in the format of
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md),
with one row per line and temperature: `species`, `wavelength` (nm),
`upper` and `lower` (the level designations, empty when the file does
not give them), `perturber` (`"electron"`), `temperature`, `density`
(\\10^{17}\\ cm\\^{-3}\\), `width` (nm), `shift` (0) and `source` (the
line list, the logarithm of the constant and its references).

## Details

**Files.** The data of VALD3 are free, but need a registration: request
an "Extract All", "Extract Element" or "Extract Stellar" list from the
VALD website or its email service, in the short or the long format, with
any wavelength unit (Å, nm or cm\\^{-1}\\), medium and energy unit, and
give the saved file to `read_vald()`. The long format also gives the
designations of the levels (`lower` and `upper`). The line lists of
Kurucz are free to download from `kurucz.harvard.edu`: the whole list
(`gfall*.dat`, in air above 200 nm, or `gfallvac*.dat`, in vacuum) or
the list of one ion (such as `atoms/2001/gf2001.pos` for Ca II), in the
160-column format of these files. The wavenumber files (`gfallwn*.dat`)
are not read. The wavelengths are kept in the medium of the file: in
air, they match those of
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)
above 200 nm.

**Widths.** Both line lists give, for each line, the decimal logarithm
of the quadratic Stark damping constant \\\gamma\\ at 10 000 K per
electron: the full width at half maximum of the Lorentzian profile in
angular frequency (rad s\\^{-1}\\), divided by the electron density
(cm\\^{-3}\\). The full width at half maximum in wavelength is \$\$w =
\frac{\lambda^2}{2 \pi c} \gamma N_e\$\$ It is tabulated at the density
\\10^{17}\\ cm\\^{-3}\\ (the width is proportional to it) and at each of
`temperature`, scaled as \\(T / 10^4\\\mathrm{K})^{1/6}\\, the
temperature dependence of the quadratic Stark width in the impact
approximation (Gray, 2005). The shift is not given: it is set to zero.
Lines without a Stark constant (zero in the file) are left out, and a
line listed several times (as its isotopic or hyperfine components) is
kept once.

**Accuracy.** Most of these constants come from approximate formulas
rather than from calculations such as those of STARK-B, and the
temperature law is approximate too: prefer STARK-B
([`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md))
or measured widths
([`stark_table()`](https://christiangoueguel.com/specProc/reference/stark_table.md))
when they exist, and use these for the other lines. A table of several
species needs `species` in
[`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)
and
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md).

When you use these data, cite the line list and the references of the
constants, given in the `source` column (the reference codes of VALD are
described in the BibTeX file that comes with each extract).

## References

- Ryabchikova, T., Piskunov, N., Kurucz, R.L., Stempels, H.C., Heiter,
  U., Pakhomov, Yu., Barklem, P.S. (2015). A major upgrade of the VALD
  database. Physica Scripta, 90(5):054005.
  [doi:10.1088/0031-8949/90/5/054005](https://doi.org/10.1088/0031-8949/90/5/054005)

- Kurucz, R.L. Atomic line lists, `kurucz.harvard.edu/linelists.html`.

- Gray, D.F. (2005). The Observation and Analysis of Stellar
  Photospheres, 3rd edition. Cambridge University Press.

## See also

[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md),
[`stark_table()`](https://christiangoueguel.com/specProc/reference/stark_table.md),
[`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md),
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md)

## Author

Christian L. Goueguel

## Examples

``` r
# two lines of a VALD3 "Extract Element" list, short format (made-up
# references)
file <- tempfile(fileext = ".txt")
writeLines(c(
  "                                            Damping parameters    Lande",
  "Elm Ion       WL_air(A) Excit(eV) log(gf)   Rad.  Stark    Waals  factor References",
  "'Ca 2',       3933.6630,   0.0000,  0.135, 8.170,-6.270,  -7.760, 1.170,'   1 K10   Ca+  '",
  "'Ca 2',       3968.4690,   0.0000, -0.180, 8.160,-6.270,  -7.760, 1.330,'   1 K10   Ca+  '"
), file)
ca <- read_vald(file)
ca
#> # A tibble: 10 × 10
#>    species wavelength upper lower perturber temperature density   width shift
#>    <chr>        <dbl> <chr> <chr> <chr>           <dbl>   <dbl>   <dbl> <dbl>
#>  1 Ca II         393. ""    ""    electron         2500    1e17 0.00350     0
#>  2 Ca II         393. ""    ""    electron         5000    1e17 0.00393     0
#>  3 Ca II         393. ""    ""    electron        10000    1e17 0.00441     0
#>  4 Ca II         393. ""    ""    electron        20000    1e17 0.00495     0
#>  5 Ca II         393. ""    ""    electron        40000    1e17 0.00556     0
#>  6 Ca II         397. ""    ""    electron         2500    1e17 0.00356     0
#>  7 Ca II         397. ""    ""    electron         5000    1e17 0.00400     0
#>  8 Ca II         397. ""    ""    electron        10000    1e17 0.00449     0
#>  9 Ca II         397. ""    ""    electron        20000    1e17 0.00504     0
#> 10 Ca II         397. ""    ""    electron        40000    1e17 0.00566     0
#> # ℹ 1 more variable: source <chr>
stark_width(ca, wavelength = 393.366, temperature = 9000, density = 5e16)
#> # A tibble: 1 × 9
#>   wavelength tabulated_wavelength upper lower perturber temperature density
#>        <dbl>                <dbl> <chr> <chr> <chr>           <dbl>   <dbl>
#> 1       393.                 393. ""    ""    electron         9000    5e16
#> # ℹ 2 more variables: width <dbl>, shift <dbl>

# the same line in the 160-column format of the Kurucz line lists
kurucz <- tempfile(fileext = ".pos")
writeLines(paste0(
  "   393.3663  0.111 20.01       0.000  0.5 4s 2S        25414.400  1.5 4p 2P",
  "       8.17 -6.27 -7.76K10 "
), kurucz)
read_kurucz(kurucz, temperature = c(5000, 20000))
#> # A tibble: 2 × 10
#>   species wavelength upper lower perturber temperature density   width shift
#>   <chr>        <dbl> <chr> <chr> <chr>           <dbl>   <dbl>   <dbl> <dbl>
#> 1 Ca II         393. 4p 2P 4s 2S electron         5000    1e17 0.00393     0
#> 2 Ca II         393. 4p 2P 4s 2S electron        20000    1e17 0.00495     0
#> # ℹ 1 more variable: source <chr>
```
