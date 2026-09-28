# Stark Width of a Line at Given Plasma Conditions

Computes the Stark width (and shift) of a line at a given temperature
and electron density, from the Stark broadening parameters returned by
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md).

## Usage

``` r
stark_width(
  stark,
  wavelength,
  temperature,
  density = 1e+17,
  perturber = "electron",
  tolerance = 0.5
)
```

## Arguments

- stark:

  A tibble returned by
  [`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md).

- wavelength:

  The wavelength of the line, in nm.

- temperature:

  The temperature, in K (a vector is allowed).

- density:

  The perturber (electron) density, in cm\\^{-3}\\. Default is
  \\10^{17}\\.

- perturber:

  The perturber. Default is `"electron"`.

- tolerance:

  The largest allowed difference between `wavelength` and the tabulated
  wavelength, in nm. Default is 0.5.

## Value

A tibble with one row per temperature and columns `wavelength` (the
requested wavelength, nm), `tabulated_wavelength`, `upper`, `lower`,
`perturber`, `temperature`, `density`, `width` (FWHM, nm) and `shift`
(nm).

## Details

The transition whose wavelength is closest to `wavelength` is used; its
wavelength must lie within `tolerance` nm. STARK-B often tabulates a
multiplet at its mean wavelength \\\lambda_m\\; the width and shift of a
line of the multiplet at \\\lambda\\ are then \\w\_\lambda = w_m
\lambda^2 / \lambda_m^2\\ (and likewise for the shift), as the database
recommends. This scaling is applied to every match. Within the impact
approximation, the width and shift are proportional to the perturber
density, so the tabulated values at the density closest to `density` are
scaled linearly to `density`. Between the tabulated temperatures, the
width is interpolated linearly in \\\log w\\ against \\\log T\\, and the
shift linearly against \\\log T\\; temperatures outside the tabulated
range give an error.

## See also

[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md),
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md)

## Examples

``` r
# A table in the format of starkb_lines(), for illustration
stark <- data.frame(
  species = "X II", wavelength = 500, upper = "u", lower = "l",
  perturber = "electron", temperature = c(5000, 10000, 20000),
  density = 1e17, width = c(0.020, 0.014, 0.010), shift = 0, source = ""
)
stark_width(stark, wavelength = 500, temperature = 8000, density = 5e16)
#> # A tibble: 1 × 9
#>   wavelength tabulated_wavelength upper lower perturber temperature density
#>        <dbl>                <dbl> <chr> <chr> <chr>           <dbl>   <dbl>
#> 1        500                  500 u     l     electron         8000    5e16
#> # ℹ 2 more variables: width <dbl>, shift <dbl>
```
