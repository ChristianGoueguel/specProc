# Uncertainty of the Electron Density from Stark Broadening

Computes the electron density from the Stark width of a line, as
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md),
with its standard uncertainty and confidence interval, from the standard
error of the width and the uncertainty of the Stark parameter of the
line.

## Usage

``` r
electron_density_uncertainty(
  width,
  width_se = 0,
  method = "stark",
  stark = NULL,
  wavelength = NULL,
  temperature = NULL,
  reference_width = NULL,
  reference_density = 1e+17,
  reference_rsd = 0,
  tolerance = 0.5,
  level = 0.95
)
```

## Arguments

- width:

  The measured Stark full width at half maximum, in nm (see Details). A
  vector is allowed.

- width_se:

  The standard error of `width`, in nm: one value, or one per width.
  Default is 0.

- method:

  `"stark"` (default) or `"halpha"`.

- stark:

  For `method = "stark"`: a tibble returned by
  [`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md).
  Alternatively, give `reference_width`.

- wavelength:

  For `method = "stark"` with `stark`: the wavelength of the line, in
  nm.

- temperature:

  For `method = "stark"` with `stark`: the plasma temperature, in K.

- reference_width:

  For `method = "stark"`: the electron-impact Stark width (nm) at
  `reference_density`, when `stark` is not given.

- reference_density:

  The density of `reference_width`, in cm\\^{-3}\\. Default is
  \\10^{17}\\.

- reference_rsd:

  The relative standard uncertainty of the reference width: of the Stark
  parameter of the line (`method = "stark"`), or of the width law of
  H\\\alpha\\ (`method = "halpha"`). Default is 0.

- tolerance:

  For `method = "stark"` with `stark`: the largest difference between
  `wavelength` and the tabulated wavelength, in nm (see
  [`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)).
  Default is 0.5.

- level:

  The confidence level of the interval. Default is 0.95.

## Value

A tibble with one row per width: the `width` and `width_se`, the
`electron_density` (cm\\^{-3}\\), its standard uncertainty `se`, its
relative standard uncertainty `rsd`, and the `lower` and `upper` bounds
of the interval. Pass `se` to
[`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md)
as `electron_density_se`.

## Details

The electron density is proportional to \\(w / w\_{ref})^p\\, where
\\w\\ is the measured width, \\w\_{ref}\\ the reference width of the
line (its Stark parameter, or the width law of H\\\alpha\\), \\p = 1\\
for `method = "stark"` and \\p = 1 / 0.67823\\ for `method = "halpha"`.
With lognormal errors of \\w\\ and \\w\_{ref}\\, the density is
lognormal, with \$\$\sigma\_{\ln N_e} = p \sqrt{\ln(1 + u_w^2) + \ln(1 +
u\_{ref}^2)}\$\$ where \\u_w\\ and \\u\_{ref}\\ are the relative
standard uncertainties of the width and of the reference width. The
interval is \\N_e e^{\pm z \sigma\_{\ln N_e}}\\, and the relative
standard uncertainty is \\\sqrt{e^{\sigma^2\_{\ln N_e}} - 1}\\.

The width of H\\\alpha\\ grows less than linearly with the density, so
its relative uncertainty is amplified about 1.5 times in the density.
The standard error of the width is that of the fit of the line (the
`std.error` of the width in
[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)),
or that of replicate measurements. The uncertainty of the reference
width depends on its source: give it with `reference_rsd`. The weak
dependence of the Stark width on the temperature is not propagated.

## See also

[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md),
[`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md),
[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)

## Examples

``` r
# H-alpha width of 0.50 +/- 0.02 nm, with a 10% uncertainty of the width law
electron_density_uncertainty(0.5, width_se = 0.02, method = "halpha", reference_rsd = 0.1)
#> # A tibble: 1 × 7
#>   width width_se electron_density      se   rsd   lower   upper
#>   <dbl>    <dbl>            <dbl>   <dbl> <dbl>   <dbl>   <dbl>
#> 1   0.5     0.02          3.14e16 5.00e15 0.159 2.30e16 4.28e16

# a line with a Stark width of 0.012 nm at 1e17 cm-3, known within 20%
electron_density_uncertainty(0.024, width_se = 0.002, reference_width = 0.012,
                             reference_rsd = 0.2)
#> # A tibble: 1 × 7
#>   width width_se electron_density      se   rsd   lower   upper
#>   <dbl>    <dbl>            <dbl>   <dbl> <dbl>   <dbl>   <dbl>
#> 1 0.024    0.002             2e17 4.35e16 0.217 1.31e17 3.05e17
```
