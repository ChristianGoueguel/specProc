# Electron Density from Stark Broadening

Estimates the electron density of a plasma from the Stark width of an
emission line: from the Stark broadening parameters of the line
(STARK-B, or a reference width), or from the width of the hydrogen
H\\\alpha\\ line at 656.28 nm.

## Usage

``` r
electron_density(
  width,
  method = "stark",
  stark = NULL,
  wavelength = NULL,
  temperature = NULL,
  reference_width = NULL,
  reference_density = 1e+17,
  tolerance = 0.5
)
```

## Arguments

- width:

  The measured Stark full width at half maximum, in nm (see Details). A
  vector is allowed.

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

- tolerance:

  For `method = "stark"` with `stark`: the largest difference between
  `wavelength` and the tabulated wavelength, in nm (see
  [`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)).
  Default is 0.5.

## Value

A numeric vector of electron densities, in cm\\^{-3}\\.

## Details

`width` is the Stark full width at half maximum, in nm, with the
instrumental and Doppler broadening removed. The line must be optically
thin: self-absorption broadens lines and overestimates the density (see
[`self_absorption()`](https://christiangoueguel.com/specProc/reference/self_absorption.md)).

- For isolated lines (`method = "stark"`), the Stark profile is
  Lorentzian: use the Lorentzian width `wL` of a Voigt fit with
  [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)
  or
  [`multipeak_fit()`](https://christiangoueguel.com/specProc/reference/multipeak_fit.md),
  whose Gaussian part absorbs the instrumental and Doppler broadening.

- The Stark profile of H\\\alpha\\ (`method = "halpha"`) is not
  Lorentzian: use the FWHM of the whole line, for example
  [`voigt_fwhm()`](https://christiangoueguel.com/specProc/reference/voigt_fwhm.md)
  of the fitted widths, corrected for the instrumental width when it is
  not negligible (for Gaussian broadening, approximately \\\sqrt{w^2 -
  w\_{inst}^2}\\).

- `method = "stark"`: in the impact approximation the Stark width is
  proportional to the electron density, \$\$N_e = N\_{ref}
  \frac{w}{w\_{ref}(T)}\$\$ where \\w\_{ref}(T)\\ is the electron-impact
  width at density \\N\_{ref}\\. It is taken from `stark` (a table
  returned by
  [`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md),
  interpolated at `temperature` with
  [`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)),
  or given directly as `reference_width` (nm) at `reference_density`.
  Tables from other sources can be built with
  [`stark_table()`](https://christiangoueguel.com/specProc/reference/stark_table.md)
  or read from saved STARK-B files with
  [`read_starkb()`](https://christiangoueguel.com/specProc/reference/read_starkb.md).
  Ion broadening is neglected, which is usually justified for lines of
  ions and at LIBS densities.

- `method = "halpha"`: the computer-simulation fit of Gigosos, González
  and Cardeñoso (2003) for the H\\\alpha\\ line, \$\$w = 1.098\\(N_e /
  10^{17})^{0.67823}\\ \mathrm{nm},\$\$ which includes the ion dynamics
  and depends only weakly on temperature.

## References

- Gigosos, M.A., González, M.Á., Cardeñoso, V. (2003). Computer
  simulated Balmer-alpha, -beta and -gamma Stark line profiles for
  non-equilibrium plasmas diagnostics. Spectrochimica Acta Part B,
  58(8):1489-1504.

- Konjević, N. (1999). Plasma broadening and shifting of non-hydrogenic
  spectral lines: present status and applications. Physics Reports,
  316(6):339-401.

## See also

[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md),
[`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md),
[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md),
[`voigt_fwhm()`](https://christiangoueguel.com/specProc/reference/voigt_fwhm.md)

## Examples

``` r
# From the H-alpha line: a Stark FWHM of 0.5 nm
electron_density(0.5, method = "halpha")
#> [1] 3.135367e+16

# From a line with known Stark width 0.012 nm at 1e17 cm-3
electron_density(c(0.006, 0.012, 0.024), reference_width = 0.012)
#> [1] 5e+16 1e+17 2e+17
```
