# Doppler Width of an Emission Line

Computes the full width at half maximum of the Doppler (thermal)
broadening of a line emitted by atoms or ions of a given mass at a given
temperature.

## Usage

``` r
doppler_width(wavelength, temperature, mass)
```

## Arguments

- wavelength:

  The wavelength of the line, in nm.

- temperature:

  The temperature of the emitters, in K.

- mass:

  The mass of the emitter: its atomic mass, in u, or its element symbol
  (such as `"Ca"`), for the standard atomic weight.

## Value

A numeric vector of Doppler widths (FWHM), in nm. The arguments are
recycled to a common length.

## Details

The thermal motion of the emitters gives a Gaussian profile of full
width at half maximum \$\$w_D = \lambda \sqrt{\frac{8 k_B T \ln 2}{m
c^2}}\$\$ where \\\lambda\\ is the wavelength, \\T\\ the temperature and
\\m\\ the mass of the emitter. In LIBS plasmas, it is usually much
smaller than the instrumental width, except for light elements such as
hydrogen. Both are Gaussian and add in quadrature, \\w_G =
\sqrt{w\_{inst}^2 + w_D^2}\\, the Gaussian width to hold fixed in a
Voigt fit (see
[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)).

## See also

[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md),
[`voigt_fwhm()`](https://christiangoueguel.com/specProc/reference/voigt_fwhm.md)

## Author

Christian L. Goueguel

## Examples

``` r
# Ca II 393.37 nm and H-alpha at 10000 K
doppler_width(c(393.37, 656.28), temperature = 10000, mass = c("Ca", "H"))
#> [1] 0.004450434 0.046818017

# with an instrumental width of 0.05 nm: the Gaussian width of a Voigt fit
sqrt(0.05^2 + doppler_width(393.37, 10000, "Ca")^2)
#> [1] 0.05019767
```
