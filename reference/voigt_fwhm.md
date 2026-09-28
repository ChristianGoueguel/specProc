# Full Width at Half Maximum of a Voigt Profile

Computes the full width at half maximum (FWHM) of a Voigt profile from
its Gaussian and Lorentzian widths, with the approximation of Olivero
and Longbothum (1977), accurate to about 0.02%: \$\$f_V \approx
0.5346\\f_L + \sqrt{0.2166\\f_L^2 + f_G^2}\$\$

## Usage

``` r
voigt_fwhm(wG, wL)
```

## Arguments

- wG, wL:

  The Gaussian and Lorentzian full widths at half maximum, for example
  the `wG` and `wL` parameters of a Voigt fit with
  [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md).
  Vectors are recycled.

## Value

A numeric vector of Voigt FWHM, in the units of `wG` and `wL`.

## Details

The total width of a fitted line is often better determined than its
split into Gaussian and Lorentzian parts, which trade off against each
other when a line is noisy or sampled by few channels. It is the width
to use for the H\\\alpha\\ line in
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md),
whose Stark profile is not Lorentzian.

## References

- Olivero, J.J., Longbothum, R.L. (1977). Empirical fits to the Voigt
  line width: a brief review. Journal of Quantitative Spectroscopy and
  Radiative Transfer, 17(2):233-236.

## See also

[`voigt_profile()`](https://christiangoueguel.com/specProc/reference/voigt_profile.md),
[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md),
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md)

## Author

Christian L. Goueguel

## Examples

``` r
voigt_fwhm(wG = 0.1, wL = 0)    # Gaussian limit
#> [1] 0.1
voigt_fwhm(wG = 0, wL = 0.1)    # Lorentzian limit
#> [1] 0.1000003
voigt_fwhm(wG = 0.1, wL = 0.1)
#> [1] 0.1637596
```
