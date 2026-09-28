# Self-Absorption Coefficient from Line Widths

Estimates the self-absorption coefficient of an emission line from the
ratio of its measured Stark width to the width it would have if it were
optically thin (El Sherbini et al., 2005).

## Usage

``` r
self_absorption(width, thin_width, alpha = -0.54)
```

## Arguments

- width:

  The measured Lorentzian full width at half maximum of the line, in nm.

- thin_width:

  The width of the line if it were optically thin, in nm.

- alpha:

  The exponent of the relation. Default is -0.54.

## Value

A tibble with the self-absorption coefficient `SA` and the
`intensity_correction` factor \\1/SA\\.

## Details

Self-absorption flattens and broadens a line. The self-absorption
coefficient, the ratio of the measured peak intensity to the intensity
without absorption, is \$\$SA =
\left(\frac{\Delta\lambda}{\Delta\lambda_0}\right)^{1/\alpha}, \quad
\alpha = -0.54\$\$ where \\\Delta\lambda\\ is the measured Lorentzian
(Stark) width and \\\Delta\lambda_0\\ the width of the optically thin
line. The latter follows from the electron density measured on an
optically thin line (such as H\\\alpha\\, see
[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md))
and the Stark width of the line at that density
([`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)).
\\SA = 1\\ means no self-absorption; the measured intensity divided by
\\SA\\ corrects it.

## References

- El Sherbini, A.M., El Sherbini, Th.M., Hegazy, H., Cristoforetti, G.,
  Legnaioli, S., Palleschi, V., Pardini, L., Salvetti, A., Tognoni, E.
  (2005). Evaluation of self-absorption coefficients of aluminum
  emission lines in laser-induced breakdown spectroscopy measurements.
  Spectrochimica Acta Part B, 60(12):1573-1579.

## See also

[`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md),
[`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)

## Examples

``` r
# a line twice as wide as expected from its Stark width
self_absorption(width = 0.04, thin_width = 0.02)
#> # A tibble: 1 × 4
#>   width thin_width    SA intensity_correction
#>   <dbl>      <dbl> <dbl>                <dbl>
#> 1  0.04       0.02 0.277                 3.61
```
