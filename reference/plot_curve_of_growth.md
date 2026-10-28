# Plot a Curve of Growth

Draws the curve of growth fitted by
[`curve_of_growth()`](https://christiangoueguel.com/specProc/reference/curve_of_growth.md):
the reduced intensity of each line against its optical depth at the line
center, on logarithmic axes, with the optically thin limit and the curve
of growth of the median line profile.

## Usage

``` r
plot_curve_of_growth(object, title = NULL)
```

## Arguments

- object:

  The result of
  [`curve_of_growth()`](https://christiangoueguel.com/specProc/reference/curve_of_growth.md).

- title:

  The plot title. By default, the temperature and the largest optical
  depth.

## Value

A ggplot object.

## Details

The reduced intensity, \\I\\V(\lambda_0) / (F B\_\lambda(T))\\, equals
the optical depth for an optically thin line, and bends below it as the
line saturates. Points of all species lie on the same curve when the
model holds (up to the differences of their line profiles), as in the
C-sigma graphs of Aragón and Aguilera (2014).

## See also

[`curve_of_growth()`](https://christiangoueguel.com/specProc/reference/curve_of_growth.md)

## Examples

``` r
lines <- data.frame(
  species = "Ca I", wavelength = c(400, 405, 520, 560, 610, 650),
  Aki = c(1.5e8, 1.2e8, 3e7, 5e7, 4e7, 2e7), gk = c(4, 2, 6, 4, 3, 5),
  Ei = c(0, 0, 2.0, 2.1, 2.2, 2.4), stark_width = 0.02
)
lines$Ek <- lines$Ei + 1239.842 / lines$wavelength
lines$intensity <- curve_of_growth_model(lines, 9000, scale = 1e20, optical_depth = 5)$intensity
plot_curve_of_growth(curve_of_growth(lines, temperature = 9000))
```
