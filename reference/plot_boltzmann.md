# Draw a Boltzmann or Saha-Boltzmann Plot

Plots the points and the fitted line of
[`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md)
or
[`saha_boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann_plot.md),
with the estimated temperature, or the parallel Boltzmann plots of the
species of
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md).

## Usage

``` r
plot_boltzmann(object, title = NULL)
```

## Arguments

- object:

  An object returned by
  [`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md),
  [`saha_boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann_plot.md)
  or
  [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md).

- title:

  The plot title. By default, the method and temperature.

## Value

A ggplot object.

## See also

[`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md),
[`saha_boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann_plot.md)

## Examples

``` r
kB <- 8.617333262e-5
lines <- data.frame(wavelength = c(400, 450, 500), Aki = c(1e8, 2e7, 3e7),
                    gk = c(3, 7, 9), Ek = c(3.1, 4.6, 6.0))
lines$intensity <- with(lines, gk * Aki / wavelength * exp(-Ek / (kB * 9000)))
plot_boltzmann(boltzmann_plot(lines))
```
