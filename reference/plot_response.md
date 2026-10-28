# Plot a Spectral Response

Plots the relative spectral response measured by
[`response_calibration()`](https://christiangoueguel.com/specProc/reference/response_calibration.md),
with a band of plus or minus one standard uncertainty.

## Usage

``` r
plot_response(object, title = NULL)
```

## Arguments

- object:

  The result of
  [`response_calibration()`](https://christiangoueguel.com/specProc/reference/response_calibration.md).

- title:

  The plot title. By default, the number of lamp spectra and the median
  uncertainty.

## Value

A ggplot object.

## See also

[`response_calibration()`](https://christiangoueguel.com/specProc/reference/response_calibration.md)

## Examples

``` r
planck <- function(wl, t) 1 / (wl^5 * (exp(1.4388e7 / (wl * t)) - 1))
reference <- data.frame(wavelength = seq(250, 850, by = 10))
reference$value <- planck(reference$wavelength, 3100)
reference$rsd <- 0.03
wl <- seq(260, 840, by = 0.1)
set.seed(1)
lamp <- 2e4 * exp(-((wl - 500) / 250)^2) * planck(wl, 3100) / planck(500, 3100) +
  rnorm(length(wl), sd = 20)
names(lamp) <- wl
plot_response(response_calibration(lamp, reference))
```
