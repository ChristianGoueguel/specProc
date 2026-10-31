# Plot a Spectral Response

Plots the relative spectral response measured by
[`response_calibration()`](https://christiangoueguel.com/specProc/reference/response_calibration.md),
with a band of plus or minus one standard uncertainty.

## Usage

``` r
plot_response(
  object,
  title = NULL,
  wavelength = NULL,
  xlab = "Wavelength (nm)",
  ylab = "Relative response",
  base_size = 11,
  linewidth = 0.6
)
```

## Arguments

- object:

  The result of
  [`response_calibration()`](https://christiangoueguel.com/specProc/reference/response_calibration.md).

- title:

  The plot title. By default, the number of lamp spectra and the median
  uncertainty.

- wavelength:

  Optional (`NULL` by default). The wavelength range `c(min, max)` to
  draw, in nm.

- xlab, ylab:

  The axis titles. Default are `"Wavelength (nm)"` and
  `"Relative response"`.

- base_size:

  The size of the text, in points. Default is 11.

- linewidth:

  The width of the response line. Default is 0.6.

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
