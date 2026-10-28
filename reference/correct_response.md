# Correct Spectra or Line Intensities for the Spectral Response

Divides spectra, or the intensities of emission lines, by the relative
spectral response of the instrument measured by
[`response_calibration()`](https://christiangoueguel.com/specProc/reference/response_calibration.md).

## Usage

``` r
correct_response(x, response)
```

## Arguments

- x:

  Spectra (a numeric vector named by wavelength, or a matrix or data
  frame with one spectrum per row and the wavelengths as column names),
  or a table of lines with `wavelength` and `intensity` columns.

- response:

  The result of
  [`response_calibration()`](https://christiangoueguel.com/specProc/reference/response_calibration.md).

## Value

`x`, corrected. For a table of lines, with the corrected `intensity`
(and `height`), its standard error `intensity_se`, and the `response`
used. Channels or lines without response become `NA`.

## Details

Spectra must have the wavelength axis of the response (the same
channels, in the same order): each channel is divided by its response.

A table of lines, such as returned by
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md),
is recognized by its `wavelength` and `intensity` columns. The
`intensity` (and `height`) of each line is divided by the response at
the channel of its peak (`peak_wavelength`, or else the channel nearest
to its `wavelength`), and its standard error `intensity_se` (created if
absent) combines in quadrature its relative standard error and that of
the response. Correcting the line intensities rather than the spectra
keeps the noise estimates of
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)
valid. The errors of the response of nearby lines are correlated, which
[`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md)
does not account for.

Correct the intensities for the response before
[`correct_self_absorption()`](https://christiangoueguel.com/specProc/reference/correct_self_absorption.md),
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)
or
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md).

## See also

[`response_calibration()`](https://christiangoueguel.com/specProc/reference/response_calibration.md),
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)

## Examples

``` r
planck <- function(wl, t) 1 / (wl^5 * (exp(1.4388e7 / (wl * t)) - 1))
reference <- data.frame(wavelength = seq(250, 850, by = 10))
reference$value <- planck(reference$wavelength, 3100)
reference$rsd <- 0.03
wl <- seq(260, 840, by = 0.1)
true_response <- exp(-((wl - 500) / 250)^2)
set.seed(1)
lamp <- 2e4 * true_response * planck(wl, 3100) / planck(500, 3100) + rnorm(length(wl), sd = 20)
names(lamp) <- wl
response <- response_calibration(lamp, reference)

# a spectrum with two lines of equal emission, at 300 and 650 nm
emission <- 1000 * exp(-(wl - 300)^2 / 0.01) + 1000 * exp(-(wl - 650)^2 / 0.01)
spectrum <- stats::setNames(true_response * emission + rnorm(length(wl)), wl)
lines <- line_intensities(spectrum, c(a = 300, b = 650))
lines$intensity   # unequal: the response differs
#> [1] 72.05381 95.34854
corrected <- correct_response(lines, response)
corrected[c("line", "intensity", "intensity_se", "response")]
#> # A tibble: 2 × 4
#>   line  intensity intensity_se response
#>   <chr>     <dbl>        <dbl>    <dbl>
#> 1 a          135.         5.14    0.534
#> 2 b          137.         4.10    0.698

# or the whole spectrum
corrected_spectrum <- correct_response(spectrum, response)
```
