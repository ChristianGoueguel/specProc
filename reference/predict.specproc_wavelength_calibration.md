# Correct Wavelengths with a Wavelength Calibration

Converts measured wavelengths to calibrated ones with the correction
fitted by
[`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md).

## Usage

``` r
# S3 method for class 'specproc_wavelength_calibration'
predict(object, wavelength, ...)
```

## Arguments

- object:

  The result of
  [`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md).

- wavelength:

  Measured wavelengths, in nm.

- ...:

  Not used.

## Value

The corrected wavelengths. Each wavelength gets the correction of the
detector segment whose range contains it (the first one where detectors
overlap), or of the nearest segment.

## See also

[`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md),
[`apply_calibration()`](https://christiangoueguel.com/specProc/reference/apply_calibration.md)

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]
wl <- as.numeric(names(spectra))
first_detector <- spectra[seq_len(which(diff(wl) < 0)[1])]
reference <- c(`Mg I` = 285.213, `Ca II` = 317.933, `Ca II` = 393.366, `K I` = 404.414,
               `Ca I` = 422.673, `Mg I` = 518.360, `Na I` = 588.995, `Ca I` = 643.907)
cal <- wavelength_calibration(first_detector, reference)
# the corrected wavelengths of the Ca II H and K lines
predict(cal, c(393.3, 396.8))
#> [1] 393.3549 396.8556
```
