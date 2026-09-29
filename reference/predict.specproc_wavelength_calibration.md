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
