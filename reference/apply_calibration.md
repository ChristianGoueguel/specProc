# Apply a Wavelength Calibration

Relabels the wavelengths of spectra with the correction fitted by
[`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md).
The intensities are not changed.

## Usage

``` r
apply_calibration(spectra, calibration)
```

## Arguments

- spectra:

  Spectra with the wavelength axis of the calibration: a numeric vector
  named by wavelength, or a matrix or data frame with the wavelengths as
  column names (other columns are kept unchanged).

- calibration:

  The result of
  [`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md).

## Value

`spectra`, with the corrected wavelengths as names.

## Details

The spectra must have the wavelength axis of the calibration (the same
channels, in the same order), so that each channel gets the correction
of its detector segment. The corrected wavelengths are rounded to 4
decimals (0.1 pm) for the column names.

## See also

[`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md)
