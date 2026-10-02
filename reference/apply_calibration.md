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

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]
wl <- as.numeric(names(spectra))
first_detector <- spectra[seq_len(which(diff(wl) < 0)[1])]
reference <- c(`Mg I` = 285.213, `Ca II` = 317.933, `Ca II` = 393.366, `K I` = 404.414,
               `Ca I` = 422.673, `Mg I` = 518.360, `Na I` = 588.995, `Ca I` = 643.907)
cal <- wavelength_calibration(first_detector, reference)
corrected <- apply_calibration(first_detector, cal)
head(names(corrected))
#> [1] "199.3947" "199.4819" "199.5692" "199.6565" "199.7437" "199.8310"
```
