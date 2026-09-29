# Plot a Wavelength Calibration

Plots the offset of each reference line (reference minus measured
wavelength) against its measured wavelength, with the fitted correction
of each detector segment. Lines not used are drawn as open circles and
labeled with the reason.

## Usage

``` r
plot_wavelength_calibration(object, title = NULL)
```

## Arguments

- object:

  The result of
  [`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md).

- title:

  The plot title.

## Value

A ggplot object.

## See also

[`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md)
