# Plot of a Wavelength Selection

Shows the variables selected by
[`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md):
the mean spectrum, with the selected regions shaded, above the
importance of each variable (VIP or selectivity ratio, with the
threshold when one was used) or, for iPLS, the RMSECV of each interval
alone, with the selected intervals colored.

## Usage

``` r
plot_wavelength_selection(object, title = NULL)
```

## Arguments

- object:

  An object returned by
  [`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md).

- title:

  The plot title.

## Value

A ggplot object.

## See also

[`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 380 & wl < 430)]
sel <- select_wavelengths(spectra, forageLIBS$Ca, method = "sr", num_terms = 50)
plot_wavelength_selection(sel)
```
