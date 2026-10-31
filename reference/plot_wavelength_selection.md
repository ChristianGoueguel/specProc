# Plot of a Wavelength Selection

Shows the variables selected by
[`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md):
the mean spectrum, with the selected regions shaded, above the
importance of each variable (VIP or selectivity ratio, with the
threshold when one was used) or, for iPLS, the RMSECV of each interval
alone, with the selected intervals colored.

## Usage

``` r
plot_wavelength_selection(
  object,
  title = NULL,
  wavelength = NULL,
  xlab = NULL,
  base_size = 11,
  linewidth = 0.3
)
```

## Arguments

- object:

  An object returned by
  [`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md).

- title:

  The plot title.

- wavelength:

  Optional (`NULL` by default). The wavelength range `c(min, max)` to
  draw, in nm. Needs variables named by wavelength.

- xlab:

  The title of the wavelength axis. Default is `"Wavelength (nm)"` (or
  `"Variable"`). The panels are named on their left.

- base_size:

  The size of the text, in points. Default is 11.

- linewidth:

  The width of the mean spectrum and importance lines. Default is 0.3.

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

# the Ca II H and K lines
plot_wavelength_selection(sel, wavelength = c(390, 400))
```
