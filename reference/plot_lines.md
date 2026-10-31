# Overlay Emission Lines on a Spectrum

Plots a spectrum with vertical markers at the wavelengths of candidate
emission lines, colored by ionization stage, and at the heads of
molecular bands, to identify the lines and bands of the spectrum.

## Usage

``` r
plot_lines(
  spectrum,
  lines = NULL,
  shift = 0,
  scale_markers = TRUE,
  interactive = TRUE,
  title = NULL,
  bands = NULL,
  wavelength = NULL,
  xlab = "Wavelength (nm)",
  ylab = "Intensity (arb. units)",
  base_size = 11,
  linewidth = 0.3,
  spectrum_color = "grey35",
  marker_linewidth = 0.5,
  marker_alpha = 0.7
)
```

## Arguments

- spectrum:

  A spectrum: a numeric vector named by wavelength, or a one-row matrix
  or data frame whose column names are the wavelengths. Average several
  spectra first.

- lines:

  A tibble returned by
  [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md),
  or `NULL` to draw only molecular bands.

- shift:

  A wavelength shift, in nm, added to the line and band wavelengths to
  correct the wavelength calibration of the spectrometer. Default is 0.

- scale_markers:

  A logical: scale the markers by the relative intensity of the lines
  (`TRUE`, default) or draw them full height.

- interactive:

  A logical: plotly (`TRUE`, default) or ggplot2.

- title:

  An optional plot title.

- bands:

  An optional tibble of molecular band heads returned by
  [`molecular_bands()`](https://christiangoueguel.com/specProc/reference/molecular_bands.md).

- wavelength:

  Optional (`NULL` by default). The wavelength range `c(min, max)` to
  draw, in nm. The markers are scaled to the maximum of the spectrum
  within the range, and the lines and bands outside it are left out.

- xlab, ylab:

  The axis titles. Default are `"Wavelength (nm)"` and
  `"Intensity (arb. units)"`.

- base_size:

  The size of the text, in points. Default is 11. Ignored for
  interactive plots.

- linewidth:

  The width of the spectrum line. Default is 0.3.

- spectrum_color:

  The color of the spectrum. Default is `"grey35"`.

- marker_linewidth:

  The width of the line and band markers. Default is 0.5.

- marker_alpha:

  The opacity of the line and band markers, between 0 and 1. Default is
  0.7.

## Value

A plotly or ggplot object.

## Details

With `interactive = TRUE`, the plot is made with plotly: the spectrum is
drawn with WebGL, the lines of each ionization stage form one trace that
can be shown or hidden from the legend, and hovering over a line shows
its species, wavelength, transition probability, upper-level energy and
accuracy grade. Otherwise, a ggplot is returned.

With `scale_markers = TRUE`, the height of each marker is its relative
intensity (see
[`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md))
times the maximum of the spectrum; otherwise all markers span the full
height.

The heads of molecular bands (`bands`, see
[`molecular_bands()`](https://christiangoueguel.com/specProc/reference/molecular_bands.md))
are drawn as dashed markers, one color per molecule, scaled by their
relative intensity within their system.

## See also

[`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md),
[`molecular_bands()`](https://christiangoueguel.com/specProc/reference/molecular_bands.md),
[`line_finder()`](https://christiangoueguel.com/specProc/reference/line_finder.md)

## Examples

``` r
data(forageLIBS)
spectrum <- colMeans(forageLIBS[-(1:14)])
# the K I doublet, with the atomic data of the NIST database
lines <- tibble::tibble(
  species = "K I", element = "K", stage = 1L, wavelength = c(404.414, 404.721),
  relative_intensity = c(1, 0.5), Aki = c(1.15e6, 1.07e6), Ek = c(3.065, 3.063),
  gk = c(4, 2), accuracy = "A", lower = "4s 2S", upper = c("5p 2P* 3/2", "5p 2P* 1/2")
)
plot_lines(spectrum, lines, wavelength = c(403, 406), interactive = FALSE)

```
