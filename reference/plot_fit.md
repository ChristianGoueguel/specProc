# Plotting of Fitted Spectral Line

Plots the data and the fitted lineshape returned by
[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)
or
[`multipeak_fit()`](https://christiangoueguel.com/specProc/reference/multipeak_fit.md),
together with the residuals. The fitted curve is drawn on a fine
wavelength grid, so it shows the fitted profile between the measured
channels. For multi-peak fits, the individual peak contributions are
drawn as dashed lines. When several spectra were fitted, one panel is
drawn per spectrum.

## Usage

``` r
plot_fit(
  data,
  title = NULL,
  pt.size = 3,
  pt.colour = "black",
  pt.shape = 21,
  pt.fill = "black",
  line.size = 1,
  line.colour = "red",
  linetype = "solid",
  resid.shape = 21,
  resid.size = 2,
  resid.fill = "blue",
  resid.colour = "black"
)
```

## Arguments

- data:

  Data to be displayed from the `peak_fit` or `multipeak_fit` function

- title:

  Plot title

- pt.size:

  Size of original data point

- pt.colour:

  Colour of original data points

- pt.shape:

  Shape of original data points

- pt.fill:

  Colour fill of original data points

- line.size:

  Size of the fitted data line

- line.colour:

  Colour of the fitted data line

- linetype:

  Type of the fitted data line

- resid.shape:

  Shape of the residuals data points

- resid.size:

  Size of the residuals data points

- resid.fill:

  Colour fill of the residuals data points

- resid.colour:

  Colour of the residuals data points

## Value

A `patchwork` (ggplot2) object.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
mean_spectrum <- colMeans(forageLIBS[-(1:14)])
wl <- as.numeric(names(mean_spectrum))
halpha <- tibble::as_tibble(as.list(mean_spectrum[wl > 653.5 & wl < 659.5]))
plot_fit(peak_fit(halpha, profile = "lorentzian"), title = "H-alpha 656.28 nm")
```
