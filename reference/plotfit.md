# Plotting of Fitted Spectral Line

Plots the data and the fitted lineshape returned by
[`peakfit()`](https://christiangoueguel.com/specProc/reference/peakfit.md)
or
[`multipeakfit()`](https://christiangoueguel.com/specProc/reference/multipeakfit.md),
together with the residuals. For multi-peak fits, the individual peak
contributions are drawn as dashed lines. When several spectra were
fitted, one panel is drawn per spectrum.

## Usage

``` r
plotfit(
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

  Data to be displayed from the `peakfit` or `multipeakfit` function

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
# \donttest{
if (requireNamespace("patchwork", quietly = TRUE)) {
  wl <- seq(395, 397, by = 0.02)
  set.seed(1)
  spec <- 10 + gaussian(wl, y0 = 0, xc = 396.15, wG = 0.2, A = 50) + rnorm(length(wl))
  df <- as.data.frame(t(spec))
  names(df) <- wl
  plotfit(peakfit(df, profile = "gaussian"))
}

# }
```
