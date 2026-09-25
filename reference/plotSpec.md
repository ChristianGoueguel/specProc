# Plotting of Spectra

Spectrum plots are commonly x–y plots in which the x-axis represents the
wavelength and the y-axis represents intensity of a spectrum's signal.
The function allows to plot a spectrum or several spectra in a single
plot, identified either by an id (for example, the samples or spectra
id) or by a target variable (for example, the concentration of a
chemical element).

## Usage

``` r
plotSpec(x, id = NULL, colvar = NULL, .interactive = FALSE, drop_na = FALSE)
```

## Arguments

- x:

  data frame or tibble of the spectra.

- id:

  optional (`NULL` by default). Column name of a factor variable that
  identified each spectrum.

- colvar:

  optional (`NULL` by default). Column name of a numeric variable to be
  display in color scale.

- .interactive:

  optional (`FALSE` by default). When set to `TRUE` enables interactive
  plot.

- drop_na:

  Optional (`FALSE` by default). Remove rows with NA intensity if
  drop_na is `TRUE`.

## Value

Object of class ggplot or of class plotly if `.interactive = TRUE`.

## Details

This function is based on the ggplot2 package, thus allowing users to
easily add or modify different components of the plot. Each row of `x`
is drawn as a separate line. All columns other than `id` and `colvar`
must be named by their wavelength.

## Author

Christian L. Goueguel

## Examples

``` r
wl <- seq(390, 400, length.out = 200)
spectra <- as.data.frame(t(sapply(1:3, function(i) i * exp(-(wl - 395)^2 / 0.1) + 0.1 * i)))
names(spectra) <- wl
spectra$conc <- 1:3
plotSpec(spectra, colvar = conc)

```
