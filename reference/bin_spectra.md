# Binning of Adjacent Channels

Reduces the number of channels of spectra by replacing each group of
`width` adjacent channels by their mean.

## Usage

``` r
bin_spectra(x, width = 3, segments = TRUE)
```

## Arguments

- x:

  A numeric matrix or data frame of spectra, one per row, or a numeric
  vector (one spectrum). Their names are the wavelengths.

- width:

  The number of adjacent channels averaged, a positive integer. Default
  is 3; `width = 1` returns the spectra unchanged.

- segments:

  A logical: bin the segments between gaps of the wavelength axis
  separately (`TRUE`, default). Needs wavelengths as names.

## Value

The binned spectra, of the same type as `x` (a tibble for a data frame),
with `ceiling(n / width)` channels for each detector segment of `n`
channels.

## Details

Averaging keeps the intensities on their scale and divides the noise of
independent channels by about `sqrt(width)`, at the cost of spectral
resolution: keep `width` smaller than the width of the emission lines (a
few channels in LIBS spectra) to keep them resolved. Fewer channels also
make the models faster to fit.

The channels are grouped from the first one; when their number is not a
multiple of `width`, the last group is smaller. When `segments = TRUE`,
the spectrum is split between the detectors of a multi-spectrometer
system (where the wavelengths step back, or jump by more than 5 times
the median spacing), and each detector is binned on its own, so that no
group straddles two detectors. The binned channels are named by their
mean wavelength (to 10 significant digits), or, when the names are not
wavelengths, by the name of their first channel; a channel left alone in
its group keeps its name.

## References

- Vrábel, J., Képeš, E., Duponchel, L., et al. (2020). Classification of
  challenging laser-induced breakdown spectroscopy soil sample data -
  EMSLIBS contest. Spectrochimica Acta Part B, 169:105872.

## See also

[`step_bin_spectra()`](https://christiangoueguel.com/specProc/reference/step_bin_spectra.md),
[`gap_derivative()`](https://christiangoueguel.com/specProc/reference/gap_derivative.md),
[`average()`](https://christiangoueguel.com/specProc/reference/average.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[1:3, -(1:14)]
binned <- bin_spectra(spectra, width = 3)
ncol(spectra)
#> [1] 7152
ncol(binned)
#> [1] 2384
names(binned)[1:3]
#> [1] "199.4644141" "199.7261717" "199.9879292"
```
