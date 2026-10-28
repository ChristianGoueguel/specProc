# Transfer Spectra to Another Instrument

Transfers spectra measured on the secondary instrument to the primary
instrument, with a transfer fitted by
[`ds()`](https://christiangoueguel.com/specProc/reference/ds.md),
[`sst()`](https://christiangoueguel.com/specProc/reference/sst.md) or
[`pds()`](https://christiangoueguel.com/specProc/reference/pds.md).

## Usage

``` r
# S3 method for class 'specproc_transfer'
predict(object, newdata, ...)
```

## Arguments

- object:

  A transfer returned by
  [`ds()`](https://christiangoueguel.com/specProc/reference/ds.md),
  [`sst()`](https://christiangoueguel.com/specProc/reference/sst.md) or
  [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md).

- newdata:

  Spectra of the secondary instrument: a numeric matrix or data frame
  with the wavelengths of the transfer spectra.

- ...:

  Not used.

## Value

The transferred spectra, a matrix (or a tibble for a data frame) with
the wavelengths of the primary spectra.

## See also

[`ds()`](https://christiangoueguel.com/specProc/reference/ds.md),
[`sst()`](https://christiangoueguel.com/specProc/reference/sst.md),
[`pds()`](https://christiangoueguel.com/specProc/reference/pds.md)

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- as.matrix(forageLIBS[which(wl > 760 & wl < 780)])
secondary <- 0.8 * cbind(spectra[, -1], spectra[, ncol(spectra)]) + 50
transfer <- pds(spectra[1:30, ], secondary[1:30, ], win = 3)
transferred <- predict(transfer, secondary[31:40, ])
```
