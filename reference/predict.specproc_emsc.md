# Apply EMSC to New Spectra

Corrects new spectra with the reference spectrum, polynomial basis and
interferents of a model fitted by
[`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md).

## Usage

``` r
# S3 method for class 'specproc_emsc'
predict(object, newdata, ...)
```

## Arguments

- object:

  An object returned by
  [`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md).

- newdata:

  A numeric matrix or data frame of new spectra, with the same variables
  as the calibration data.

- ...:

  Not used.

## Value

A tibble of corrected spectra.

## See also

[`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md),
[`predict.specproc_filter()`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 760 & wl < 780)]  # the K I resonance lines
fit <- emsc(spectra[1:300, ], degree = 2)
corrected <- predict(fit, spectra[301:368, ])
dim(corrected)
#> [1]  68 245
```
