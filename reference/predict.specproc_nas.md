# Net Analyte Signal of New Samples

Computes the net analyte signal, selectivity and predicted concentration
of new samples with a model fitted by
[`nas()`](https://christiangoueguel.com/specProc/reference/nas.md).

## Usage

``` r
# S3 method for class 'specproc_nas'
predict(object, newdata, ...)
```

## Arguments

- object:

  An object returned by
  [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md).

- newdata:

  A numeric matrix or data frame of new spectra, with the same variables
  as the calibration spectra.

- ...:

  Not used.

## Value

A tibble with one row per new sample and columns `nas`, `selectivity`,
`predicted` and, if the model has a `noise` level, `snr`.

## See also

[`nas()`](https://christiangoueguel.com/specProc/reference/nas.md)
