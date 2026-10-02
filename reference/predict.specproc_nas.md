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

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 760 & wl < 780)]  # the K I resonance lines
cal <- 1:300
fit <- nas(spectra[cal, ], forageLIBS$K[cal], ncomp = 3)
head(predict(fit, spectra[-cal, ]))
#> # A tibble: 6 × 3
#>       nas selectivity predicted
#>     <dbl>       <dbl>     <dbl>
#> 1  -144.     0.00575       1.98
#> 2   522.     0.0144        2.15
#> 3   764.     0.104         2.21
#> 4   -61.3    0.00227       2.00
#> 5    25.4    0.000857      2.02
#> 6 -1329.     0.140         1.68
```
