# Map New Spectra onto a Self-Organizing Map

Places new spectra on the best-matching unit of a map fitted by
[`som()`](https://christiangoueguel.com/specProc/reference/som.md), with
their quantization error, and flags the spectra that fit no unit.

## Usage

``` r
# S3 method for class 'specproc_som'
predict(object, newdata, ...)
```

## Arguments

- object:

  A map fitted by
  [`som()`](https://christiangoueguel.com/specProc/reference/som.md).

- newdata:

  A numeric matrix or data frame of spectra, with the variables of the
  training data.

- ...:

  Not used.

## Value

A tibble with one row per spectrum: its best-matching `unit`, the
coordinates `x` and `y` of the unit on the map, the quantization error
`qe`, and `novel`, `TRUE` when the error exceeds the cut-off of the
training spectra (the spectrum fits no known group).

## See also

[`som()`](https://christiangoueguel.com/specProc/reference/som.md),
[`plot_som()`](https://christiangoueguel.com/specProc/reference/plot_som.md)

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]
fit <- som(spectra[1:300, ])
head(predict(fit, spectra[301:368, ]))
#> # A tibble: 6 × 5
#>    unit     x     y     qe novel
#>   <int> <dbl> <dbl>  <dbl> <lgl>
#> 1    24  12.5  1.87 29413. FALSE
#> 2    72  12.5  5.33 39763. TRUE 
#> 3    46  10.5  3.60 23005. FALSE
#> 4    36  12    2.73 16719. FALSE
#> 5    47  11.5  3.60 19377. FALSE
#> 6    11  11    1    34501. FALSE
```
