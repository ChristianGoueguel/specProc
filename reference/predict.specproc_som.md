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
# the Na I and K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which((wl > 585 & wl < 595) | (wl > 760 & wl < 780))]
fit <- som(spectra[1:300, ])
head(predict(fit, spectra[301:368, ]))
#> # A tibble: 6 × 5
#>    unit     x     y     qe novel
#>   <int> <dbl> <dbl>  <dbl> <lgl>
#> 1    23   1    2.73  6712. FALSE
#> 2     6   6    1    16826. TRUE 
#> 3    38   5.5  3.60  6112. FALSE
#> 4    13   2.5  1.87  4720. FALSE
#> 5     3   3    1     5178. FALSE
#> 6    45   1    4.46  9293. FALSE
```
