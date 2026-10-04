# Predictions of a Robust SIMCA Model

Predicts the classes of new observations with a model fitted by
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md),
or their distances to the classes.

## Usage

``` r
# S3 method for class 'specproc_rsimca'
predict(object, newdata, type = c("class", "distances"), ...)
```

## Arguments

- object:

  An object returned by
  [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md).

- newdata:

  A numeric matrix or data frame with the same variables as the training
  data.

- type:

  `"class"` (default) for the predicted classes, or `"distances"` for
  the combined distances to the classes.

- ...:

  Not used.

## Value

With `type = "class"`, a factor of the predicted classes. With
`type = "distances"`, a tibble with the combined distance of each
observation to each class (one column per class), and `outlying`, `TRUE`
for the observations outlying for every class.

## See also

[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 380 & wl < 430)]
level <- cut(forageLIBS$Ca, c(-Inf, 0.6, Inf), labels = c("low", "high"))
set.seed(1)
fit <- rsimca(spectra[1:300, ], level[1:300], ncomp = 3)
head(predict(fit, spectra[301:368, ], type = "distances"))
#> # A tibble: 6 × 3
#>     low  high outlying
#>   <dbl> <dbl> <lgl>   
#> 1 0.857 3.98  FALSE   
#> 2 1.41  3.55  TRUE    
#> 3 0.405 0.533 FALSE   
#> 4 0.469 1.73  FALSE   
#> 5 0.494 0.759 FALSE   
#> 6 0.761 2.51  TRUE    
```
