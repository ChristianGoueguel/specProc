# Predictions of a Robust Discriminant Analysis

Predicts the classes, or the posterior probabilities of the classes, of
new observations with a model fitted by
[`robust_da()`](https://christiangoueguel.com/specProc/reference/robust_da.md).

## Usage

``` r
# S3 method for class 'specproc_robust_da'
predict(object, newdata, type = c("class", "prob"), ...)
```

## Arguments

- object:

  An object returned by
  [`robust_da()`](https://christiangoueguel.com/specProc/reference/robust_da.md).

- newdata:

  A numeric matrix or data frame with the same variables as the training
  data.

- type:

  `"class"` (default) for the predicted classes, or `"prob"` for the
  posterior probabilities.

- ...:

  Not used.

## Value

With `type = "class"`, a factor of the predicted classes. With
`type = "prob"`, a tibble with one column of posterior probabilities per
class.

## See also

[`robust_da()`](https://christiangoueguel.com/specProc/reference/robust_da.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
lines <- forageLIBS[c("393.3599236", "396.8602175")]
level <- cut(forageLIBS$Ca, c(-Inf, 0.6, Inf), labels = c("low", "high"))
set.seed(1)
fit <- robust_da(lines[1:300, ], level[1:300], method = "quadratic")
head(predict(fit, lines[301:368, ], type = "prob"))
#> # A tibble: 6 × 2
#>     low   high
#>   <dbl>  <dbl>
#> 1 0.956 0.0441
#> 2 0.871 0.129 
#> 3 0.609 0.391 
#> 4 0.795 0.205 
#> 5 0.666 0.334 
#> 6 0.847 0.153 
```
