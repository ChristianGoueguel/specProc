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
# iris: train on 100 flowers, predict the 50 others
set.seed(1)
train <- sample(nrow(iris), 100)
fit <- robust_da(iris[train, 1:4], iris$Species[train], method = "quadratic")
head(predict(fit, iris[-train, 1:4]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(predict(fit, iris[-train, 1:4], type = "prob"))
#> # A tibble: 6 × 3
#>   setosa versicolor virginica
#>    <dbl>      <dbl>     <dbl>
#> 1      1   2.09e-38  1.70e-54
#> 2      1   1.45e-33  3.66e-48
#> 3      1   1.77e-50  1.08e-62
#> 4      1   3.77e-43  2.34e-56
#> 5      1   1.18e-28  5.02e-45
#> 6      1   1.50e-53  7.72e-65
```
