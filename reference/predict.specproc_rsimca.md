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
# iris: train on 100 flowers, predict the 50 others
set.seed(1)
train <- sample(nrow(iris), 100)
fit <- rsimca(iris[train, 1:4], iris$Species[train], ncomp = 2)
head(predict(fit, iris[-train, 1:4]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(predict(fit, iris[-train, 1:4], type = "distances"))
#> # A tibble: 6 × 4
#>   setosa versicolor virginica outlying
#>    <dbl>      <dbl>     <dbl> <lgl>   
#> 1 0.0893       7.77      16.8 FALSE   
#> 2 0.214        6.41      14.7 FALSE   
#> 3 0.0472      10.1       19.0 FALSE   
#> 4 0.0234       8.37      17.2 FALSE   
#> 5 0.275        5.68      14.0 FALSE   
#> 6 0.0947      10.7       20.0 FALSE   
```
