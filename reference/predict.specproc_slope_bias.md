# Correct Predictions for Slope and Bias

Applies the correction fitted by
[`slope_bias()`](https://christiangoueguel.com/specProc/reference/slope_bias.md)
to new predictions.

## Usage

``` r
# S3 method for class 'specproc_slope_bias'
predict(object, newdata, ...)
```

## Arguments

- object:

  The result of
  [`slope_bias()`](https://christiangoueguel.com/specProc/reference/slope_bias.md).

- newdata:

  Predictions of the model for new slave spectra.

- ...:

  Not used.

## Value

The corrected predictions.

## See also

[`slope_bias()`](https://christiangoueguel.com/specProc/reference/slope_bias.md)

## Examples

``` r
set.seed(1)
reference <- runif(20, 0.5, 4)
predicted <- 0.2 + 1.1 * reference + rnorm(20, sd = 0.05)
predict(slope_bias(predicted, reference), c(1.3, 2.4, 3.5))
#> [1] 0.9694174 1.9887610 3.0081046
```
