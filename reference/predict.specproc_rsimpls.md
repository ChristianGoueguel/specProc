# Predictions of a Robust PLS Model

Predicts the responses of new observations with a model fitted by
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md),
or computes their scores.

## Usage

``` r
# S3 method for class 'specproc_rsimpls'
predict(object, newdata, type = c("response", "scores"), ...)
```

## Arguments

- object:

  An object returned by
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md).

- newdata:

  A numeric matrix or data frame with the same variables as the
  calibration data.

- type:

  `"response"` (default) for the predicted responses, or `"scores"` for
  the scores and their score distances.

- ...:

  Not used.

## Value

With `type = "response"`, a numeric vector of predictions (one response)
or a matrix with one column per response. With `type = "scores"`, a
tibble with the scores (`Comp1`, ...) and the score distance `sd` of
each observation.

## See also

[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 380 & wl < 430)]
set.seed(1)
fit <- rsimpls(spectra[1:300, ], forageLIBS$Ca[1:300], ncomp = 4)
head(predict(fit, spectra[301:368, ]))
#> [1] 0.4159052 0.5470408 0.7232786 0.6207605 0.8097063 0.1862145
head(predict(fit, spectra[301:368, ], type = "scores"))
#> # A tibble: 6 × 5
#>     Comp1  Comp2   Comp3  Comp4    sd
#>     <dbl>  <dbl>   <dbl>  <dbl> <dbl>
#> 1 -71399.  7157.  -5198. -4782.  3.49
#> 2 -59582.  1583.   2839.  -564.  2.22
#> 3 -24817.  3041.   4451.  2834.  1.82
#> 4 -44576.  6857.   -438.   191.  1.90
#> 5 -29412.  8679.   3571.  4413.  2.55
#> 6 -50417. -3468. -10568. -6265.  3.93
```
