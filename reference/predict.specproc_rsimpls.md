# Predictions of a Robust PLS Model

Predicts the responses of new observations with a model fitted by
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md),
or computes their scores.

## Usage

``` r
# S3 method for class 'specproc_rsimpls'
predict(object, newdata, type = c("response", "scores"), ncomp = NULL, ...)
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
  the scores and their score and orthogonal distances.

- ncomp:

  With `type = "response"`, the number of components of the predictions,
  from 1 to the `kmax` of the model. Default is the `ncomp` of the
  model. The models with fewer or more components come from the same
  ROBPCA fit (see
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)).

- ...:

  Not used.

## Value

With `type = "response"`, a numeric vector of predictions (one response)
or a matrix with one column per response. With `type = "scores"`, a
tibble with the scores (`Comp1`, ...), the score distance `sd` and the
orthogonal distance `od` of each observation.

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
head(predict(fit, spectra[301:368, ], ncomp = 2))
#> [1] 0.5726315 0.5169454 0.6104433 0.6242335 0.6821011 0.4632534
head(predict(fit, spectra[301:368, ], type = "scores"))
#> # A tibble: 6 × 6
#>     Comp1  Comp2   Comp3  Comp4    sd     od
#>     <dbl>  <dbl>   <dbl>  <dbl> <dbl>  <dbl>
#> 1 -71399.  7157.  -5198. -4782.  3.49  5342.
#> 2 -59582.  1583.   2839.  -564.  2.22 23184.
#> 3 -24817.  3041.   4451.  2834.  1.82  8393.
#> 4 -44576.  6857.   -438.   191.  1.90  7613.
#> 5 -29412.  8679.   3571.  4413.  2.55  6447.
#> 6 -50417. -3468. -10568. -6265.  3.93 10371.
```
