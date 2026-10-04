# Predictions of a Robust PCR Model

Predicts the responses of new observations with a model fitted by
[`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md), or
computes their scores.

## Usage

``` r
# S3 method for class 'specproc_rpcr'
predict(object, newdata, type = c("response", "scores"), ncomp = NULL, ...)
```

## Arguments

- object:

  An object returned by
  [`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md).

- newdata:

  A numeric matrix or data frame with the same variables as the
  calibration data.

- type:

  `"response"` (default) for the predicted responses, or `"scores"` for
  the scores and their score and orthogonal distances.

- ncomp:

  With `type = "response"`, the number of components of the predictions,
  from 1 to the `kmax` of the model. Default is the `ncomp` of the
  model. The models with fewer or more components use the same robust
  PCA (see
  [`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md)).

- ...:

  Not used.

## Value

With `type = "response"`, a numeric vector of predictions (one response)
or a matrix with one column per response. With `type = "scores"`, a
tibble with the scores (`Comp1`, ...), the score distance `sd` and the
orthogonal distance `od` of each observation.

## See also

[`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 380 & wl < 430)]
set.seed(1)
fit <- rpcr(spectra[1:300, ], forageLIBS$Ca[1:300], ncomp = 5)
head(predict(fit, spectra[301:368, ]))
#> [1] 0.4693860 0.3976267 0.6559014 0.5791613 0.7150952 0.3257995
head(predict(fit, spectra[301:368, ], type = "scores"))
#> # A tibble: 6 × 7
#>     Comp1  Comp2   Comp3  Comp4   Comp5    sd    od
#>     <dbl>  <dbl>   <dbl>  <dbl>   <dbl> <dbl> <dbl>
#> 1 -75682. -2806. -14836.  1082.   -36.5  3.23 5893.
#> 2 -61523. 16754. -16108.  1342. -1467.   3.61 7167.
#> 3 -25741. -1352.   3296. -4067. -6537.   2.43 5076.
#> 4 -47613. -3065.  -8407. -1544. -1743.   2.08 5455.
#> 5 -32271. -8880.    184. -5290. -4690.   2.44 4033.
#> 6 -50785.  -192.  -7612.  7918. -2650.   2.88 8824.
```
