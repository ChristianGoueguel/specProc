# Robust Cross-Validation of a Robust Calibration Model

Robust root mean squared error of cross-validation (R-RMSECV) of the
robust PLS
([`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md))
or robust PCR
([`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md))
models with 1 to `kmax` components, and the robust component selection
(RCS) criterion of Engelen and Hubert (2005), to choose the number of
components when the calibration data contain outliers.

## Usage

``` r
robust_rmsecv(
  x,
  y,
  method = c("rsimpls", "rpcr"),
  kmax = 10,
  folds = 10,
  gamma = 0.5,
  alpha = 0.75,
  ndir = 250,
  nsamp = 500,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame of the predictors (spectra), one
  observation per row.

- y:

  A numeric vector, matrix or data frame of the responses, with one row
  per observation.

- method:

  The model: `"rsimpls"` (default), robust PLS, or `"rpcr"`, robust PCR.

- kmax:

  The largest number of components. Default is 10.

- folds:

  The number of folds of the cross-validation, from 2 to the number of
  observations (leave-one-out). Default is 10.

- gamma:

  The weight of the cross-validated error in the RCS criterion, between
  0 and 1. Default is 0.5.

- alpha:

  The robustness parameter of
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md):
  the fraction of observations assumed to be regular, between 0.5 and 1.
  Default is 0.75.

- ndir:

  The number of random directions of the outlyingness in
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md).
  Default is 250.

- nsamp:

  The number of random subsets of FAST-MCD (and, for `"rpcr"`, of the
  LTS or MCD regression). Default is 500.

- ...:

  Not used.

## Value

A tibble with one row per number of components: `ncomp`, the robust `R2`
and `RMSE` of the fit to all the observations (as the `components` of
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
and
[`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md)),
the robust `RMSECV` and the `RCS` criterion. The weights \\w_i\\ are its
`"weights"` attribute.

## Details

The squared prediction errors of outliers would dominate the usual
RMSECV. The robust RMSECV leaves them out: the model is first fitted to
all the observations, and only those regular in every model with 1 to
`kmax` components (residual distance below its cut-off) enter the error,
\$\$R\text{-}RMSECV_k = \sqrt{\frac{1}{q \sum_i w_i} \sum_i w_i \\y_i -
\hat y\_{-i,k}\\^2},\$\$ where \\w_i\\ is 1 for these observations and 0
for the others, and \\\hat y\_{-i,k}\\ is the prediction of observation
\\i\\ by the model with \\k\\ components fitted without it.

The robust root mean squared error of the fit to the calibration data,
`RMSE` (the square root of the robust residual sum of squares, R-RSS),
decreases with the number of components, while the cross-validated error
eventually increases. The RCS criterion combines them, \$\$RCS_k =
\sqrt{\gamma \\ R\text{-}RMSECV_k^2 + (1 - \gamma) \\
R\text{-}RSS_k},\$\$ and its first local minimum, or the point after
which it decreases little, suggests the number of components.
`gamma = 1` gives the robust RMSECV, and `gamma = 0` the R-RSS.

**Differences from the paper.** The paper computes the leave-one-out
errors with a fast approximation that updates the robust fits instead of
refitting them. Here, each fold is refitted. Leave-one-out
(`folds = nrow(x)`) is then slow, and K-fold cross-validation (default
10 folds) is used instead.

The folds are fitted in parallel when a parallel plan is set with
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
(and the future.apply package is installed). The folds and the fits are
random: use [`set.seed()`](https://rdrr.io/r/base/Random.html) for
reproducible results.

## References

- Engelen, S., Hubert, M. (2005). Fast model selection for robust
  calibration methods. Analytica Chimica Acta, 544(1-2):219-228.

## See also

[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md),
[`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md)

## Author

Christian L. Goueguel

## Examples

``` r
# \donttest{
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 380 & wl < 430)]
set.seed(1)
robust_rmsecv(spectra[1:300, ], forageLIBS$Ca[1:300], kmax = 8, folds = 5)
#> # A tibble: 8 × 5
#>   ncomp    R2   RMSE RMSECV    RCS
#>   <int> <dbl>  <dbl>  <dbl>  <dbl>
#> 1     1 0.169 0.140  0.142  0.141 
#> 2     2 0.500 0.109  0.114  0.111 
#> 3     3 0.661 0.0895 0.0933 0.0914
#> 4     4 0.740 0.0784 0.0811 0.0798
#> 5     5 0.756 0.0760 0.0830 0.0796
#> 6     6 0.755 0.0762 0.0838 0.0801
#> 7     7 0.755 0.0762 0.0835 0.0799
#> 8     8 0.753 0.0765 0.0815 0.0790
# }
```
