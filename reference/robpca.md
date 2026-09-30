# Robust Principal Component Analysis (ROBPCA)

Robust PCA by the ROBPCA algorithm of Hubert, Rousseeuw and Vanden
Branden (2005), which combines projection pursuit with the minimum
covariance determinant (MCD) estimator. It resists up to `n - h`
outlying spectra and works when there are more variables than
observations. The computational kernels (outlyingness, FAST-MCD) are
written in C++.

## Usage

``` r
robpca(
  x,
  k = NULL,
  kmax = 10,
  alpha = 0.75,
  ndir = 250,
  var_explained = 0.8,
  nsamp = 500
)
```

## Arguments

- x:

  A numeric matrix or data frame, with one observation per row.

- k:

  The number of principal components. If `NULL` (default), it is chosen
  from `var_explained` and `kmax`.

- kmax:

  The maximum number of components. Default is 10.

- alpha:

  The robustness parameter: the fraction of observations the estimates
  are based on, between 0.5 and 1. Default is 0.75. The subset size is
  \\h = \max(\lfloor\alpha n\rfloor, \lfloor(n + k\_{max} +
  1)/2\rfloor)\\.

- ndir:

  The number of random directions used for the outlyingness. Default is
  250; use `"all"` for all directions through pairs of observations.

- var_explained:

  The fraction of variance used to choose `k` when it is not given.
  Default is 0.8.

- nsamp:

  The number of random subsets of FAST-MCD. Default is 500.

## Value

An object of class `specproc_robpca`, a list with:

- `loadings`: \\p \times k\\ matrix of robust loadings.

- `eigenvalues`: robust eigenvalues (variances of the scores).

- `center`: robust center.

- `scores`: \\n \times k\\ matrix of scores.

- `sd`, `od`: score and orthogonal distances of each observation.

- `cutoff_sd`, `cutoff_od`: their cut-offs.

- `outlier_type`: a factor classifying each observation as `"regular"`,
  `"good leverage"` (high SD only), `"orthogonal outlier"` (high OD
  only) or `"bad leverage"` (both).

- `k`, `h`, `alpha`: the settings used.

- `H0`, `H1`: logical vectors of the observations in the subsets \\H_0\\
  and \\H_1\\.

Use
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_robpca.md)
for the scores and distances of new observations.

## Details

The algorithm follows the published description:

1.  The data are reduced by a singular value decomposition to the affine
    subspace they span (at most \\n - 1\\ dimensions).

2.  The Stahel-Donoho outlyingness of every observation is computed over
    directions through pairs of observations, using the univariate MCD
    location and scale on each direction. The `h` least outlying
    observations form the subset \\H_0\\.

3.  The number of components `k` is chosen (if not given) as the
    smallest number that explains `var_explained` of the variance of
    \\H_0\\, with at most `kmax`. Observations whose orthogonal distance
    to the \\k\\-dimensional PCA subspace of \\H_0\\ is below the
    cut-off form \\H_1\\, and the subspace is re-estimated from \\H_1\\.

4.  All observations are projected onto this subspace, and the
    reweighted FAST-MCD estimator of the scores gives the final center,
    loadings and eigenvalues.

Each observation then has a score distance (SD), its robust Mahalanobis
distance within the PCA subspace, and an orthogonal distance (OD) to the
subspace. The cut-off for the SD is \\\sqrt{\chi^2\_{k,0.975}}\\; the
cut-off for the OD uses the Wilson-Hilferty approximation, with the
univariate MCD of \\OD^{2/3}\\.
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
plots both.

This is an independent implementation of the published algorithm, not a
port of the rospca or rrcov code, so its results can differ slightly
from theirs (random directions and subsets, and small-sample correction
factors, which are not applied here). Use
[`set.seed()`](https://rdrr.io/r/base/Random.html) for reproducible
results.

## References

- Hubert, M., Rousseeuw, P.J., Vanden Branden, K. (2005). ROBPCA: a new
  approach to robust principal component analysis. Technometrics,
  47(1):64-79.

- Rousseeuw, P.J., Van Driessen, K. (1999). A fast algorithm for the
  minimum covariance determinant estimator. Technometrics,
  41(3):212-223.

## See also

[`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
for sparse loadings,
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
for data with cellwise outliers or missing values,
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)
for a recipe step,
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md).

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
forageLIBS |>
  dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
  center() |>
  robpca() |>
  print()
#> Robust PCA (ROBPCA)
#> 
#> Observations:   368 (h = 276)
#> Variables:      7152
#> Components:     3
#> Eigenvalues:    2.645e+09 5.893e+08 1.863e+08
#> 
#> Outlier types:
#> 
#>            regular      good leverage orthogonal outlier       bad leverage 
#>                314                  6                 34                 14 
```
