# Robust PCA for Cellwise and Casewise Outliers (MacroPCA)

Robust PCA that handles both outlying observations (casewise outliers),
outlying cells (cellwise outliers) and missing values, after the
MacroPCA algorithm of Hubert, Rousseeuw and Van den Bossche (2019). The
results have the same form as those of
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
so that
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_robpca.md)
and
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
work the same way, and
[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)
shows the outlying cells.

## Usage

``` r
macropca(
  x,
  k = NULL,
  alpha = 0.5,
  kmax = 10,
  var_explained = 0.8,
  scale = FALSE,
  ndir = 250,
  maxiter = 20,
  tol = 1e-04
)
```

## Arguments

- x:

  A numeric matrix or data frame, with one observation per row. Missing
  values are allowed.

- k:

  The number of principal components. If `NULL` (default), it is chosen
  from `var_explained` and `kmax`.

- alpha:

  The robustness parameter, between 0.5 and 1: the fraction of
  observations used in the fit. Default is 0.5.

- kmax:

  The maximum number of components. Default is 10.

- var_explained:

  The fraction of variance used to choose `k` when it is not given.
  Default is 0.8.

- scale:

  A logical: scale the variables by their robust scale (`FALSE`,
  default: they are only centered, so that intense emission lines are
  not outweighed by noise and continuum channels).

- ndir:

  The number of random directions of the outlyingness, or `"all"`.
  Default is 250.

- maxiter, tol:

  The maximum number of iterations and the tolerance on the change of
  the subspace. Defaults are 20 and 1e-4.

## Value

An object of class `specproc_macropca` (inheriting from
`specproc_robpca`), with the components described in
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
and:

- `std_resid`: the standardized cell residuals (`NA` for missing cells).

- `flagged_cells`: a logical matrix of the flagged cells.

- `imputed`: the data with the missing values imputed by the PCA fit
  (the flagged cells keep their values).

- `resid_center`, `resid_scale`: the robust center and scale of the
  residuals of each variable, used to standardize those of new data.

## Details

The algorithm has four steps:

1.  **Deviating cells.** The cells that deviate from the values
    predicted by the most correlated variables are detected and imputed
    by the detection of deviating cells (DDC) of Rousseeuw and Van den
    Bossche (2018), as are the missing values. The neighbor search and
    the predictions of DDC are computed in C++, by blocks of variables,
    so that spectra with thousands of channels are handled quickly.

2.  **Initial subspace.** The `h` observations with the smallest
    Stahel-Donoho outlyingness (as in
    [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
    with `ndir` directions) give an initial PCA of the imputed data.
    When `k` is `NULL`, it is the smallest number of components that
    explain `var_explained` of the variance of these observations (at
    most `kmax`).

3.  **Iterations.** The flagged and missing cells are imputed by the
    fitted values of the current PCA, the observations within the
    cut-off of the orthogonal distances are kept, and the PCA is
    refitted on them, until the subspace changes by less than `tol` (at
    most `maxiter` times).

4.  **Final fit.** The center and the eigenvalues are re-estimated by
    the minimum covariance determinant of the scores, as in
    [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md).

The residuals of each variable are robustly standardized (median and
MAD), and the cells beyond \\\sqrt{\chi^2\_{1, 0.99}}\\ are flagged. The
scores of each observation are then computed with its flagged and
missing cells imputed by the fit (iteratively, as for new data with
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_robpca.md)),
while its orthogonal distance and cell residuals use its observed cells,
so that the deviating cells of an observation count in its orthogonal
distance. The cut-offs of the score and orthogonal distances are those
of
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
so that the outlier maps of the robust PCA methods of specProc can be
compared.

## References

- Hubert, M., Rousseeuw, P.J., Van den Bossche, W. (2019). MacroPCA: an
  all-in-one PCA method allowing for missing values as well as cellwise
  and rowwise outliers. Technometrics, 61(4):459-473.

- Rousseeuw, P.J., Van den Bossche, W. (2018). Detecting deviating data
  cells. Technometrics, 60(2):135-145.

- Raymaekers, J., Rousseeuw, P.J. (2021). Fast robust correlation for
  high-dimensional data. Technometrics, 63(2):184-198.

## See also

[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`step_macropca()`](https://christiangoueguel.com/specProc/reference/step_macropca.md),
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md),
[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)

## Author

Christian L. Goueguel

## Examples

``` r
# \donttest{
set.seed(1)
# LIBS spectra of forage samples
minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
forageLIBS |>
  dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
  center() |>
  macropca() |>
  print()
#> Robust PCA for cellwise and casewise outliers (MacroPCA)
#> 
#> Observations:   368 (h = 189)
#> Variables:      7152
#> Components:     3
#> Eigenvalues:    2.467e+09 5.716e+08 1.225e+08
#> Flagged cells:  72011
#> 
#> Outlier types:
#> 
#>            regular      good leverage orthogonal outlier       bad leverage 
#>                301                 16                 41                 10 
# }
```
