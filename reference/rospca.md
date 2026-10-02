# Robust Sparse Principal Component Analysis (ROSPCA)

Robust sparse PCA by the ROSPCA approach of Hubert, Reynkens, Schmitt
and Verdonck (2016): outliers are detected as in
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
and sparse loadings are computed from the clean observations by a
grid-search sparse PCA (Croux, Filzmoser and Fritz, 2013). Sparse
loadings are zero for most variables, which makes the components easier
to interpret, for example as a few emission lines. The computational
kernels are written in C++.

## Usage

``` r
rospca(
  x,
  k = 2,
  lambda = 1,
  alpha = 0.75,
  ndir = 250,
  stand = FALSE,
  ngrid = 10,
  maxiter = 10
)
```

## Arguments

- x:

  A numeric matrix or data frame, with one observation per row.

- k:

  The number of principal components. Default is 2.

- lambda:

  A non-negative number: the sparsity parameter. Default is 1.

- alpha:

  The robustness parameter: the fraction of observations the estimates
  are based on, between 0.5 and 1. Default is 0.75. The subset size is
  \\h = \max(\lfloor\alpha n\rfloor, \lfloor(n + k\_{max} +
  1)/2\rfloor)\\.

- ndir:

  The number of random directions used for the outlyingness. Default is
  250; use `"all"` for all directions through pairs of observations.

- stand:

  A logical value: standardize the variables robustly (median and
  \\Q_n\\) before the analysis (`FALSE`, default) or only center them by
  their median.

- ngrid:

  The number of angles in the grid search. Default is 10.

- maxiter:

  The maximum number of grid refinements per component. Default is 10.

## Value

An object of class `specproc_rospca` (inheriting from
`specproc_robpca`), with the components described in
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
and:

- `scale`: the scales that standardize the variables before projection:
  the robust scale (\\Q_n\\, if `stand = TRUE`) times the standard
  deviation in \\H_2\\.

- `lambda`: the sparsity parameter.

- `H2`, `H3`: logical vectors of the observations in these subsets.

## Details

The algorithm follows the published description in three steps:

1.  **Outlier detection.** The variables are robustly standardized
    (median and \\Q_n\\) if `stand = TRUE`. As in
    [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
    the `h` least outlying observations form \\H_0\\, and those whose
    orthogonal distance to the \\k\\-dimensional PCA subspace of \\H_0\\
    is below the cut-off form \\H_1\\

2.  **Sparsification.** The observations of \\H_1\\ are standardized and
    the sparse loadings are computed by maximizing, component by
    component, the variance of the scores minus `lambda` times the
    \\L_1\\ norm of the loadings. Variables with zero loadings on all
    components are set aside; the observations whose orthogonal distance
    to the sparse subspace is below the cut-off form \\H_2\\, and the
    sparse loadings are recomputed from \\H_2\\, standardized in turn.

3.  **Eigenvalues and center.** The eigenvalues are first estimated by
    the squared \\Q_n\\ of the scores of \\H_2\\. The `h` observations
    of \\H_2\\ with the smallest score distances form \\H_3\\; the
    center is their mean, and the final eigenvalues are the variances of
    their scores. The components are sorted by decreasing eigenvalue.

Larger `lambda` values give sparser loadings. The loadings apply to the
data standardized by `center` and `scale`; they are unit vectors, but
those of different components are not exactly orthogonal. The
eigenvalues are the variances of the scores in these standardized units.
`lambda` is best chosen by validation, for example with
[`step_rospca()`](https://christiangoueguel.com/specProc/reference/step_rospca.md)
and tune.

This is an independent implementation of the published description, not
a port of the rospca code, so results can differ from those of
[`rospca::rospca()`](https://rdrr.io/pkg/rospca/man/rospca.html).

## References

- Hubert, M., Reynkens, T., Schmitt, E., Verdonck, T. (2016). Sparse PCA
  for high-dimensional data with outliers. Technometrics, 58(4):424-434.

- Croux, C., Filzmoser, P., Fritz, H. (2013). Robust sparse principal
  component analysis. Technometrics, 55(2):202-214.

## See also

[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`step_rospca()`](https://christiangoueguel.com/specProc/reference/step_rospca.md),
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
# the 380-430 nm window (Ca II H and K lines)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
forageLIBS[, which(wl > 380 & wl < 430)] |>
  center() |>
  rospca() |>
  print()
#> Robust sparse PCA (ROSPCA, lambda = 1)
#> 
#> Observations:   368 (h = 276)
#> Variables:      594
#> Components:     2
#> Eigenvalues:    374.97  13.91
#> Non-zero loadings per component: 594 187
#> 
#> Outlier types:
#> 
#>            regular      good leverage orthogonal outlier       bad leverage 
#>                276                 35                 30                 27 
```
