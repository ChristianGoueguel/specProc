# Robust PCA for Cellwise and Casewise Outliers (MacroPCA)

Robust PCA that handles both outlying observations (casewise outliers),
outlying cells (cellwise outliers) and missing values, by the MacroPCA
algorithm of Hubert, Rousseeuw and Van den Bossche (2019), which
combines the detection of deviating cells (DDC) with the steps of ROBPCA
(Hubert, Rousseeuw and Vanden Branden, 2005). The results have the same
form as those of
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
so that
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_robpca.md)
and
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
work the same way, and
[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)
shows the outlying cells.
[`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md)
starts from a MacroPCA fit.

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
  tol = 0.005
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

  The maximum number of iterations of step 4, and the tolerance on the
  largest angle between successive subspaces (as a fraction of a right
  angle). Defaults are 20 and 0.005, as in the paper.

## Value

An object of class `specproc_macropca` (inheriting from
`specproc_robpca`), with the components described in
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
and:

- `std_resid`: the standardized cell residuals (`NA` for missing cells).

- `flagged_cells`: a logical matrix of the flagged cells.

- `flagged_rows`: the observations flagged by DDC.

- `imputed`: the data with the missing values imputed by the PCA fit
  (the flagged cells keep their values).

- `resid_scale`: the robust scale of the residuals of each variable,
  used to standardize those of new data.

## Details

The algorithm follows Hubert, Rousseeuw and Van den Bossche (2019) and
the MacroPCA code of the cellWise package, whose results it reproduces
(with `scale = FALSE`): 0. **Deviating cells.** The cells that deviate
from the values predicted by the most correlated variables, and the
outlying observations, are detected by the DDC of Rousseeuw and Van den
Bossche (2018), computed in C++ as in cellWise (for more than 750
variables, the neighbors are those with the largest wrapped
correlations, found exactly by blocks of variables). DDC also imputes
the flagged and missing cells. Of the observations flagged by DDC, at
most the \\n - h\\ most outlying are set aside.

1.  **Standardization.** With `scale = TRUE`, the variables are divided
    by their robust scale (1-step M-estimator).

2.  **Projection pursuit.** As in
    [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
    the outlyingness of each observation is its largest standardized
    distance (with the univariate MCD) over `ndir` directions through
    pairs of observations (all pairs when there are few), on the data in
    which only the `h` observations with the fewest flagged cells have
    their flagged cells imputed. The `h` least outlying observations not
    set aside form \\H_0\\.

3.  **Subspace dimension.** A classical PCA of the observations of
    \\H_0\\, with their flagged and missing cells imputed, gives the
    eigenvalues: when `k` is `NULL`, it is the smallest number of
    components that explain `var_explained` of their variance (at most
    `kmax`).

4.  **Iterative subspace estimation.** The flagged and missing cells are
    imputed by the fitted values of the current PCA, and the PCA of
    \\H_0\\ is refitted, until the largest angle between the old and the
    new subspace (as a fraction of a right angle) is below `tol` (at
    most `maxiter` times).

5.  **Reweighting.** The observations whose orthogonal distance is below
    the cut-off, and not set aside, form \\H^\*\\, and the PCA is
    refitted on them (with their flagged cells imputed).

6.  **Robust basis.** The center and the eigenvectors within the
    subspace are estimated by concentration steps on the scores of
    \\H^\*\\ followed by the deterministic MCD (DetMCD), so that good
    leverage observations do not tilt the loadings.

7.  **Distances.** The scores and the distances of all the observations
    are computed from the data with only the missing cells imputed. The
    cut-off of the orthogonal distances is computed on the data whose
    observations of \\H^\*\\ have their flagged cells imputed.

8.  **Residuals.** The residuals of the observed cells are standardized
    by their 1-step M scale, and the cells beyond \\\sqrt{\chi^2\_{1,
    0.99}}\\ are flagged.

As in the paper, the cut-offs of the score distances
(\\\sqrt{\chi^2\_{k, 0.99}}\\) and of the orthogonal distances (the
Wilson-Hilferty approximation with the univariate MCD and the 0.99
quantile) are at the 99% level. New observations are analyzed as in
MacroPCApredict of cellWise: their deviating cells are detected with the
DDC model of the fit, and their flagged and missing cells are imputed
iteratively by the fit before their distances are computed.

## References

- Hubert, M., Rousseeuw, P.J., Van den Bossche, W. (2019). MacroPCA: an
  all-in-one PCA method allowing for missing values as well as cellwise
  and rowwise outliers. Technometrics, 61(4):459-473.
  [doi:10.1080/00401706.2018.1562989](https://doi.org/10.1080/00401706.2018.1562989)

- Hubert, M., Rousseeuw, P.J., Vanden Branden, K. (2005). ROBPCA: a new
  approach to robust principal component analysis. Technometrics,
  47(1):64-79.

- Rousseeuw, P.J., Van den Bossche, W. (2018). Detecting deviating data
  cells. Technometrics, 60(2):135-145.

- Hubert, M., Rousseeuw, P.J., Verdonck, T. (2012). A deterministic
  algorithm for robust location and scatter. Journal of Computational
  and Graphical Statistics, 21(3):618-637.

## See also

[`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md),
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
#> Eigenvalues:    1.934e+09 5.125e+08 1.366e+08
#> Flagged cells:  60069
#> 
#> Outlier types:
#> 
#>            regular      good leverage orthogonal outlier       bad leverage 
#>                267                  9                 77                 15 
# }
```
