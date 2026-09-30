# Robust PCA for Cellwise and Casewise Outliers (MacroPCA)

Robust PCA that handles both outlying observations (casewise outliers),
outlying cells (cellwise outliers) and missing values, by the MacroPCA
algorithm of Hubert, Rousseeuw and Van den Bossche (2019). This function
is a wrapper around
[`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html)
that returns the results in the same form as
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
so that
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_robpca.md)
and
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
work the same way, and adds
[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md).

## Usage

``` r
macropca(x, k = NULL, alpha = 0.5, kmax = 10, var_explained = 0.8, ...)
```

## Arguments

- x:

  A numeric matrix or data frame, with one observation per row. Missing
  values are allowed.

- k:

  The number of principal components. If `NULL` (default), it is chosen
  from `var_explained` and `kmax`.

- alpha:

  The robustness parameter, between 0.5 and 1. Default is 0.5.

- kmax:

  The maximum number of components. Default is 10.

- var_explained:

  The fraction of variance used to choose `k` when it is not given.
  Default is 0.8.

- ...:

  Further parameters of MacroPCA, passed in `MacroPCApars` (see
  [`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html)),
  for example `maxdir`, or `scale = TRUE` to scale the variables (by
  default, they are only centered, unlike in
  [`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html),
  so that intense emission lines are not outweighed by noise and
  continuum channels).

## Value

An object of class `specproc_macropca` (inheriting from
`specproc_robpca`), with the components described in
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
and:

- `std_resid`: the standardized cell residuals.

- `flagged_cells`: a logical matrix of the flagged cells.

- `imputed`: the data with outlying cells and missing values imputed.

- `fit`: the object returned by
  [`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html).

## Details

MacroPCA first detects outlying cells with the DetectDeviatingCells
algorithm, imputes them and the missing values, and then iterates a
robust PCA that down-weights outlying observations. The standardized
residuals of each cell show which cells deviate from the PCA fit; they
are displayed by
[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md).

Spectra often have many more channels than observations; MacroPCA's DDC
step can then be slow, and averaging adjacent channels first helps.

When `k` is `NULL`, MacroPCA is run a first time to estimate the
variance explained by up to `kmax` components, and `k` is the smallest
number of components that explain `var_explained` of it (or `kmax`, if
none does), as in
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md).
MacroPCA is then run again with this `k`, so giving `k` halves the
computing time.

## References

- Hubert, M., Rousseeuw, P.J., Van den Bossche, W. (2019). MacroPCA: an
  all-in-one PCA method allowing for missing values as well as cellwise
  and rowwise outliers. Technometrics, 61(4):459-473.

## See also

[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`step_macropca()`](https://christiangoueguel.com/specProc/reference/step_macropca.md),
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md),
[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
# LIBS spectra of forage samples (MacroPCA is run twice to choose k)
minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
set.seed(1)
forageLIBS |>
  dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
  center() |>
  macropca() |>
  print()
#> Robust PCA for cellwise and casewise outliers (MacroPCA)
#> 
#> Observations:   368 (h = 186)
#> Variables:      7152
#> Components:     3
#> Eigenvalues:    1.951e+09 5.207e+08 1.389e+08
#> Flagged cells:  59386
#> 
#> Outlier types:
#> 
#>            regular      good leverage orthogonal outlier       bad leverage 
#>                266                 10                 78                 14 
```
