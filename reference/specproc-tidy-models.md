# Tidy and Glance Methods for Chemometric Models

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) lists the
estimates of a chemometric model in a long tibble, and
[`glance()`](https://generics.r-lib.org/reference/glance.html)
summarizes the model in a one-row tibble, following the conventions of
the broom package. Through parsnip, they also apply to the fitted models
and workflows of the `"rsimpls"`, `"lts"`, `"rsimca"` and `"mcd"`
engines.

## Usage

``` r
# S3 method for class 'specproc_robpca'
tidy(x, matrix = "eigenvalues", ...)

# S3 method for class 'specproc_robpca'
glance(x, ...)

# S3 method for class 'specproc_rsimpls'
tidy(x, matrix = "coefficients", ...)

# S3 method for class 'specproc_rsimpls'
glance(x, ...)

# S3 method for class 'specproc_rpcr'
tidy(x, matrix = "coefficients", ...)

# S3 method for class 'specproc_rpcr'
glance(x, ...)

# S3 method for class 'specproc_lts'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'specproc_lts'
glance(x, ...)

# S3 method for class 'specproc_rsimca'
tidy(x, ...)

# S3 method for class 'specproc_rsimca'
glance(x, ...)

# S3 method for class 'specproc_robust_da'
tidy(x, ...)

# S3 method for class 'specproc_robust_da'
glance(x, ...)

# S3 method for class 'specproc_opls'
tidy(x, matrix = "coefficients", ...)

# S3 method for class 'specproc_opls'
glance(x, ...)

# S3 method for class 'specproc_som'
tidy(x, ...)

# S3 method for class 'specproc_som'
glance(x, ...)

# S3 method for class 'specproc_wavelength_selection'
tidy(x, ...)

# S3 method for class 'specproc_wavelength_selection'
glance(x, ...)

# S3 method for class 'specproc_nas'
tidy(x, ...)

# S3 method for class 'specproc_nas'
glance(x, ...)

# S3 method for class 'specproc_domain'
tidy(x, ...)

# S3 method for class 'specproc_domain'
glance(x, ...)
```

## Arguments

- x:

  An object returned by
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md),
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
  [`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md),
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md),
  [`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md),
  [`lts_fit()`](https://christiangoueguel.com/specProc/reference/linear_reg_lts.md),
  [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md),
  [`robust_da()`](https://christiangoueguel.com/specProc/reference/robust_da.md),
  [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md),
  [`som()`](https://christiangoueguel.com/specProc/reference/som.md),
  [`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md),
  [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md) or
  [`applicability_domain()`](https://christiangoueguel.com/specProc/reference/applicability_domain.md).

- matrix:

  The table to return (see Details).

- ...:

  Not used.

- conf.int:

  A logical: add the confidence interval (`FALSE`, default).

- conf.level:

  The confidence level of the interval. Default is 0.95.

## Value

A tibble.

## Details

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns,
choosing the table with `matrix` when a model has several:

- robust PCA
  ([`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md),
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
  [`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md)):
  `"eigenvalues"` (default), one row per component `PC` with its
  `eigenvalue` and `std.dev`; `"loadings"`, the `value` of each variable
  (`column`) on each `PC`; `"scores"`, the `value` of each observation
  (`row`) on each `PC`, as
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) of a
  `prcomp` object in broom (whose names of the tables are also accepted,
  such as `"rotation"` and `"x"`);

- [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
  and
  [`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md):
  `"coefficients"` (default), the `estimate` of each `term` (the
  intercept and the variables) for each `response`, in the units of the
  responses; `"components"`, the robust `r.squared` and the `rmse` of
  the models with 1 to `kmax` components (`ncomp`); `"loadings"` and
  `"scores"`, as for robust PCA;

- [`lts_fit()`](https://christiangoueguel.com/specProc/reference/linear_reg_lts.md):
  the `term`, `estimate`, `std.error`, `statistic` and `p.value` of the
  reweighted least-squares fit, and, with `conf.int = TRUE`, its
  confidence interval (`conf.low`, `conf.high`);

- [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)
  and
  [`robust_da()`](https://christiangoueguel.com/specProc/reference/robust_da.md):
  one row per `class`, with its `prior`, the number `n` of its regular
  observations and their misclassification rate `error`, and, for
  RSIMCA, its number of components `ncomp` and the cutoffs of the score
  and orthogonal distances (`cutoff.sd`, `cutoff.od`);

- [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md):
  `"coefficients"` (default), the `estimate` of each `term` (variable)
  with its `vip` and `ortho.vip`; `"components"`, the explained
  variances `r2x`, `r2y` and `q2` of each component, their cumulative
  values (`r2x.cum`, ...) and their `significance`;

- [`som()`](https://christiangoueguel.com/specProc/reference/som.md):
  one row per `unit` of the map, with its `row` and `col` in the grid,
  its position (`x`, `y`), the number `n` of observations it represents
  and their mean quantization error `qe`;

- [`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md):
  one row per variable (`term`), with its `importance`, whether it is
  `selected`, and, for iPLS, its `interval`;

- [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md):
  the regression vector, the `estimate` of each `term`;

- [`applicability_domain()`](https://christiangoueguel.com/specProc/reference/applicability_domain.md):
  one row per component `PC` of the PCA, with its `std.dev`, `percent`
  of the variance and `cumulative` percent (as fractions).

[`glance()`](https://generics.r-lib.org/reference/glance.html) returns
the number of observations `nobs` and of variables `nvar`, and:

- robust PCA: the number of components `k`, `h`, `alpha`, the cutoffs
  `cutoff.sd` and `cutoff.od`, the numbers of `n.good.leverage`,
  `n.orthogonal.outlier` and `n.bad.leverage` observations, the number
  of `flagged.cells` (MacroPCA and cellPCA) and the `method`;

- [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
  and
  [`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md):
  `ncomp`, `kmax`, `h`, `alpha`, the robust `r.squared`, the robust
  scale of the residuals `sigma` (one response), the numbers of
  `n.good.leverage`, `n.vertical.outlier` and `n.bad.leverage`
  observations, the `method` and, for RPCR, the `regression`;

- [`lts_fit()`](https://christiangoueguel.com/specProc/reference/linear_reg_lts.md):
  the robust `r.squared`, the robust scale of the residuals `sigma`,
  `df.residual`, `alpha` and the number of `n.outliers` (weight 0 in the
  reweighted fit);

- [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)
  and
  [`robust_da()`](https://christiangoueguel.com/specProc/reference/robust_da.md):
  the number of `n.classes`, the overall misclassification rate `error`,
  `alpha`, the number of observations left out (`n.outliers`; for
  RSIMCA, those outlying for all classes) and the `method`;

- [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md):
  the numbers of predictive and orthogonal components (`ncomp`,
  `ncomp.ortho`), `r2x.cum`, `r2y.cum`, `q2.cum`, `rmsee` and, after a
  permutation test, its p-values `p.r2y` and `p.q2`;

- [`som()`](https://christiangoueguel.com/specProc/reference/som.md):
  the number of `n.units`, the `topology`, the `quantization.error` and
  `topographic.error` of the map, the number of `n.novel` observations
  (beyond the cutoff of the quantization error), the `epochs` and
  whether the map is `robust`;

- [`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md):
  the `method`, `ncomp`, the number of selected variables `n.selected`,
  `robust`, the `threshold` and, for iPLS, the `rmsecv` of the final
  model;

- [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md):
  the `method`, `ncomp` and the figures of merit (`sensitivity`,
  `selectivity` and, with a noise level, `analytical.sensitivity`, `lod`
  and `loq`);

- [`applicability_domain()`](https://christiangoueguel.com/specProc/reference/applicability_domain.md):
  the number of components `k`, the variance `explained`, the
  `conf.level` and the limits `t2.limit` and `q.limit`.

## See also

[specproc-tidy](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
for the fits, calibrations and plasma diagnostics

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- as.numeric(names(forageLIBS[-(1:14)]))
spectra <- forageLIBS[-(1:14)][, wl > 380 & wl < 430]
set.seed(1)
fit <- rsimpls(spectra[1:300, ], forageLIBS$Ca[1:300], ncomp = 4)
tidy(fit, matrix = "components")
#> # A tibble: 10 × 3
#>    ncomp r.squared   rmse
#>    <int>     <dbl>  <dbl>
#>  1     1     0.211 0.141 
#>  2     2     0.491 0.113 
#>  3     3     0.689 0.0885
#>  4     4     0.761 0.0776
#>  5     5     0.762 0.0775
#>  6     6     0.768 0.0765
#>  7     7     0.767 0.0765
#>  8     8     0.767 0.0766
#>  9     9     0.767 0.0765
#> 10    10     0.775 0.0753
glance(fit)
#> # A tibble: 1 × 12
#>    nobs  nvar ncomp  kmax     h alpha r.squared  sigma n.good.leverage
#>   <int> <int> <int> <int> <int> <dbl>     <dbl>  <dbl>           <int>
#> 1   300   594     4    10   225  0.75     0.738 0.0918              39
#> # ℹ 3 more variables: n.vertical.outlier <int>, n.bad.leverage <int>,
#> #   method <chr>
# the regression vector, one row per wavelength
head(tidy(fit))
#> # A tibble: 6 × 3
#>   response term            estimate
#>   <chr>    <chr>              <dbl>
#> 1 y        (Intercept)  1.61       
#> 2 y        380.011254  -0.000000637
#> 3 y        380.096143  -0.00000117 
#> 4 y        380.202312  -0.000000911
#> 5 y        380.285294  -0.000000907
#> 6 y        380.3682761 -0.00000108 

pca <- robpca(spectra, k = 3)
tidy(pca)
#> # A tibble: 3 × 3
#>      PC eigenvalue std.dev
#>   <int>      <dbl>   <dbl>
#> 1     1 786105711.  28038.
#> 2     2  54636943.   7392.
#> 3     3  39024510.   6247.
glance(pca)
#> # A tibble: 1 × 12
#>    nobs  nvar     k     h alpha cutoff.sd cutoff.od n.good.leverage
#>   <int> <int> <int> <int> <dbl>     <dbl>     <dbl>           <int>
#> 1   368   594     3   276  0.75      3.06    12301.              39
#> # ℹ 4 more variables: n.orthogonal.outlier <int>, n.bad.leverage <int>,
#> #   flagged.cells <int>, method <chr>
```
