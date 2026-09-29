# Hotelling's T-squared and Q Residuals of a PCA Model

Computes, for each sample, the two distances used to monitor a principal
component analysis (PCA) model: Hotelling's \\T^2\\, the distance within
the model plane of the first `k` components, and the Q residual (squared
prediction error, SPE), the squared distance to that plane, with their
95% and 99% limits.

## Usage

``` r
q_residuals(
  model,
  k,
  newdata = NULL,
  method = "jackson",
  center = TRUE,
  scale = FALSE
)
```

## Arguments

- model:

  A [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit that
  kept all its components (the default of
  [`prcomp()`](https://rdrr.io/r/stats/prcomp.html)), or a numeric
  matrix or data frame, on which a PCA is fitted with `center` and
  `scale`.

- k:

  The number of components of the model.

- newdata:

  Optional new samples (a matrix or data frame with the variables of the
  model), whose distances are computed instead of those of the
  calibration samples.

- method:

  The limit of Q: `"jackson"` (default, Jackson-Mudholkar) or `"box"`.

- center, scale:

  Passed to [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html)
  when `model` is data. Default is `TRUE` and `FALSE`.

## Value

A tibble of class `specproc_influence`, with one row per sample:
`sample` (row number), `t2`, `q`, their limits (`t2_limit_95`,
`t2_limit_99`, `q_limit_95`, `q_limit_99`) and `outlier`, the type of
the sample at the 99% limits: `"regular"`, `"extreme"` (high \\T^2\\
only), `"residual"` (high Q only) or `"both"`. Draw it with
[`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md).

## Details

With scores \\t\_{ia}\\ and eigenvalues \\\lambda_a\\, \$\$T^2_i =
\sum\_{a=1}^{k} \frac{t\_{ia}^2}{\lambda_a}, \qquad Q_i = \lVert x_i -
\hat{x}\_i \rVert^2\$\$ where \\\hat{x}\_i\\ is the reconstruction of
the (centered and scaled) sample from the `k` components. A high \\T^2\\
is an extreme but well-modeled sample (for example, a high
concentration); a high Q is a sample the model does not describe
(another matrix, a contamination, an instrumental problem). The limit of
\\T^2\\ is \\k(n-1)/(n-k)\\F(k, n-k)\\ for the calibration samples and
\\k(n+1)(n-1)/(n(n-k))\\F(k, n-k)\\ for new samples. The limit of Q is
that of Jackson and Mudholkar (1979), from the eigenvalues of the
components left out, or Box's (1954) scaled chi-square approximation
(`method = "box"`). Both approximate the distribution of Q from the
eigenvalues of the calibration samples, and tend to be slightly
conservative (fewer false alarms than the nominal level).

These are classical estimates, themselves affected by outliers: see
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
and
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
for robust score and orthogonal distances.

## References

- Jackson, J.E., Mudholkar, G.S. (1979). Control procedures for
  residuals associated with principal component analysis. Technometrics,
  21(3):341-349.

- Box, G.E.P. (1954). Some theorems on quadratic forms applied in the
  study of analysis of variance problems, I. Annals of Mathematical
  Statistics, 25(2):290-302.

- Nomikos, P., MacGregor, J.F. (1995). Multivariate SPC charts for
  monitoring batch processes. Technometrics, 37(1):41-59.

## See also

[`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md),
[`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md),
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(soilLIBS)
spectra <- average(soilLIBS[-(2:8)], Sample)
pca <- stats::prcomp(spectra[-1], scale. = TRUE)
influence <- q_residuals(pca, k = 3)
influence[influence$outlier != "regular", ]
#> # A tibble: 1 × 8
#>   sample    t2     q t2_limit_95 t2_limit_99 q_limit_95 q_limit_99 outlier
#>    <int> <dbl> <dbl>       <dbl>       <dbl>      <dbl>      <dbl> <fct>  
#> 1     18  17.7 1800.        8.76        13.2      2586.      2960. extreme
plot_influence(influence, label = spectra$Sample)
```
