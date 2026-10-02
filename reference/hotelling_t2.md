# Hotelling's T-squared Statistic of Samples in an Embedding

Computes Hotelling's \\T^2\\ statistic of each sample from its scores on
the first `k` components of an embedding (such as PCA or PLS scores),
with its limits at one or more confidence levels, for all the samples
together or within groups, with
[`HotellingEllipse::ellipseParam()`](https://pkgdown.r-lib.org,%20https://github.com/ChristianGoueguel/HotellingEllipse/reference/ellipseParam.html).
Samples beyond a limit are far from the center of the data (or of their
group) given its covariance: candidate outliers.

## Usage

``` r
hotelling_t2(
  data,
  columns = NULL,
  k = 2,
  group = NULL,
  conf_level = 0.975,
  method = "f"
)
```

## Arguments

- data:

  The embedding: a data frame, a matrix, a
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit, or an
  object of
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  or
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md).

- columns:

  The components used: a character vector of column names. By default,
  the first `k` embedding coordinates (columns named like `PC1`,
  `UMAP1`, ...), or else the first `k` numeric columns.

- k:

  The number of components, when `columns` is not given. Default is 2.

- group:

  An optional grouping: a column of `data` (unquoted or as a string), or
  a vector with one value per sample.

- conf_level:

  The confidence level(s) of the limits: one or more values between 0
  and 1. Default is 0.975.

- method:

  The distribution of the limits: `"f"` (default) or `"beta"`.

## Value

A tibble with one row per sample: its row number `sample`, the `group`
(with `group`), `t2`, then for each confidence level (in %, e.g. 97.5)
its limit `limit_97.5` and whether the sample exceeds it,
`outlier_97.5`, and the number of samples `n` of its group (or of the
data).

## Details

\\T^2\\ is the squared Mahalanobis distance of a sample to the mean of
the scores, with their covariance. Its limit at the confidence level
\\1 - \alpha\\, for \\n\\ samples, is

- `method = "f"` (default): \\k(n - 1)/(n - k)\\ F\_{1-\alpha}(k, n -
  k)\\, the limit for a new sample, more conservative;

- `method = "beta"`: \\(n - 1)^2/n\\ B\_{1-\alpha}(k/2, (n - k -
  1)/2)\\, the exact distribution for the samples that estimated the
  mean and covariance (Tracy, Young and Mason, 1992); the F limit can
  even exceed the largest \\T^2\\ a sample can reach, \\(n - 1)^2 / n\\,
  for small \\n\\.

Within groups (`group`), each group gets its own mean, covariance and
limits, which tells whether a sample is typical of its own group rather
than of the whole data set; each group needs more than `k + 1` samples.

The statistic uses the `k` components given, not only the two shown on a
plot: a sample can lie inside the ellipse of two components and still
exceed the limit in `k` dimensions. Classical estimates are themselves
sensitive to outliers; with many outliers, prefer the robust distances
of
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md).

## References

- Hotelling, H. (1931). The generalization of Student's ratio. The
  Annals of Mathematical Statistics, 2(3):360-378.

- Tracy, N.D., Young, J.C., Mason, R.L. (1992). Multivariate control
  charts for individual observations. Journal of Quality Technology,
  24(2):88-95.

- Jackson, J.E. (1991). A User's Guide to Principal Components. Wiley,
  New York.

## See also

[`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md),
[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md),
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)

## Author

Christian L. Goueguel

## Examples

``` r
if (rlang::is_installed("HotellingEllipse", version = "1.3.0")) {
  data(forageLIBS)
  # the 380-430 nm window (Ca II H and K lines)
  wl <- suppressWarnings(as.numeric(names(forageLIBS)))
  spectra <- forageLIBS[which(wl > 380 & wl < 430)]
  pca <- stats::prcomp(spectra)
  t2 <- hotelling_t2(pca, k = 3)
  t2[t2$outlier_97.5, ]
  # two limits, with the exact distribution of the calibration samples
  hotelling_t2(pca, k = 3, conf_level = c(0.95, 0.99), method = "beta")
}
#> # A tibble: 368 × 7
#>    sample    t2 limit_95 limit_99 outlier_95 outlier_99     n
#>     <int> <dbl>    <dbl>    <dbl> <lgl>      <lgl>      <int>
#>  1      1 1.31      7.76     11.2 FALSE      FALSE        368
#>  2      2 1.15      7.76     11.2 FALSE      FALSE        368
#>  3      3 0.770     7.76     11.2 FALSE      FALSE        368
#>  4      4 4.46      7.76     11.2 FALSE      FALSE        368
#>  5      5 0.903     7.76     11.2 FALSE      FALSE        368
#>  6      6 0.594     7.76     11.2 FALSE      FALSE        368
#>  7      7 5.60      7.76     11.2 FALSE      FALSE        368
#>  8      8 1.16      7.76     11.2 FALSE      FALSE        368
#>  9      9 1.32      7.76     11.2 FALSE      FALSE        368
#> 10     10 1.59      7.76     11.2 FALSE      FALSE        368
#> # ℹ 358 more rows
```
