# Hotelling's T-squared Statistic of Samples in an Embedding

Computes Hotelling's \\T^2\\ statistic of each sample from its scores on
the first `k` components of an embedding (such as PCA or PLS scores),
with the 95% and 99% limits, for all the samples together or within
groups. Samples beyond the limits are far from the center of the data
(or of their group) given its covariance: candidate outliers.

## Usage

``` r
hotelling_t2(data, columns = NULL, k = 2, group = NULL)
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

## Value

A tibble with one row per sample: its row number `sample`, the `group`
(with `group`), `t2`, the limits `limit_95` and `limit_99`, `outlier_95`
and `outlier_99`, and the number of samples `n` of its group (or of the
data).

## Details

\\T^2\\ is the squared Mahalanobis distance of a sample to the mean of
the scores, with their covariance. Its limit at the confidence level
\\1 - \alpha\\ is \\k(n - 1)/(n - k)\\ F\_{1-\alpha}(k, n - k)\\ for
\\n\\ samples. Within groups (`group`), each group gets its own mean,
covariance and limits, which tells whether a sample is typical of its
own group rather than of the whole data set; each group needs more than
`k + 1` samples.

The statistic uses the `k` components given, not only the two shown on a
plot: a sample can lie inside the ellipse of two components and still
exceed the limit in `k` dimensions. Classical estimates are themselves
sensitive to outliers; with many outliers, prefer the robust distances
of
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md).

## References

- Hotelling, H. (1931). The generalization of Student's ratio. The
  Annals of Mathematical Statistics, 2(3):360-378.

- Jackson, J.E. (1991). A User's Guide to Principal Components. Wiley,
  New York.

## See also

[`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md),
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(soilLIBS)
spectra <- average(soilLIBS[-(2:8)], Sample)
pca <- stats::prcomp(spectra[-1], scale. = TRUE)
t2 <- hotelling_t2(pca, k = 3)
t2[t2$outlier_95, ]
#> # A tibble: 1 × 7
#>   sample    t2 limit_95 limit_99 outlier_95 outlier_99     n
#>    <int> <dbl>    <dbl>    <dbl> <lgl>      <lgl>      <int>
#> 1     18  17.7     8.76     13.2 TRUE       TRUE          50
```
