# Plot a Two-Dimensional Embedding

Draws the samples in two dimensions of an embedding, such as UMAP
coordinates from
[`embed::step_umap()`](https://embed.tidymodels.org/reference/step_umap.html),
principal component scores from
[`recipes::step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html),
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
or [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html), colored by
a variable.

## Usage

``` r
plot_embedding(
  data,
  x = NULL,
  y = NULL,
  colour = NULL,
  size = 2,
  alpha = 0.8,
  ellipse = FALSE,
  conf_level = 0.975,
  robust = FALSE,
  distribution = "normal",
  hotelling = "none",
  k = 2,
  t2_method = "f",
  flag = FALSE,
  label = NULL,
  biplot = FALSE,
  biplot_top = 10,
  aspect_ratio = 0.7,
  title = NULL
)
```

## Arguments

- data:

  The embedding: a data frame (such as a baked recipe), a matrix, a
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit, or an
  object of
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  or
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md).

- x, y:

  The columns of the axes, unquoted or as strings. By default, the first
  two embedding coordinates (see details).

- colour:

  The variable coloring the points: a column of `data` (unquoted or as a
  string), or a vector with one value per sample.

- size:

  The size of the points: a number (default 2), or a numeric column of
  `data` or vector with one value per sample to vary it.

- alpha:

  The opacity of the points.

- ellipse:

  A logical: draw confidence ellipses (`FALSE`, default). Needs the
  ConfidenceEllipse package.

- conf_level:

  The confidence level(s) of the ellipses, confidence and \\T^2\\: one
  or more values between 0 and 1. Default is 0.975. The samples beyond a
  \\T^2\\ limit are flagged at the highest level.

- robust:

  A logical: robust ellipses (`FALSE`, default).

- distribution:

  The quantile of the ellipses: `"normal"` (default, chi-square) or
  `"hotelling"`.

- hotelling:

  Hotelling's \\T^2\\ ellipses and outliers: `"none"` (default), `"all"`
  (all samples) or `"group"` (within the groups of a discrete `colour`).
  Needs the HotellingEllipse package (1.3.0 or later).

- k:

  The number of components of \\T^2\\: the two axes, then the next
  embedding coordinates. Default is 2.

- t2_method:

  The distribution of the \\T^2\\ limits: `"f"` (default) or `"beta"`
  (see
  [`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md)).

- flag:

  A logical: circle in red the samples beyond the \\T^2\\ limit
  (`FALSE`, default).

- label:

  The labels of the samples beyond the \\T^2\\ limit, whether or not
  they are circled: `NULL` or `FALSE` (default) for none, `TRUE` for
  their row numbers, or a column of `data` or a vector with one value
  per sample.

- biplot:

  A logical: draw the loadings over the scores (`FALSE`, default).

- biplot_top:

  The number of loadings drawn as labeled arrows. Default is 10.

- aspect_ratio:

  The ratio of the height to the width of the panel. Default is 0.7;
  `NULL` lets the panel fill the plot.

- title:

  The plot title.

## Value

A ggplot object.

## Details

By default, the axes are the first two columns named like embedding
coordinates (`UMAP1`, `PC1`, `Comp1`, ...; the name followed by a
number), or else the first two numeric columns. A numeric `colour` uses
a continuous viridis scale, other types a discrete palette.

With `ellipse = TRUE`, a confidence ellipse is drawn for each group of a
discrete `colour` (or for all the samples otherwise), with
[`ConfidenceEllipse::confidence_ellipse()`](https://christiangoueguel.github.io/ConfidenceEllipse/reference/confidence_ellipse.html),
at each level of `conf_level` (0.975 by default). It covers the region
expected to hold that share of the samples of the group if they follow a
bivariate normal distribution, from their mean and covariance, or from
robust estimates (MCD) with `robust = TRUE`, which resist outlying
samples. `distribution = "hotelling"` uses the quantile of Hotelling's
\\T^2\\ distribution, which accounts for the uncertainty of the
estimates and suits small groups. Robust estimates need larger groups
(the MCD fits a subset of about three quarters of the samples): with
fewer than about 10 samples per group, their ellipses can be flat or
leave out several samples. Groups with fewer than 4 samples get no
ellipse.

With `hotelling = "all"`, the ellipses of Hotelling's \\T^2\\ at each
level of `conf_level` (97.5% by default) are drawn for all the samples,
and the samples beyond the limit at the highest level of \\T^2\\ on `k`
components are counted in the subtitle, circled in red with
`flag = TRUE` and labeled with `label` (each independently of the
other): the classical outlier limits of a score plot. For a PCA model
([`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html),
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
or
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)),
\\T^2\\ is that of the model: the scores divided by the variances of the
components (the robust eigenvalues of a robust fit), so the ellipses are
centered at 0 with the axes of the components, and semi-axes
\\\sqrt{\lambda_a L}\\ for the limit \\L\\. The limit is the F (or Beta)
limit of
[`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md)
for a [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit, and
the chi-square quantile of ROBPCA for a robust fit, whose flagged
samples are then the leverage points of
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
(with `k` its number of components and `conf_level = 0.975`). The scores
of the outlying samples then do not inflate or tilt the ellipses. For
other embeddings, \\T^2\\ is computed from the mean and covariance of
the samples (with
[`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md)
and
[`HotellingEllipse::ellipseCoord()`](https://pkgdown.r-lib.org,%20https://github.com/ChristianGoueguel/HotellingEllipse/reference/ellipseCoord.html)).
With `hotelling = "group"`, each group of a discrete `colour` gets its
own ellipses and limits, from its mean and covariance, which flags the
samples atypical of their own group. The ellipses are labeled with their
level (instead of a legend), and drawn for the two components shown,
while the limits use `k` components, so with `k > 2` a flagged sample
can lie inside the ellipses. With `ellipse = TRUE`, the confidence
ellipses are always computed from the samples shown. Without groups,
`ellipse = TRUE` and `hotelling = "all"` would draw two ellipses of all
the samples: only the \\T^2\\ ellipse is drawn, with a warning. With
groups, both are drawn: the confidence ellipses of the groups and the
\\T^2\\ limits of all the samples. \\T^2\\ suits linear scores such as
PCA or PLS; on a UMAP map, whose distances are not meaningful, prefer
`ellipse`.

**Style.** The panel is grey outside the outermost ellipse (of \\T^2\\,
or of each group) and white inside, so that the samples beyond the
limits stand out, with no grid and a fixed `aspect_ratio` (0.7 by
default; `NULL` lets the plot fill the space), and thin black lines
through the origin (when it lies in the range of the samples, as for
centered scores). Without ellipses, the panel is white.

**Axes.** For a [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html)
fit or a robust PCA, the axis titles give the share of the variance of
each component: of the total variance for
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html), and of the
variance of the `k` components of the model for
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
and
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
which estimate only these. With groups, the ellipses (confidence, or
\\T^2\\ with `hotelling = "group"`) are filled with the color of their
group.

**Point size.** `size` is a number, or a numeric variable (a column of
`data` or a vector with one value per sample), such as the concentration
of an element, which sets the size of each point, with a legend.

**Biplot.** With `biplot = TRUE` (for a
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit or a robust
PCA), the loadings of the two components are drawn over the scores: all
the variables as faint points, and the `biplot_top` longest as labeled
arrows. For spectra, the arrows are the local peaks of the loading
length along the wavelength, one per emission line, labeled with their
wavelength. Each axis of loadings is scaled to the range of its scores,
and read on the top and right axes. A sample lies toward the arrows of
the variables in which it is high.

In a UMAP embedding, only the neighborhoods are meaningful: the sizes of
the clusters and the distances between them are not, and they change
with `neighbors` and `min_dist`. Read the plot as a map of which samples
are similar, not as a quantitative projection.

## See also

[`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md),
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md),
[`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md),
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)

## Author

Christian L. Goueguel

## Examples

``` r
# robust PCA of the forage spectra, with the Hotelling's T-squared limit
if (rlang::is_installed("HotellingEllipse", version = "1.3.0")) {
  data(forageLIBS)
  spectra_id <- names(forageLIBS)[1:2]
  minerals <- names(forageLIBS)[3:14]
  set.seed(1)
  forageLIBS |>
    dplyr::select(-dplyr::all_of(c(spectra_id, minerals))) |>
    center() |>
    robpca() |>
    plot_embedding(hotelling = "all", flag = FALSE, label = TRUE)
}

```
