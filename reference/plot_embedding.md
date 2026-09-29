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
  conf_level = 0.95,
  robust = FALSE,
  distribution = "normal",
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

- size, alpha:

  The size and opacity of the points.

- ellipse:

  A logical: draw confidence ellipses (`FALSE`, default). Needs the
  ConfidenceEllipse package.

- conf_level:

  The confidence level of the ellipses. Default is 0.95.

- robust:

  A logical: robust ellipses (`FALSE`, default).

- distribution:

  The quantile of the ellipses: `"normal"` (default, chi-square) or
  `"hotelling"`.

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
[`ConfidenceEllipse::confidence_ellipse()`](https://christiangoueguel.github.io/ConfidenceEllipse/reference/confidence_ellipse.html).
It covers the region expected to hold `conf_level` of the samples of the
group if they follow a bivariate normal distribution, from their mean
and covariance, or from robust estimates (MCD) with `robust = TRUE`,
which resist outlying samples. `distribution = "hotelling"` uses the
quantile of Hotelling's \\T^2\\ distribution, which accounts for the
uncertainty of the estimates and suits small groups. Robust estimates
need larger groups (the MCD fits a subset of about three quarters of the
samples): with fewer than about 10 samples per group, their ellipses can
be flat or leave out several samples. Groups with fewer than 4 samples
get no ellipse.

In a UMAP embedding, only the neighborhoods are meaningful: the sizes of
the clusters and the distances between them are not, and they change
with `neighbors` and `min_dist`. Read the plot as a map of which samples
are similar, not as a quantitative projection.

## See also

[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md),
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(soilLIBS)
spectra <- average(soilLIBS[-(2:8)], Sample)

# soil texture in three classes (USDA general terms: coarse = sands and
# sandy loams, medium = loams and silty loams, fine = clays and clay loams)
classes <- c(Sand = "coarse", `Loamy Sand` = "coarse", `Sandy Loam` = "coarse",
             Loam = "medium", `Silt Loam` = "medium", Silt = "medium",
             `Clay Loam` = "fine", `Silty Clay Loam` = "fine", `Sandy Clay Loam` = "fine",
             Clay = "fine", `Silty Clay` = "fine", `Sandy Clay` = "fine")
texture <- soilLIBS$Texture[match(spectra$Sample, soilLIBS$Sample)]
texture <- factor(classes[as.character(texture)], levels = c("fine", "medium", "coarse"))

pca <- stats::prcomp(spectra[-1], scale. = TRUE)
plot_embedding(pca, colour = texture, title = "PCA of the sample spectra")


if (rlang::is_installed("ConfidenceEllipse")) {
  plot_embedding(pca, colour = texture, ellipse = TRUE, distribution = "hotelling")
}
#> Warning: RGL: unable to open X11 display
#> Warning: 'rgl.init' failed, will use the null device.
#> See '?rgl.useNULL' for ways to avoid this warning.


if (rlang::is_installed(c("recipes", "embed"))) {
  set.seed(1)
  umap <- recipes::recipe(~ ., data = spectra) |>
    recipes::update_role(Sample, new_role = "id") |>
    recipes::step_normalize(recipes::all_predictors()) |>
    recipes::step_pca(recipes::all_predictors(), num_comp = 10) |>
    embed::step_umap(recipes::all_predictors(), neighbors = 10) |>
    recipes::prep() |>
    recipes::bake(new_data = NULL)
  plot_embedding(umap, colour = texture)
}
```
