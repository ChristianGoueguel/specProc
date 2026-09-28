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

- title:

  The plot title.

## Value

A ggplot object.

## Details

By default, the axes are the first two columns named like embedding
coordinates (`UMAP1`, `PC1`, `Comp1`, ...; the name followed by a
number), or else the first two numeric columns. A numeric `colour` uses
a continuous viridis scale, other types a discrete palette.

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
texture <- soilLIBS$Texture[match(spectra$Sample, soilLIBS$Sample)]
pca <- stats::prcomp(spectra[-1], scale. = TRUE)
plot_embedding(pca, colour = texture, title = "PCA of the sample spectra")


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
