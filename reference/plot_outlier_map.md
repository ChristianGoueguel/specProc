# Outlier Map of a Robust PCA

Plots the orthogonal distance of each observation against its score
distance, for a robust PCA fitted by
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
or
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
(Hubert, Rousseeuw and Vanden Branden, 2005).

## Usage

``` r
plot_outlier_map(
  object,
  newdata = NULL,
  labels = 3,
  relative = FALSE,
  shade = FALSE,
  log = FALSE,
  colour_by = c("type", "distance"),
  title = NULL,
  ...
)
```

## Arguments

- object:

  An object returned by
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  or
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md).

- newdata:

  Optional new observations to add to the map (a numeric matrix or data
  frame with the calibration variables).

- labels:

  The number of most outlying observations to label (by their row names,
  or row numbers). Default is 3; use 0 for no labels.

- relative:

  If `TRUE`, plot the reduced distances (divided by their cut-offs).
  Default is `FALSE`.

- shade:

  If `TRUE`, shade the three outlying regions in grey, darker for more
  harmful observations (good leverage, orthogonal outliers, bad
  leverage), and name them in their corners. The region of regular
  observations stays white. Default is `FALSE`.

- log:

  If `TRUE`, use logarithmic axes, which spread out the regular
  observations when a few are far away. Zero distances are drawn at the
  smallest positive distance. Default is `FALSE`.

- colour_by:

  The colors of the points: `"type"` (default), by outlier type, or
  `"distance"`, by their distance from the origin in reduced distances,
  \\\sqrt{(SD/c\_{SD})^2 + (OD/c\_{OD})^2}\\, on a rainbow scale from
  dark red (close to the origin, in the regular region) to blue (the
  farthest observations).

- title:

  The plot title.

- ...:

  Further arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  to style the points, such as `alpha` (default 0.85), `size` (2.2),
  `stroke` (0.4) or `colour` (the outline, `"black"`). `size` can also
  be a numeric vector with one value per sample (the calibration
  samples, then those of `newdata`), such as the concentration of an
  element, to vary the size of the points, with a legend.

## Value

A ggplot object.

## Details

The dashed lines are the cut-offs of the two distances. They divide the
map into four types of observations:

- **regular** observations (bottom left);

- **good leverage** points (bottom right): far from the center, but
  close to the PCA subspace, so they follow the structure of the data;

- **orthogonal outliers** (top left): close to the center once
  projected, but far from the PCA subspace;

- **bad leverage** points (top right): far on both counts, the most
  harmful outliers.

New observations (`newdata`) are projected onto the model with
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_robpca.md)
and shown with the calibration cut-offs, which is how new spectra are
screened before prediction.

With `relative = TRUE`, each distance is divided by its cut-off (the
reduced score and orthogonal distances), so both cut-offs are at 1
whatever the model. This puts maps of different models (for example,
with different numbers of components) on the same scale. A relative
distance tells where an observation falls with respect to the cut-off,
not how unlikely it is: twice the cut-off is not equally rare in every
model.

## See also

[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md),
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)

## Author

Christian L. Goueguel

## Examples

``` r
# LIBS spectra of forage samples
minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
set.seed(1)
fit <- forageLIBS |>
  dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
  center() |>
  robpca()

plot_outlier_map(fit, relative = TRUE, shade = TRUE, log = TRUE)

plot_outlier_map(fit, relative = TRUE, shade = TRUE, log = TRUE,
                 alpha = 0.5, size = 3, stroke = 0.2)

```
