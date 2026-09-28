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
plot_outlier_map(object, newdata = NULL, labels = 3, title = NULL)
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

- title:

  The plot title.

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

## See also

[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md),
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
[`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(100 * 10), 100, 10) %*% diag(10:1)
x[1:4, ] <- x[1:4, ] + 25                          # bad leverage
x[5:8, 9:10] <- x[5:8, 9:10] + 15                  # orthogonal outliers
fit <- robpca(x[-(90:100), ], k = 3)
plot_outlier_map(fit)

plot_outlier_map(fit, newdata = x[90:100, ])

```
