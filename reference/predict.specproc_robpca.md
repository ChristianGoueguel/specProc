# Scores and Distances of New Observations

Projects new observations onto a robust PCA model fitted by
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
or
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
and computes their score and orthogonal distances with the cut-offs of
the calibration data.

## Usage

``` r
# S3 method for class 'specproc_macropca'
predict(object, newdata, ...)

# S3 method for class 'specproc_robpca'
predict(object, newdata, ...)
```

## Arguments

- object:

  An object returned by
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  or
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md).

- newdata:

  A numeric matrix or data frame with the same variables as the
  calibration data. For
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
  missing values are allowed.

- ...:

  Not used.

## Value

A tibble with the scores (`PC1`, ..., `PCk`), the score distance `sd`,
the orthogonal distance `od` and the `outlier_type` of each new
observation.

## See also

[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md),
which can display new observations.
