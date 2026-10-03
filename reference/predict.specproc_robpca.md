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

# S3 method for class 'specproc_cellpca'
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

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 760 & wl < 780)]  # the K I resonance lines
set.seed(1)
fit <- robpca(spectra[1:300, ])
head(predict(fit, spectra[301:368, ]))
#> # A tibble: 6 × 4
#>       PC1    sd     od outlier_type      
#>     <dbl> <dbl>  <dbl> <fct>             
#> 1 -23113. 1.06   6527. regular           
#> 2 -33447. 1.53  10935. orthogonal outlier
#> 3  -4303. 0.197  4341. regular           
#> 4 -25272. 1.16   5235. regular           
#> 5 -27946. 1.28   5746. regular           
#> 6  -3636. 0.167  6703. regular           
```
