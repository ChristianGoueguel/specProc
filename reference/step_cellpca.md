# cellPCA Recipe Step

`step_cellpca()` creates a *specification* of a recipe step that
converts the selected variables into robust principal component scores
with
[`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md),
which weights the outlying cells and observations and handles missing
values.

## Usage

``` r
step_cellpca(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  options = list(),
  prefix = "CPC",
  distances = FALSE,
  keep_original_cols = FALSE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("cellpca")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose the predictors (for example,
  the spectral channels). See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html).

- role:

  The role of the new score columns. Default is `"predictor"`.

- trained:

  A logical indicating whether the step has been trained.

- num_comp:

  The number of principal components. Default is 2.

- options:

  A list of further arguments passed to
  [`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md),
  such as `alpha`, `maxiter` or `tol`.

- prefix:

  The prefix of the new column names. Default is `"CPC"`.

- distances:

  A logical: add the score and orthogonal distances as columns. Default
  is `FALSE`.

- keep_original_cols:

  A logical: keep the selected columns. Default is `FALSE`.

- res:

  The fitted robust PCA model, stored once the step has been trained.

- columns:

  The names of the selected columns, stored once the step has been
  trained.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?
  Keep the default `FALSE`, so that new data are corrected with the same
  filter as the training data.

- id:

  A character string that is unique to this step.

## Value

An updated version of `recipe` with the new step added to the sequence
of existing steps.

## Details

As
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md).
Missing values are allowed in the selected columns, both when the recipe
is prepped and when it is baked. New observations are projected by the
robust regression of their observed cells on the loadings, so that their
outlying cells do not distort their scores.

## See also

[`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md),
[`step_macropca()`](https://christiangoueguel.com/specProc/reference/step_macropca.md),
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
set.seed(1)
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_cellpca(recipes::all_predictors(), num_comp = 2, distances = TRUE)
recipes::bake(recipes::prep(rec), new_data = dat[301:368, ])
#> # A tibble: 68 × 5
#>        K    CPC1   CPC2 CPC_SD CPC_OD
#>    <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
#>  1  2.25 -17363.  375.   0.920  17.7 
#>  2  1.91 -27529. -472.   1.82   32.3 
#>  3  2.61   1731. 1248.   0.831  13.8 
#>  4  2.11 -19592. -134.   1.03   14.6 
#>  5  2.17 -22224.  -10.4  1.16   15.0 
#>  6  1.61   2360. 2403.   2.02   15.6 
#>  7  1.46 -23785.  257.   1.24    9.53
#>  8  1.87 -18414.  266.   0.957  10.3 
#>  9  2.84 -22866.  565.   1.30   27.6 
#> 10  2.32 -12227.  504.   0.638   8.68
#> # ℹ 58 more rows
```
