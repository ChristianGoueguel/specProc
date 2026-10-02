# Robust Sparse PCA (ROSPCA) Recipe Step

`step_rospca()` creates a *specification* of a recipe step that converts
the selected variables into robust sparse principal component scores,
with
[`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md).

## Usage

``` r
step_rospca(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  lambda = 1,
  options = list(),
  prefix = "RSPC",
  distances = FALSE,
  keep_original_cols = FALSE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("rospca")
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

- lambda:

  The sparsity parameter of
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md).
  Default is 1.

- options:

  A list of further arguments passed to
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md),
  such as `alpha`, `stand` or `ndir`.

- prefix:

  The prefix of the new column names. Default is `"RSPC"`.

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
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md),
with sparse loadings. `num_comp` and `lambda` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html);
`lambda` uses
[`dials::penalty()`](https://dials.tidymodels.org/reference/penalty.html)
with a range of 0.01 to 100.

## See also

[`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md),
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
set.seed(1)
rec <- recipes::recipe(K ~ ., data = dat) |>
  step_rospca(recipes::all_predictors(), num_comp = 2, distances = TRUE)
recipes::bake(recipes::prep(rec), new_data = NULL)
#> # A tibble: 368 × 5
#>        K RSPC1    RSPC2 RSPC_SD RSPC_OD
#>    <dbl> <dbl>    <dbl>   <dbl>   <dbl>
#>  1  3.68 21.1   -7.40     2.76     8.53
#>  2  2.52 12.8   -0.0993   1.14     5.92
#>  3  2.45 13.5  -17.6      4.97     7.44
#>  4  2.3   7.94 -12.5      3.49     5.68
#>  5  2.87 -4.68   2.84     0.880    5.75
#>  6  2.16  7.41   2.29     0.909    5.92
#>  7  2.94 38.0  -23.8      7.32    15.7 
#>  8  2.43  5.27   2.19     0.759    5.91
#>  9  1.81 -3.99   3.60     1.05     6.13
#> 10  2.07 20.2   -1.60     1.85     6.09
#> # ℹ 358 more rows
```
