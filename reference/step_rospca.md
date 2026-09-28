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
