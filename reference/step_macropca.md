# MacroPCA Recipe Step

`step_macropca()` creates a *specification* of a recipe step that
converts the selected variables into robust principal component scores
with
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
which handles cellwise outliers and missing values as well as outlying
observations.

## Usage

``` r
step_macropca(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  options = list(),
  prefix = "MPC",
  distances = FALSE,
  keep_original_cols = FALSE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("macropca")
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
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
  such as `alpha` or MacroPCA parameters.

- prefix:

  The prefix of the new column names. Default is `"MPC"`.

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
is prepped and when it is baked.

## See also

[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)
