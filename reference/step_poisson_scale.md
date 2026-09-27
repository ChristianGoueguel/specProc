# Poisson Scaling Recipe Step

`step_poisson_scale()` creates a *specification* of a recipe step that
divides each selected column by the square root of its mean plus an
offset, as
[`poisson_scale()`](https://christiangoueguel.com/specProc/reference/poisson_scale.md)
does (column mode).

## Usage

``` r
step_poisson_scale(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  offset = 3,
  scales = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("poisson_scale")
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

  Not used by this step, since no new variables are created.

- trained:

  A logical indicating whether the step has been trained.

- offset:

  The offset, in percent of the largest column mean. Default is 3.

- scales:

  The scale of each column, stored once the step has been trained.

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

Poisson scaling suits count data, whose variance grows with the mean.
The scales \\\sqrt{\bar{x}\_j + c}\\ are estimated from the training
data, where the offset \\c\\ is `offset` percent of the largest column
mean, and are applied unchanged to new data.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, the `scale` each column is divided by
(`NA` before the step is trained) and `id`.

## See also

[`poisson_scale()`](https://christiangoueguel.com/specProc/reference/poisson_scale.md),
[`step_pareto_scale()`](https://christiangoueguel.com/specProc/reference/step_pareto_scale.md)
