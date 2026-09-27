# Pareto Scaling Recipe Step

`step_pareto_scale()` creates a *specification* of a recipe step that
divides each selected column by the square root of its standard
deviation, as
[`pareto_scale()`](https://christiangoueguel.com/specProc/reference/pareto_scale.md)
does.

## Usage

``` r
step_pareto_scale(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  scales = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("pareto_scale")
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

The standard deviations are estimated from the training data and applied
unchanged to new data. Columns with zero standard deviation are not
scaled. The data are not centered; add
[`recipes::step_center()`](https://recipes.tidymodels.org/reference/step_center.html)
if needed.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, the `scale` each column is divided by
(`NA` before the step is trained) and `id`.

## See also

[`pareto_scale()`](https://christiangoueguel.com/specProc/reference/pareto_scale.md),
[`step_poisson_scale()`](https://christiangoueguel.com/specProc/reference/step_poisson_scale.md)
