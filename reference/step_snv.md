# Standard Normal Variate Recipe Step

`step_snv()` creates a *specification* of a recipe step that centers and
scales each spectrum by its own mean and standard deviation, with
[`snv()`](https://christiangoueguel.com/specProc/reference/snv.md).

## Usage

``` r
step_snv(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("snv")
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

The selected columns form one spectrum per row; select all the channels
of the spectrum and only them. Nothing is estimated from the training
data. The selected columns are replaced by the SNV-transformed values.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms` and `id`.

## See also

[`snv()`](https://christiangoueguel.com/specProc/reference/snv.md)
