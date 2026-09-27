# Multiplicative Scatter Correction Recipe Step

`step_msc()` creates a *specification* of a recipe step that corrects
each spectrum for multiplicative and additive effects relative to a
reference spectrum, with
[`msc()`](https://christiangoueguel.com/specProc/reference/msc.md).

## Usage

``` r
step_msc(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  robust = TRUE,
  drop.offset = TRUE,
  window = NULL,
  reference = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("msc")
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

- robust:

  A logical: use the median (`TRUE`, default) or the mean of the
  training spectra as the reference.

- drop.offset:

  A logical: remove the additive offset (`TRUE`, default) or only the
  multiplicative effect.

- window:

  An optional list of column index vectors (within the selected columns)
  for piecewise MSC. See
  [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md).

- reference:

  The reference spectrum, stored once the step has been trained.

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

The reference spectrum (the median or mean of the training spectra) is
estimated when the recipe is prepped, and new spectra are corrected
against it when they are baked. The selected columns form one spectrum
per row. They are replaced by the corrected values.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, the `reference` value of each (`NA` before
the step is trained) and `id`.

## See also

[`msc()`](https://christiangoueguel.com/specProc/reference/msc.md),
[`step_emsc()`](https://christiangoueguel.com/specProc/reference/step_emsc.md)
