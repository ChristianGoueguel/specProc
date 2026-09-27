# External Parameter Orthogonalization Recipe Step

`step_epo()` creates a *specification* of a recipe step that projects
the selected predictors onto the space orthogonal to the dominant
directions of a clutter matrix, with
[`epo()`](https://christiangoueguel.com/specProc/reference/epo.md).

## Usage

``` r
step_epo(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  clutter = NULL,
  num_comp = 2,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("epo")
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

- clutter:

  A numeric matrix or data frame of clutter spectra. If it has column
  names, the columns matching the selected predictors are used;
  otherwise it must have one column per selected predictor, in the same
  order.

- num_comp:

  The number of orthogonal components to remove.

- res:

  The fitted filter, stored once the step has been trained.

- columns:

  The names of the selected predictors, stored once the step has been
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

The clutter describes variation that should not affect the outcome, for
example the differences between repeated measurements of the same
samples. It is external information, supplied when the step is
specified, and is not estimated from the training data. If
`clutter = NULL`, the dominant directions of the training data
themselves are removed, as in
[`epo()`](https://christiangoueguel.com/specProc/reference/epo.md). The
outcome is not used.

The selected columns are replaced by the corrected values (not
centered). `num_comp` can be tuned with
[`tune::tune()`](https://tune.tidymodels.org/reference/reexports.html).
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, `num_comp` and `id`.

## See also

[`epo()`](https://christiangoueguel.com/specProc/reference/epo.md),
[`predict.specproc_filter()`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
