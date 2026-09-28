# y-Gradient Generalized Least Squares Weighting Recipe Step

`step_y_gradient_glsw()` creates a *specification* of a recipe step that
down-weights variation between samples with similar outcomes, with the
filter of
[`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md).

## Usage

``` r
step_y_gradient_glsw(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  alpha = 0.01,
  window = 5,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("y_gradient_glsw")
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

- outcome:

  The outcome variable, as a bare name or a selector. If `NULL`
  (default), the single outcome of the recipe is used.

- alpha:

  A positive number: the weighting parameter, relative to the largest
  eigenvalue of the clutter. Default is 0.01.

- window:

  An odd integer giving the width of the Savitzky-Golay window used to
  compute the gradients. Default is 5.

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

As in
[`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md),
`alpha` is relative to the largest eigenvalue of the (weighted) gradient
matrix, and the filter is stored in factored form. The filter uses the
outcome, so it is estimated on training data only; the outcome is not
needed when new data are baked. The selected columns are replaced by the
filtered values (not centered). `alpha` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html),
using
[`glsw_alpha()`](https://christiangoueguel.com/specProc/reference/glsw_alpha.md).
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, `alpha` (relative) and `id`.

## See also

[`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md),
[`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md)
