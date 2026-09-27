# Generalized Least Squares Weighting Recipe Step

`step_glsw()` creates a *specification* of a recipe step that
down-weights the directions of the selected predictors that vary in a
clutter matrix, with the generalized least squares weighting filter of
[`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md).

## Usage

``` r
step_glsw(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  clutter,
  alpha = 0.01,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("glsw")
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

  A numeric matrix or data frame of difference spectra. It is matched to
  the selected predictors as in
  [`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md).

- alpha:

  A positive number: the weighting parameter, relative to the largest
  eigenvalue of the clutter. Default is 0.01.

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

`clutter` holds differences between spectra that should be identical,
such as `x2 - x1` for the paired spectra of
[`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md). It
is centered, so the filter equals `glsw(x1, x2, alpha = a)`, where the
absolute `a` is `alpha` times the largest eigenvalue of the centered
clutter cross-product. Setting `alpha` relative to that eigenvalue makes
it independent of the scale of the spectra, which simplifies tuning:
small values remove the clutter directions almost completely (like
[`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md)),
large values leave the data nearly unchanged.

The filter is stored in factored form, so its memory use grows with the
number of predictors rather than its square. The selected columns are
replaced by the filtered values (not centered). `alpha` can be tuned
with
[`tune::tune()`](https://tune.tidymodels.org/reference/reexports.html),
using
[`glsw_alpha()`](https://christiangoueguel.com/specProc/reference/glsw_alpha.md).
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, `alpha` (relative) and `id`.

## See also

[`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md),
[`step_y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/step_y_gradient_glsw.md)
