# Direct Orthogonalization Recipe Step

`step_direct_orthogonal()` creates a *specification* of a recipe step
that removes response-orthogonal variation from the selected predictors
with
[`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md).

## Usage

``` r
step_direct_orthogonal(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  num_comp = 2,
  options = list(),
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("direct_orthogonal")
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

- num_comp:

  The number of orthogonal components to remove.

- options:

  A list of further arguments passed to
  [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md),
  such as `scale`.

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

The selected columns are replaced by the corrected values, which are
centered (and scaled, if `options = list(scale = TRUE)`). Because the
filter uses the outcome, it must be estimated on training data only.
Within a
[`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html)
and
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
this happens automatically for every resample. The outcome is not needed
when new data are baked.

The related steps `step_direct_orthogonal()`,
[`step_direct_osc()`](https://christiangoueguel.com/specProc/reference/step_direct_osc.md),
[`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md),
[`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md)
and
[`step_o2pls()`](https://christiangoueguel.com/specProc/reference/step_o2pls.md)
(which also handles several outcomes) remove response-orthogonal
variation with other algorithms.
[`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md)
and
[`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md)
remove variation described by external clutter, and
[`step_y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/step_y_gradient_glsw.md)
down-weights variation between samples with similar outcomes.

## See also

[`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md),
[`predict.specproc_filter()`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
