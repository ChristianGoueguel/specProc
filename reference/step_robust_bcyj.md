# Robust Box-Cox and Yeo-Johnson Transformation Recipe Step

**\[deprecated\]**

`step_robust_bcyj()` was deprecated in specProc 0.9.0: use
[`step_robust_boxcox()`](https://christiangoueguel.com/specProc/reference/step_robust_boxcox.md)
or
[`step_robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/step_robust_yeojohnson.md)
instead. It still works, with a warning, and the recipes trained with it
can still be baked.

## Usage

``` r
step_robust_bcyj(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  type = "bestObj",
  quantile = 0.99,
  nbsteps = 2,
  standardize = TRUE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("robust_bcyj")
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

- type:

  The transformation: `"bestObj"` (default; Box-Cox or Yeo-Johnson,
  whichever fits best, for positive variables), `"BC"` or `"YJ"`.

- quantile:

  The quantile of the absolute value of a standard normal variable
  beyond which the standardized transformed values are given weight zero
  in the re-weighting steps, strictly between 0 and 1. Default is 0.99.

- nbsteps:

  The number of re-weighting steps, a positive integer. Default is 2.

- standardize:

  A logical value: standardize the transformed variables by the mean and
  standard deviation of the inliers (`TRUE`, default).

- res:

  The fitted transformation, stored once the step has been trained.

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

## Author

Christian L. Goueguel
