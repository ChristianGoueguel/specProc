# Robust Box-Cox and Yeo-Johnson Transformation Recipe Step

`step_robust_bcyj()` creates a *specification* of a recipe step that
transforms the selected variables toward central normality with the
robust Box-Cox or Yeo-Johnson transformation of
[`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md),
like
[`recipes::step_BoxCox()`](https://recipes.tidymodels.org/reference/step_BoxCox.html)
or
[`recipes::step_YeoJohnson()`](https://recipes.tidymodels.org/reference/step_YeoJohnson.html)
but robust to outliers.

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
  whichever fits best, for positive variables), `"BC"` or `"YJ"`. See
  [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md).

- quantile:

  The quantile used for the weights of the re-weighting steps. Default
  is 0.99.

- nbsteps:

  The number of re-weighting steps. Default is 2.

- standardize:

  A logical: robustly standardize the transformed variables (`TRUE`,
  default).

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

## Details

The transformation parameter \\\lambda\\ of each variable is estimated
on the training data by re-weighted maximum likelihood (Raymaekers and
Rousseeuw, 2021) with
[`cellWise::transfo()`](https://rdrr.io/pkg/cellWise/man/transfo.html),
and applied unchanged to new data with
[`cellWise::transfo_newdata()`](https://rdrr.io/pkg/cellWise/man/transfo_newdata.html).
With `standardize = TRUE`, the transformed variables are also robustly
centered and scaled with the training estimates. Variables that cannot
be transformed (for example, constant ones) are left unchanged.

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns a tibble with columns `terms`, `lambda`, `method` (`"BC"` or
`"YJ"`) and `id`.

## See also

[`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md)

## Author

Christian L. Goueguel

## Examples

``` r
library(recipes)
set.seed(1)
dat <- data.frame(a = rlnorm(100), b = rexp(100), c = rnorm(100))
rec <- recipe(~ ., data = dat[1:80, ]) |>
  step_robust_bcyj(all_numeric_predictors())
prepped <- prep(rec)
#> Error in step_robust_bcyj(): Error in `step_robust_bcyj()`:
#> Caused by error in `cellWise::transfo_newdata()`:
#> ! Xnew has variable names, but the original input to transfo() did not. Please match the variable names to the original input data.
tidy(prepped, number = 1)
#> Error: object 'prepped' not found
bake(prepped, new_data = dat[81:100, ])
#> Error: object 'prepped' not found
```
