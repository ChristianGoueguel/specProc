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
tidy(prepped, number = 1)
#> # A tibble: 3 × 4
#>   terms  lambda method id               
#>   <chr>   <dbl> <chr>  <chr>            
#> 1 a     -0.0415 YJ     robust_bcyj_NmydP
#> 2 b      0.373  BC     robust_bcyj_NmydP
#> 3 c      0.848  YJ     robust_bcyj_NmydP
bake(prepped, new_data = dat[81:100, ])
#> # A tibble: 20 × 3
#>         a       b       c
#>     <dbl>   <dbl>   <dbl>
#>  1 -0.882  1.44    2.25  
#>  2 -0.337  0.464  -2.27  
#>  3  1.25   0.0543 -0.747 
#>  4 -1.67  -1.39   -2.06  
#>  5  0.593  0.549  -0.560 
#>  6  0.278 -0.701   0.212 
#>  7  1.12   0.0119  1.54  
#>  8 -0.560  0.0681 -0.584 
#>  9  0.323 -0.498  -1.44  
#> 10  0.195 -0.0336 -0.436 
#> 11 -0.852 -1.74    0.520 
#> 12  1.28  -1.62    0.0814
#> 13  1.23   0.330   1.18  
#> 14  0.716 -0.124  -0.426 
#> 15  1.68   0.0527 -1.98  
#> 16  0.551 -1.19    0.839 
#> 17 -1.52  -0.126  -1.34  
#> 18 -0.887  1.37    0.516 
#> 19 -1.49  -0.587   0.415 
#> 20 -0.771 -0.604   0.271 
```
