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
Rousseeuw, 2021), as in
[`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md),
and applied unchanged to new data. With `standardize = TRUE`, the
transformed variables are also centered and scaled with the mean and
standard deviation of the training inliers. Variables that cannot be
transformed (for example, constant ones) are left unchanged.

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
data(forageLIBS)
contents <- forageLIBS[c("Ca", "Mg", "P", "K", "Mn")]
rec <- recipe(~ ., data = contents[1:300, ]) |>
  step_robust_bcyj(all_numeric_predictors())
prepped <- prep(rec)
tidy(prepped, number = 1)
#> # A tibble: 5 × 4
#>   terms  lambda method id               
#>   <chr>   <dbl> <chr>  <chr>            
#> 1 Ca     0.0291 BC     robust_bcyj_xB8WH
#> 2 Mg     0.805  YJ     robust_bcyj_xB8WH
#> 3 P      0.808  YJ     robust_bcyj_xB8WH
#> 4 K      0.871  YJ     robust_bcyj_xB8WH
#> 5 Mn    -0.150  BC     robust_bcyj_xB8WH
bake(prepped, new_data = contents[301:368, ])
#> # A tibble: 68 × 5
#>        Ca      Mg      P      K      Mn
#>     <dbl>   <dbl>  <dbl>  <dbl>   <dbl>
#>  1 -1.35  -1.60   -0.404  0.546  0.465 
#>  2  0.993  2.33   -0.790 -0.137  2.27  
#>  3  0.903  0.0603  1.07   1.22  -0.867 
#>  4  1.00   0.0188 -0.345  0.271 -0.408 
#>  5  1.49   0.798  -0.286  0.389  2.74  
#>  6 -2.41  -1.85   -0.873 -0.782  0.0961
#>  7 -0.854 -0.919  -0.424 -1.12  -0.479 
#>  8  0.660 -0.0441 -0.728 -0.221 -0.479 
#>  9  1.39   1.39    2.56   1.64   1.25  
#> 10  0.898  2.39    2.76   0.681  2.20  
#> # ℹ 58 more rows
```
