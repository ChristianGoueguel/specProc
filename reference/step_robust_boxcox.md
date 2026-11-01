# Robust Box-Cox Transformation Recipe Step

`step_robust_boxcox()` creates a *specification* of a recipe step that
transforms the selected variables toward central normality with the
robust Box-Cox transformation of
[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md).
It is the robust counterpart of
[`recipes::step_BoxCox()`](https://recipes.tidymodels.org/reference/step_BoxCox.html).

## Usage

``` r
step_robust_boxcox(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  quantile = 0.99,
  nbsteps = 2,
  standardize = TRUE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("robust_boxcox")
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

## Details

The transformation parameter \\\lambda\\ of each variable is estimated
on the training data (Raymaekers and Rousseeuw, 2021), as in
[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md),
and applied unchanged to new data. With `standardize = TRUE`, the
transformed variables are also centered and scaled with the mean and
standard deviation of the training inliers. Variables that cannot be
transformed (constant ones and, with a warning, those with zero or
negative values) are left unchanged. Missing values are ignored by the
fit and stay missing; new values that are zero or negative become `NA`.

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns a tibble with columns `terms`, `lambda` (`NA` for the variables
left unchanged) and `id`.

## See also

[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md),
[`step_robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/step_robust_yeojohnson.md)
for variables with zero or negative values.

## Author

Christian L. Goueguel

## Examples

``` r
library(recipes)
data(forageLIBS)
contents <- forageLIBS[c("Ca", "K", "P", "Mn")]
rec <- recipe(~ ., data = contents[1:300, ]) |>
  step_robust_boxcox(all_numeric_predictors())
prepped <- prep(rec)
tidy(prepped, number = 1)
#> # A tibble: 4 × 3
#>   terms  lambda id                 
#>   <chr>   <dbl> <chr>              
#> 1 Ca     0.0291 robust_boxcox_xB8WH
#> 2 K      0.578  robust_boxcox_xB8WH
#> 3 P      0.396  robust_boxcox_xB8WH
#> 4 Mn    -0.150  robust_boxcox_xB8WH
bake(prepped, new_data = contents[301:368, ])
#> # A tibble: 68 × 4
#>        Ca      K      P      Mn
#>     <dbl>  <dbl>  <dbl>   <dbl>
#>  1 -1.35   0.546 -0.400  0.465 
#>  2  0.993 -0.139 -0.779  2.27  
#>  3  0.903  1.22   1.07  -0.867 
#>  4  1.00   0.269 -0.342 -0.408 
#>  5  1.49   0.389 -0.284  2.74  
#>  6 -2.41  -0.787 -0.862  0.0961
#>  7 -0.854 -1.13  -0.419 -0.479 
#>  8  0.660 -0.223 -0.718 -0.479 
#>  9  1.39   1.64   2.53   1.25  
#> 10  0.898  0.681  2.72   2.20  
#> # ℹ 58 more rows
```
