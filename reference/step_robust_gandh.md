# Robust Tukey g-and-h Transformation Recipe Step

Creates a recipe step applying
[`robust_gandh()`](https://christiangoueguel.com/specProc/reference/robust_gandh.md)
to the selected variables. The rQLS fit and all standardization
constants are estimated during
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) on the
training data and applied unchanged during
[`bake()`](https://recipes.tidymodels.org/reference/bake.html).

## Usage

``` r
step_robust_gandh(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  n_quantiles = 10,
  cutoff = NULL,
  max_iter = 50,
  tol = 1e-06,
  standardize = TRUE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("robust_gandh")
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

- n_quantiles:

  Number of fitting quantiles, an integer from 4 to 20. Default is 10,
  as recommended for rQLS in the paper.

- cutoff:

  Positive, finite Tukey biweight cutoff on the median/MAD standardized
  input scale. `NULL` (default) selects it adaptively for each variable.
  It is an order-statistic fitting tolerance, not a normal score cutoff
  or a contamination percentage.

- max_iter:

  Maximum number of trimming/refitting iterations per cutoff. Default is
  50.

- tol:

  Positive, finite relative tolerance for convergence. Default is 1e-6.

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

Selected columns are replaced in place. Missing values stay missing and
variables without enough observations or spread are left unchanged, as
in
[`robust_gandh()`](https://christiangoueguel.com/specProc/reference/robust_gandh.md).
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns
`terms`, `location`, `scale`, `g`, `h` and `id`; location and scale are
on the original input scale.

## See also

[`robust_gandh()`](https://christiangoueguel.com/specProc/reference/robust_gandh.md),
[`step_robust_boxcox()`](https://christiangoueguel.com/specProc/reference/step_robust_boxcox.md),
[`step_robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/step_robust_yeojohnson.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
contents <- forageLIBS[c("Ca", "K", "P", "Mn")]
rec <- recipes::recipe(~ ., data = contents[1:300, ]) |>
  step_robust_gandh(recipes::all_numeric_predictors())
trained <- recipes::prep(rec)
recipes::bake(trained, new_data = contents[301:368, ])
#> # A tibble: 68 × 4
#>        Ca      K      P      Mn
#>     <dbl>  <dbl>  <dbl>   <dbl>
#>  1 -1.25   0.548 -0.399  0.415 
#>  2  0.937 -0.146 -0.771  1.96  
#>  3  0.857  1.17   1.07  -0.810 
#>  4  0.946  0.273 -0.341 -0.371 
#>  5  1.38   0.393 -0.283  2.36  
#>  6 -2.35  -0.807 -0.849  0.0886
#>  7 -0.767 -1.14  -0.419 -0.438 
#>  8  0.640 -0.232 -0.712 -0.438 
#>  9  1.29   1.51   2.36   1.09  
#> 10  0.853  0.679  2.50   1.90  
#> # ℹ 58 more rows
```
