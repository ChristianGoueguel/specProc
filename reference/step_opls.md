# Orthogonal Projections to Latent Structures (OPLS) Recipe Step

`step_opls()` creates a *specification* of a recipe step that removes
`num_comp` orthogonal components from the selected predictors with the
OPLS model of
[`opls()`](https://christiangoueguel.com/specProc/reference/opls.md):
the selected columns are replaced by the OPLS-filtered data.

## Usage

``` r
step_opls(
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
  id = recipes::rand_id("opls")
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
  [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md):
  only `scale` (`"center"` by default; `"none"`, `"pareto"` or
  `"standard"`).

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

The filtered data are the same as those of
[`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md)
with the same `num_comp`, but
[`opls()`](https://christiangoueguel.com/specProc/reference/opls.md)
also offers Pareto scaling (`options = list(scale = "pareto")`). The
model is fitted without cross-validation or permutation test, which do
not change the filter. The number of orthogonal components is not
selected automatically: tune `num_comp` instead. As for
[`step_osc()`](https://christiangoueguel.com/specProc/reference/step_osc.md),
the filter uses the outcome and is estimated on the training data only;
the outcome is not needed when new data are baked.

## Tuning

`num_comp` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html);
its default range is
[`dials::num_comp()`](https://dials.tidymodels.org/reference/num_comp.html)
with values 1 to 4.

## Tidying

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns a tibble with columns `terms` (the selected predictors),
`num_comp` and `id`.

## See also

[`opls()`](https://christiangoueguel.com/specProc/reference/opls.md),
[`predict.specproc_opls()`](https://christiangoueguel.com/specProc/reference/predict.specproc_opls.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(40 * 30), 40, 30, dimnames = list(NULL, paste0("v", 1:30)))
dat <- data.frame(y = x[, 1] + rnorm(40, sd = 0.1), x)
rec <- recipes::recipe(y ~ ., data = dat[1:30, ]) |>
  step_opls(recipes::all_predictors(), num_comp = 2, options = list(scale = "pareto"))
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[31:40, ])
#> # A tibble: 10 × 31
#>        v1      v2     v3      v4      v5      v6      v7      v8      v9     v10
#>     <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1  1.35   0.0698 -1.21   0.674  -0.328  -1.29   -0.301   0.808   0.933   1.81  
#>  2 -0.235 -0.484  -0.131  0.392  -0.229  -2.36    0.472  -0.0394  1.40    0.319 
#>  3  0.310 -0.0890  1.31   0.387  -0.559  -0.445  -1.08   -0.892   0.406   1.15  
#>  4 -0.156 -0.888  -0.679 -0.694   0.316   0.588   2.80   -0.859   0.0436  0.337 
#>  5 -1.53  -1.58   -0.332 -1.08   -1.39    0.0207  0.0949 -0.327   1.66   -0.0379
#>  6 -0.531 -0.297  -0.518 -0.424  -0.970   0.112   1.000   0.733   0.733  -0.916 
#>  7 -0.529 -0.296  -0.104  1.42    0.865   0.795  -2.32   -0.666  -0.597   1.40  
#>  8 -0.169 -0.514  -0.292  0.0740 -0.994  -0.827   0.607  -0.495   0.140  -0.0481
#>  9  1.04   0.0750  0.548 -1.10   -0.0467  1.14   -1.34   -1.55   -1.62   -0.719 
#> 10  0.723 -0.0962 -0.447  1.66   -1.34   -0.445   1.22    0.521  -0.401   1.26  
#> # ℹ 21 more variables: v11 <dbl>, v12 <dbl>, v13 <dbl>, v14 <dbl>, v15 <dbl>,
#> #   v16 <dbl>, v17 <dbl>, v18 <dbl>, v19 <dbl>, v20 <dbl>, v21 <dbl>,
#> #   v22 <dbl>, v23 <dbl>, v24 <dbl>, v25 <dbl>, v26 <dbl>, v27 <dbl>,
#> #   v28 <dbl>, v29 <dbl>, v30 <dbl>, y <dbl>
```
