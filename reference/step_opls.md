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
  `center` and `scale`.

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
with the same `num_comp`. For Pareto scaling, add
[`step_pareto_scale()`](https://christiangoueguel.com/specProc/reference/step_pareto_scale.md)
before this step. The model is fitted without cross-validation or
permutation test, which do not change the filter. The number of
orthogonal components is not selected automatically: tune `num_comp`
instead. As for
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
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
# Pareto scaling, then the OPLS filter
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_pareto_scale(recipes::all_predictors()) |>
  step_opls(recipes::all_predictors(), num_comp = 2)
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 246
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1         0.374        -1.95         -5.28          -5.06        -5.87 
#>  2         6.18         -3.12          0.221          1.04        -0.104
#>  3         1.35          5.89          9.26           2.25         0.598
#>  4        -3.04         -0.904         2.97          -2.78        -3.96 
#>  5        -7.07         -3.95         -0.316          4.64         3.85 
#>  6         1.70         -0.558         0.631         -1.89        -6.36 
#>  7        -3.61          0.320        -0.649         -5.13        -8.26 
#>  8         1.97         -5.57         -6.05          -4.06        -7.92 
#>  9        -1.81         -1.80         -2.12           1.83        -1.34 
#> 10        -5.98         -3.81         -7.68          -3.18        -1.88 
#> # ℹ 58 more rows
#> # ℹ 241 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
