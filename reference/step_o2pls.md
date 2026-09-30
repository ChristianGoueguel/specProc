# O2PLS Filter Recipe Step for One or Several Outcomes

`step_o2pls()` creates a *specification* of a recipe step that removes
`num_comp` outcome-orthogonal components from the selected predictors
with
[`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md).
Unlike the other orthogonalization steps, the filter can be estimated
against several outcomes at once.

## Usage

``` r
step_o2pls(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  num_comp = 2,
  joint_comp = 1,
  options = list(),
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("o2pls")
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

  The outcome variables, as bare names or selectors. If `NULL`
  (default), all the outcomes of the recipe are used.

- num_comp:

  The number of outcome-orthogonal components to remove (`nx` in
  [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md)).

- joint_comp:

  The number of joint (predictive) components (`ncomp` in
  [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md)),
  at most the number of outcomes. Default is 1.

- options:

  A list of further arguments passed to
  [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md):
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

The joint (predictive) directions are the `joint_comp` dominant singular
vectors of \\\textbf{Y}^T\textbf{X}\\, and the removed components are
the systematic variation of the predictors orthogonal to them (Trygg and
Wold, 2003). Only the predictors are filtered: the outcomes are left
unchanged, so that models are fitted to, and assessed on, the measured
values. (The outcome-side filtering of
[`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md),
its `ny` argument, does not change the filtered predictors.)

With a single outcome and `joint_comp = 1`, the filtered data are those
of
[`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md)
and
[`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md)
with the same `num_comp`.

The selected columns are replaced by the filtered values, which are
centered (and scaled, if `options = list(scale = TRUE)`). The filter
uses the outcomes, so it is estimated on the training data only (on the
analysis set of each resample within
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html));
the outcomes are not needed when new data are baked.

## Several outcomes

Declare the outcomes in the recipe formula; a model that handles several
outcomes, such as
[`parsnip::pls()`](https://parsnip.tidymodels.org/reference/pls.html)
with the mixOmics engine, then predicts them all, with one column per
outcome (`.pred_K`, `.pred_Ca`, ...):

    rec <- recipe(K + Ca ~ ., data = spectra) |>
      step_o2pls(all_predictors(), num_comp = 2, joint_comp = 2)
    model <- parsnip::pls(num_comp = 2) |>
      set_mode("regression") |>
      set_engine("mixOmics", scale = FALSE)
    fitted <- fit(workflow(rec, model), data = spectra)
    predict(fitted, new_data = new_spectra)

Use `outcome` to estimate the filter against some of the outcomes only.

## Tuning

`num_comp` and `joint_comp` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html)
when the recipe has a single outcome. Their default ranges are
[`dials::num_comp()`](https://dials.tidymodels.org/reference/num_comp.html)
with values 1 to 4 and 1 to 3; `joint_comp` cannot exceed the number of
outcomes.

[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
does not support several outcomes. Instead, create one workflow per
outcome with the workflowsets package, and tune them all with
`workflowsets::workflow_map("tune_grid", ...)`. Each recipe has a single
outcome, for the model, but the filter can still be estimated against
all the outcomes: give the others a role of their own (`"reference"`
here), not needed to bake new data, and select them with `outcome`. Each
outcome gets its own number of filter and model components:

    outcomes <- c("K", "Ca", "Mg")
    recipes <- purrr::map(outcomes, \(y) {
      recipe(reformulate(".", response = y), data = spectra) |>
        update_role(all_of(setdiff(outcomes, y)), new_role = "reference") |>
        update_role_requirements("reference", bake = FALSE) |>
        step_o2pls(all_predictors(), outcome = c(all_outcomes(), has_role("reference")),
                   num_comp = tune("filter"), joint_comp = length(outcomes))
    }) |>
      purrr::set_names(outcomes)
    model <- parsnip::pls(num_comp = tune()) |>
      set_mode("regression") |>
      set_engine("mixOmics", scale = FALSE)

    wf_set <- workflow_set(preproc = recipes, models = list(pls = model))
    res <- workflow_map(wf_set, "tune_grid", resamples = vfold_cv(spectra, v = 5),
                        grid = 10, metrics = metric_set(rmse), seed = 1)

    # best settings and final model of each outcome
    fits <- purrr::map(purrr::set_names(res$wflow_id), \(id) {
      best <- select_best(extract_workflow_set_result(res, id), metric = "rmse")
      extract_workflow(res, id) |>
        finalize_workflow(best) |>
        fit(data = spectra)
    })
    purrr::map(fits, \(f) predict(f, new_data = new_spectra))

Select the outcomes by role
([`all_outcomes()`](https://recipes.tidymodels.org/reference/has_role.html),
[`has_role()`](https://recipes.tidymodels.org/reference/has_role.html))
rather than with `all_of(outcomes)`, which refers to a variable outside
the recipe. To filter each outcome against itself only, drop the other
outcomes from its recipe instead; the step is then equivalent to
[`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md).
Read the results outcome by outcome: `workflowsets::rank_results()`
would rank workflows of different outcomes, whose RMSE are in different
units.

## Tidying

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns a tibble with columns `terms` (the selected predictors),
`num_comp`, `joint_comp` and `id`.

## References

- Trygg, J., Wold, S., (2003). O2-PLS, a two-block (X–Y) latent variable
  regression (LVR) method with an integral OSC filter. J. Chemom.
  17(1):53–64.

## See also

[`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md),
[predict.o2pls()](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(40 * 30), 40, 30, dimnames = list(NULL, paste0("v", 1:30)))
dat <- data.frame(y1 = x[, 1] + rnorm(40, sd = 0.1), y2 = x[, 2] - x[, 3], x)
rec <- recipes::recipe(y1 + y2 ~ ., data = dat[1:30, ]) |>
  step_o2pls(recipes::all_predictors(), num_comp = 2, joint_comp = 2)
prepped <- recipes::prep(rec)
recipes::tidy(prepped, number = 1)
#> # A tibble: 30 × 4
#>    terms num_comp joint_comp id         
#>    <chr>    <dbl>      <dbl> <chr>      
#>  1 v1           2          2 o2pls_nVyEO
#>  2 v2           2          2 o2pls_nVyEO
#>  3 v3           2          2 o2pls_nVyEO
#>  4 v4           2          2 o2pls_nVyEO
#>  5 v5           2          2 o2pls_nVyEO
#>  6 v6           2          2 o2pls_nVyEO
#>  7 v7           2          2 o2pls_nVyEO
#>  8 v8           2          2 o2pls_nVyEO
#>  9 v9           2          2 o2pls_nVyEO
#> 10 v10          2          2 o2pls_nVyEO
#> # ℹ 20 more rows
recipes::bake(prepped, new_data = dat[31:40, ])
#> # A tibble: 10 × 32
#>        v1     v2      v3     v4     v5      v6     v7     v8      v9     v10
#>     <dbl>  <dbl>   <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>   <dbl>   <dbl>
#>  1  1.29   0.203 -0.861   0.615 -0.627 -1.47   -0.186  0.711  0.847   1.89  
#>  2 -0.206 -0.552 -0.257   0.432 -0.102 -2.45    0.462  0.141  1.33    0.231 
#>  3  0.295  0.430  1.30    0.135 -0.892 -0.362  -0.932 -0.612  0.803   0.957 
#>  4 -0.145 -1.02  -0.693  -0.542  0.490  0.728   2.65  -0.787  0.0706  0.248 
#>  5 -1.47  -1.45  -0.358  -1.08  -1.42   0.0886  0.101 -0.224  1.82   -0.125 
#>  6 -0.508  0.348 -0.289  -0.733 -1.59   0.0191  1.23   1.01   0.813  -0.996 
#>  7 -0.488 -0.516 -0.346   1.42   1.13   0.812  -2.30  -0.591 -0.781   1.45  
#>  8 -0.145 -0.360 -0.593  -0.186 -1.02  -1.04    0.595 -0.393  0.0765  0.0854
#>  9  1.01  -0.131  0.336  -0.970  0.225  1.27   -1.38  -1.48  -1.65   -0.746 
#> 10  0.679 -0.547 -0.0872  2.08  -1.09  -0.231   1.10   0.367 -0.449   1.10  
#> # ℹ 22 more variables: v11 <dbl>, v12 <dbl>, v13 <dbl>, v14 <dbl>, v15 <dbl>,
#> #   v16 <dbl>, v17 <dbl>, v18 <dbl>, v19 <dbl>, v20 <dbl>, v21 <dbl>,
#> #   v22 <dbl>, v23 <dbl>, v24 <dbl>, v25 <dbl>, v26 <dbl>, v27 <dbl>,
#> #   v28 <dbl>, v29 <dbl>, v30 <dbl>, y1 <dbl>, y2 <dbl>
```
