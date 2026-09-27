# Orthogonal Signal Correction Recipe Step

`step_osc()` creates a *specification* of a recipe step that removes the
variation of the selected predictors that is orthogonal to the outcome,
with [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md).
The filter is estimated on the training data when the recipe is prepped
and applied to new data when it is baked.

## Usage

``` r
step_osc(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  method = "sjoblom",
  num_comp = 2,
  options = list(),
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("osc")
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

- method:

  The OSC algorithm: `"wold"`, `"sjoblom"` (default) or `"fearn"`. See
  [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md).

- num_comp:

  The number of orthogonal components to remove.

- options:

  A list of further arguments passed to the underlying function, such as
  `scale`, `tol` or `max.iter` for
  [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md).

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

The related steps
[`step_direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/step_direct_orthogonal.md),
[`step_direct_osc()`](https://christiangoueguel.com/specProc/reference/step_direct_osc.md)
and
[`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md)
remove response-orthogonal variation with other algorithms.
[`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md)
and
[`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md)
remove variation described by external clutter, and
[`step_y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/step_y_gradient_glsw.md)
down-weights variation between samples with similar outcomes.

## Tuning

`num_comp` can be tuned with
[`tune::tune()`](https://tune.tidymodels.org/reference/reexports.html);
its default range is
[`dials::num_comp()`](https://dials.tidymodels.org/reference/num_comp.html)
with values 1 to 4. When the model also has a `num_comp` argument (for
example,
[`parsnip::pls()`](https://parsnip.tidymodels.org/reference/pls.html)),
give the step's parameter its own id, such as
`num_comp = tune("filter_comp")`, so that the two can be tuned together:

    rec <- recipe(K ~ ., data = spectra) |>
      step_osc(all_predictors(), method = "fearn", num_comp = tune("filter_comp"))
    model <- parsnip::pls(num_comp = tune()) |>
      set_mode("regression") |>
      set_engine("mixOmics", scale = FALSE)  # center only, like pls::plsr()
    wf <- workflow(rec, model)
    tune_grid(wf, resamples = group_vfold_cv(spectra, group = Sample), grid = 9)

## Tidying

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns a tibble with columns `terms` (the selected predictors),
`num_comp` and `id`.

## See also

[`osc()`](https://christiangoueguel.com/specProc/reference/osc.md),
[`predict.specproc_filter()`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)

## Author

Christian L. Goueguel

## Examples

``` r
library(recipes)
set.seed(1)
x <- matrix(rnorm(60 * 20), 60, 20, dimnames = list(NULL, paste0("wl", 1:20)))
dat <- data.frame(y = x[, 1] + rnorm(60, sd = 0.1), x)

rec <- recipe(y ~ ., data = dat[1:40, ]) |>
  step_osc(all_predictors(), method = "fearn", num_comp = 2)
prepped <- prep(rec)
tidy(prepped, number = 1)
#> # A tibble: 20 × 3
#>    terms num_comp id       
#>    <chr>    <dbl> <chr>    
#>  1 wl1          2 osc_5GxgY
#>  2 wl2          2 osc_5GxgY
#>  3 wl3          2 osc_5GxgY
#>  4 wl4          2 osc_5GxgY
#>  5 wl5          2 osc_5GxgY
#>  6 wl6          2 osc_5GxgY
#>  7 wl7          2 osc_5GxgY
#>  8 wl8          2 osc_5GxgY
#>  9 wl9          2 osc_5GxgY
#> 10 wl10         2 osc_5GxgY
#> 11 wl11         2 osc_5GxgY
#> 12 wl12         2 osc_5GxgY
#> 13 wl13         2 osc_5GxgY
#> 14 wl14         2 osc_5GxgY
#> 15 wl15         2 osc_5GxgY
#> 16 wl16         2 osc_5GxgY
#> 17 wl17         2 osc_5GxgY
#> 18 wl18         2 osc_5GxgY
#> 19 wl19         2 osc_5GxgY
#> 20 wl20         2 osc_5GxgY

# new spectra are corrected with the filter estimated on the training data
bake(prepped, new_data = dat[41:60, -1])
#> # A tibble: 20 × 20
#>       wl1     wl2      wl3     wl4     wl5      wl6    wl7     wl8     wl9
#>     <dbl>   <dbl>    <dbl>   <dbl>   <dbl>    <dbl>  <dbl>   <dbl>   <dbl>
#>  1 -0.260 -0.550   0.755   -2.29    0.798   1.16     0.681  0.237   0.372 
#>  2 -0.319  0.347  -0.188   -0.415  -0.193  -1.36     1.49   1.91   -0.0221
#>  3  0.629 -0.743   1.13    -0.763   1.29   -0.306   -0.748  0.910  -1.64  
#>  4  0.483  0.303   1.02    -0.561  -0.642  -0.634   -0.586  0.481  -0.152 
#>  5 -0.789 -0.832  -0.228   -1.25   -0.575  -2.21    -0.381  0.640   0.433 
#>  6 -0.782  2.00    2.34     1.34   -0.730  -1.48    -0.773  0.318  -1.55  
#>  7  0.262  0.477   0.159   -0.330  -0.741   0.922   -0.239 -0.0488  0.252 
#>  8  0.721  1.30   -1.54    -1.79    0.921  -1.08    -0.579 -0.286  -0.277 
#>  9 -0.187  0.420   0.00298  0.204   0.297   0.255    1.45   1.30   -0.0381
#> 10  0.796  1.60    0.453    0.300   0.864   1.51    -0.617  1.20   -0.225 
#> 11  0.306 -0.763   2.62    -1.00   -0.470   1.29    -0.323 -0.464  -1.18  
#> 12 -0.692 -0.401   0.296   -3.06    0.402   0.401    0.531 -1.57    0.959 
#> 13  0.267  1.39    0.604   -0.432  -0.0701 -0.380    1.31  -0.397  -1.02  
#> 14 -1.21  -0.538   0.110    0.280  -1.29   -0.345   -1.00   0.544  -0.439 
#> 15  1.36  -0.225  -0.204    0.128   1.48    0.804   -0.350  1.02    0.506 
#> 16  1.91  -0.0176  0.0811  -0.861   0.668   0.793   -0.147 -1.19   -0.156 
#> 17 -0.426  0.211   0.761   -0.222   1.29   -1.25     0.265  0.0587  0.365 
#> 18 -1.11  -0.186   2.14    -1.10    0.729  -0.962   -0.212 -0.255   0.0582
#> 19  0.488  0.525   1.24     0.951  -0.0390 -2.18    -1.52   1.29    0.437 
#> 20 -0.251 -0.589   1.75     0.0815 -0.425  -0.00891  1.97   0.997   0.473 
#> # ℹ 11 more variables: wl10 <dbl>, wl11 <dbl>, wl12 <dbl>, wl13 <dbl>,
#> #   wl14 <dbl>, wl15 <dbl>, wl16 <dbl>, wl17 <dbl>, wl18 <dbl>, wl19 <dbl>,
#> #   wl20 <dbl>
```
