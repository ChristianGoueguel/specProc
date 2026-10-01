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
[`step_direct_osc()`](https://christiangoueguel.com/specProc/reference/step_direct_osc.md),
[`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md),
[`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md)
and
[`step_o2pls()`](https://christiangoueguel.com/specProc/reference/step_o2pls.md)
(which also handles several outcomes) remove response-orthogonal
variation with other algorithms.
[`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md)
and
[`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md)
remove variation described by external clutter, and
[`step_y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/step_y_gradient_glsw.md)
down-weights variation between samples with similar outcomes.

## Tuning

`num_comp` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html);
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
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
rec <- recipe(K ~ ., data = dat[1:300, ]) |>
  step_osc(all_predictors(), method = "fearn", num_comp = 2)
prepped <- prep(rec)
tidy(prepped, number = 1)
#> # A tibble: 245 × 3
#>    terms       num_comp id       
#>    <chr>          <dbl> <chr>    
#>  1 760.0161416        2 osc_uaqm9
#>  2 760.1001689        2 osc_uaqm9
#>  3 760.1841961        2 osc_uaqm9
#>  4 760.2682233        2 osc_uaqm9
#>  5 760.3522506        2 osc_uaqm9
#>  6 760.4362778        2 osc_uaqm9
#>  7 760.520305         2 osc_uaqm9
#>  8 760.6043323        2 osc_uaqm9
#>  9 760.6883595        2 osc_uaqm9
#> 10 760.7723867        2 osc_uaqm9
#> # ℹ 235 more rows
# new spectra are corrected with the filter estimated on the training data
bake(prepped, new_data = dat[301:368, -1])
#> # A tibble: 68 × 245
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1        -46.5         -67.3          -95.9        -89.7         -97.1 
#>  2        -57.4        -134.          -112.         -97.1        -109.  
#>  3        -40.1          -3.71          20.9        -31.1         -45.4 
#>  4        -81.2         -67.2          -38.3        -79.2         -89.5 
#>  5       -111.          -89.4          -62.1        -18.4         -25.5 
#>  6         21.5           4.08          13.9         -6.85        -41.7 
#>  7        -58.0         -31.3          -39.1        -72.1         -96.3 
#>  8         -5.23        -68.6          -72.9        -54.9         -85.3 
#>  9        -88.8         -91.0          -96.8        -59.1         -86.1 
#> 10        -24.9          -9.84         -40.0         -4.57          6.69
#> # ℹ 58 more rows
#> # ℹ 240 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
