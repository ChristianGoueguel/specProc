# Direct Orthogonalization Recipe Step

`step_direct_orthogonal()` creates a *specification* of a recipe step
that removes response-orthogonal variation from the selected predictors
with
[`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md).

## Usage

``` r
step_direct_orthogonal(
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
  id = recipes::rand_id("direct_orthogonal")
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
  [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md),
  such as `scale`.

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

The related steps `step_direct_orthogonal()`,
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

## See also

[`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md),
[`predict.specproc_filter()`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_direct_orthogonal(recipes::all_predictors(), num_comp = 2)
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 246
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1        -11.6          -37.2        -58.7         -57.9         -63.5 
#>  2        -18.9         -103.         -69.5         -63.6         -72.9 
#>  3        -16.1           20.1         44.1          -7.08        -21.1 
#>  4        -32.1          -22.5         12.3         -32.9         -41.3 
#>  5        -59.0          -42.1         -8.04         30.7          25.6 
#>  6         67.3           51.0         57.0          39.9           5.35
#>  7         30.0           53.5         47.9          14.1          -8.03
#>  8         47.2          -20.1        -19.5          -4.90        -33.5 
#>  9        -61.4          -70.4        -65.2         -36.3         -61.1 
#> 10          7.20          18.7         -6.49         25.2          37.9 
#> # ℹ 58 more rows
#> # ℹ 241 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
