# y-Gradient Generalized Least Squares Weighting Recipe Step

`step_y_gradient_glsw()` creates a *specification* of a recipe step that
down-weights variation between samples with similar outcomes, with the
filter of
[`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md).

## Usage

``` r
step_y_gradient_glsw(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  alpha = 0.01,
  window = 5,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("y_gradient_glsw")
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

- alpha:

  A positive number: the weighting parameter, relative to the largest
  eigenvalue of the clutter. Default is 0.01.

- window:

  An odd integer giving the width of the Savitzky-Golay window used to
  compute the gradients. Default is 5.

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

As in
[`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md),
`alpha` is relative to the largest eigenvalue of the (weighted) gradient
matrix, and the filter is stored in factored form. The filter uses the
outcome, so it is estimated on training data only; the outcome is not
needed when new data are baked. The selected columns are replaced by the
filtered values (not centered). `alpha` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html),
using
[`glsw_alpha()`](https://christiangoueguel.com/specProc/reference/glsw_alpha.md).
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, `alpha` (relative) and `id`.

## See also

[`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md),
[`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_y_gradient_glsw(recipes::all_predictors(), alpha = 0.01)
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 246
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1          707.          739.          718.          684.          669.
#>  2          713.          688.          721.          693.          673.
#>  3          686.          778.          803.          716.          694.
#>  4          682.          749.          785.          704.          686.
#>  5          656.          731.          766.          769.          755.
#>  6          753.          794.          802.          747.          705.
#>  7          725.          804.          802.          731.          701.
#>  8          756.          746.          748.          727.          689.
#>  9          670.          719.          724.          719.          684.
#> 10          719.          788.          763.          760.          764.
#> # ℹ 58 more rows
#> # ℹ 241 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
