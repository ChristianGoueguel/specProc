# Pareto Scaling Recipe Step

`step_pareto_scale()` creates a *specification* of a recipe step that
divides each selected column by the square root of its standard
deviation, as
[`pareto_scale()`](https://christiangoueguel.com/specProc/reference/pareto_scale.md)
does.

## Usage

``` r
step_pareto_scale(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  scales = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("pareto_scale")
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

- scales:

  The scale of each column, stored once the step has been trained.

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

The standard deviations are estimated from the training data and applied
unchanged to new data. Columns with zero standard deviation are not
scaled. The data are not centered; add
[`recipes::step_center()`](https://recipes.tidymodels.org/reference/step_center.html)
if needed.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, the `scale` each column is divided by
(`NA` before the step is trained) and `id`.

## See also

[`pareto_scale()`](https://christiangoueguel.com/specProc/reference/pareto_scale.md),
[`step_poisson_scale()`](https://christiangoueguel.com/specProc/reference/step_poisson_scale.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_pareto_scale(recipes::all_predictors())
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 246
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1          142.          143.          139.          138.          136.
#>  2          138.          132.          134.          135.          132.
#>  3          148.          155.          157.          150.          148.
#>  4          139.          144.          147.          141.          138.
#>  5          135.          141.          144.          148.          146.
#>  6          158.          159.          159.          156.          151.
#>  7          145.          152.          150.          145.          141.
#>  8          149.          145.          143.          145.          140.
#>  9          134.          137.          136.          140.          135.
#> 10          146.          151.          147.          150.          151.
#> # ℹ 58 more rows
#> # ℹ 241 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
