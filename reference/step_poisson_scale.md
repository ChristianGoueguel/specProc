# Poisson Scaling Recipe Step

`step_poisson_scale()` creates a *specification* of a recipe step that
divides each selected column by the square root of its mean plus an
offset, as
[`poisson_scale()`](https://christiangoueguel.com/specProc/reference/poisson_scale.md)
does (column mode).

## Usage

``` r
step_poisson_scale(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  offset = 3,
  scales = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("poisson_scale")
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

- offset:

  The offset, in percent of the largest column mean. Default is 3.

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

Poisson scaling suits count data, whose variance grows with the mean.
The scales \\\sqrt{\bar{x}\_j + c}\\ are estimated from the training
data, where the offset \\c\\ is `offset` percent of the largest column
mean, and are applied unchanged to new data.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, the `scale` each column is divided by
(`NA` before the step is trained) and `id`.

## See also

[`poisson_scale()`](https://christiangoueguel.com/specProc/reference/poisson_scale.md),
[`step_pareto_scale()`](https://christiangoueguel.com/specProc/reference/step_pareto_scale.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_poisson_scale(recipes::all_predictors())
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 246
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1          21.1          20.9          20.4          20.3          20.0
#>  2          20.4          19.3          19.7          19.7          19.3
#>  3          21.9          22.7          23.1          22.0          21.6
#>  4          20.6          21.1          21.6          20.6          20.3
#>  5          19.9          20.6          21.1          21.7          21.4
#>  6          23.4          23.3          23.3          22.9          22.1
#>  7          21.5          22.3          22.0          21.3          20.7
#>  8          22.1          21.2          21.0          21.2          20.5
#>  9          19.9          20.1          20.0          20.4          19.8
#> 10          21.6          22.1          21.6          22.0          22.1
#> # ℹ 58 more rows
#> # ℹ 241 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
