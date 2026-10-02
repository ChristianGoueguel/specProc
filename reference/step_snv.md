# Standard Normal Variate Recipe Step

`step_snv()` creates a *specification* of a recipe step that centers and
scales each spectrum by its own mean and standard deviation, with
[`snv()`](https://christiangoueguel.com/specProc/reference/snv.md).

## Usage

``` r
step_snv(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("snv")
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

The selected columns form one spectrum per row; select all the channels
of the spectrum and only them. Nothing is estimated from the training
data. The selected columns are replaced by the SNV-transformed values.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms` and `id`.

## See also

[`snv()`](https://christiangoueguel.com/specProc/reference/snv.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_snv(recipes::all_predictors())
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 246
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1        -0.376        -0.377        -0.382        -0.383        -0.385
#>  2        -0.362        -0.372        -0.368        -0.368        -0.372
#>  3        -0.385        -0.378        -0.376        -0.384        -0.387
#>  4        -0.382        -0.378        -0.374        -0.382        -0.385
#>  5        -0.384        -0.378        -0.374        -0.369        -0.372
#>  6        -0.378        -0.378        -0.378        -0.381        -0.387
#>  7        -0.381        -0.374        -0.376        -0.382        -0.388
#>  8        -0.373        -0.381        -0.382        -0.381        -0.387
#>  9        -0.375        -0.373        -0.374        -0.370        -0.377
#> 10        -0.384        -0.380        -0.385        -0.381        -0.381
#> # ℹ 58 more rows
#> # ℹ 241 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
