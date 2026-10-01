# Savitzky-Golay Recipe Step

`step_savgol()` creates a *specification* of a recipe step that smooths
the spectra, or computes their derivative, with
[`savitzky_golay()`](https://christiangoueguel.com/specProc/reference/savitzky_golay.md).

## Usage

``` r
step_savgol(
  recipe,
  ...,
  window = 11,
  order = 2,
  derivative = 0,
  segments = TRUE,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("savgol")
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

- window:

  The width of the window, an odd number of channels. Default is 11.

- order:

  The degree of the polynomial, smaller than `window`. Default is 2.

- derivative:

  The derivative: 0 (smoothing, default), 1 or 2. It must not exceed
  `order`.

- segments:

  A logical: filter the segments between gaps of the wavelength axis
  separately (`TRUE`, default). Needs wavelengths as names.

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

The selected columns form one spectrum per row, and are replaced by the
filtered values. Nothing is estimated from the training data. The
`window`, `order` and `derivative` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html)
(dials parameters `window_size()`, `degree_int()` and
[`savgol_derivative()`](https://christiangoueguel.com/specProc/reference/savgol_derivative.md));
an even `window`, as a tuning grid may propose, is increased to the next
odd number.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the `terms`, `window`, `order`, `derivative` and `id`.

## See also

[`savitzky_golay()`](https://christiangoueguel.com/specProc/reference/savitzky_golay.md),
[`step_snv()`](https://christiangoueguel.com/specProc/reference/step_snv.md)

## Examples

``` r
if (rlang::is_installed("recipes")) {
  data(forageLIBS)
  rec <- recipes::recipe(K ~ ., data = forageLIBS[-c(1:10, 12:14)]) |>
    step_savgol(recipes::all_predictors(), window = 11, derivative = 1) |>
    recipes::prep()
  recipes::tidy(rec, number = 1)
}
#> # A tibble: 7,152 × 5
#>    terms       window order derivative id          
#>    <chr>        <dbl> <dbl>      <dbl> <chr>       
#>  1 199.3771616     11     2          1 savgol_L6XeZ
#>  2 199.4644141     11     2          1 savgol_L6XeZ
#>  3 199.5516666     11     2          1 savgol_L6XeZ
#>  4 199.6389192     11     2          1 savgol_L6XeZ
#>  5 199.7261717     11     2          1 savgol_L6XeZ
#>  6 199.8134242     11     2          1 savgol_L6XeZ
#>  7 199.9006767     11     2          1 savgol_L6XeZ
#>  8 199.9879292     11     2          1 savgol_L6XeZ
#>  9 200.0751817     11     2          1 savgol_L6XeZ
#> 10 200.1624343     11     2          1 savgol_L6XeZ
#> # ℹ 7,142 more rows
```
