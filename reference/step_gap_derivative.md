# Gap-Segment Derivative Recipe Step

`step_gap_derivative()` creates a *specification* of a recipe step that
computes the gap-segment (Norris-Williams) derivative of the spectra
with
[`gap_derivative()`](https://christiangoueguel.com/specProc/reference/gap_derivative.md).

## Usage

``` r
step_gap_derivative(
  recipe,
  ...,
  derivative = 1,
  gap = 5,
  segment = 3,
  segments = TRUE,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("gap_derivative")
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

- derivative:

  The derivative: 1 (default) or 2.

- gap:

  The number of channels between two segments, an odd number. Default is
  5.

- segment:

  The number of channels averaged in each segment, an odd number.
  Default is 3.

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

The selected columns form one spectrum per row, and are replaced by
their derivative. Nothing is estimated from the training data. The
`derivative`, `gap` and `segment` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html)
(dials parameters
[`savgol_derivative()`](https://christiangoueguel.com/specProc/reference/savgol_derivative.md),
with values 1 and 2, and `window_size()`); an even `gap` or `segment`,
as a tuning grid may propose, is increased to the next odd number.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the `terms`, `derivative`, `gap`, `segment` and `id`.

## See also

[`gap_derivative()`](https://christiangoueguel.com/specProc/reference/gap_derivative.md),
[`step_savgol()`](https://christiangoueguel.com/specProc/reference/step_savgol.md),
[`step_bin_spectra()`](https://christiangoueguel.com/specProc/reference/step_bin_spectra.md)

## Examples

``` r
if (rlang::is_installed("recipes")) {
  data(forageLIBS)
  rec <- recipes::recipe(K ~ ., data = forageLIBS[-c(1:10, 12:14)]) |>
    step_gap_derivative(recipes::all_predictors(), gap = 5, segment = 3) |>
    recipes::prep()
  recipes::tidy(rec, number = 1)
}
#> # A tibble: 7,152 × 5
#>    terms       derivative   gap segment id                  
#>    <chr>            <dbl> <dbl>   <dbl> <chr>               
#>  1 199.3771616          1     5       3 gap_derivative_yTKKH
#>  2 199.4644141          1     5       3 gap_derivative_yTKKH
#>  3 199.5516666          1     5       3 gap_derivative_yTKKH
#>  4 199.6389192          1     5       3 gap_derivative_yTKKH
#>  5 199.7261717          1     5       3 gap_derivative_yTKKH
#>  6 199.8134242          1     5       3 gap_derivative_yTKKH
#>  7 199.9006767          1     5       3 gap_derivative_yTKKH
#>  8 199.9879292          1     5       3 gap_derivative_yTKKH
#>  9 200.0751817          1     5       3 gap_derivative_yTKKH
#> 10 200.1624343          1     5       3 gap_derivative_yTKKH
#> # ℹ 7,142 more rows
```
