# Binning Recipe Step

`step_bin_spectra()` creates a *specification* of a recipe step that
replaces each group of `width` adjacent channels of the spectra by their
mean, with
[`bin_spectra()`](https://christiangoueguel.com/specProc/reference/bin_spectra.md).

## Usage

``` r
step_bin_spectra(
  recipe,
  ...,
  width = 3,
  segments = TRUE,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  bins = NULL,
  skip = FALSE,
  id = recipes::rand_id("bin_spectra")
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

- width:

  The number of adjacent channels averaged, a positive integer. Default
  is 3; `width = 1` returns the spectra unchanged.

- segments:

  A logical: bin the segments between gaps of the wavelength axis
  separately (`TRUE`, default). Needs wavelengths as names.

- role:

  The role of the binned columns. Default is `"predictor"`.

- trained:

  A logical indicating whether the step has been trained.

- columns:

  The names of the selected columns, stored once the step has been
  trained.

- bins:

  The groups of channels and the names of the binned columns, set when
  the recipe is prepped. Not to be set by the user.

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
fewer binned columns, named by their mean wavelength (see
[`bin_spectra()`](https://christiangoueguel.com/specProc/reference/bin_spectra.md)).
The groups of channels are set from the column names when the recipe is
prepped; nothing is estimated from the training data. The `width` can be
tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html)
(dials parameter `window_size()`).
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the `terms`, `width` and `id`.

## See also

[`bin_spectra()`](https://christiangoueguel.com/specProc/reference/bin_spectra.md),
[`step_gap_derivative()`](https://christiangoueguel.com/specProc/reference/step_gap_derivative.md),
[`step_spectral_norm()`](https://christiangoueguel.com/specProc/reference/step_spectral_norm.md)

## Examples

``` r
if (rlang::is_installed("recipes")) {
  data(forageLIBS)
  # binning, normalization to the total area and gap-segment derivative
  rec <- recipes::recipe(K ~ ., data = forageLIBS[-c(1:10, 12:14)]) |>
    step_bin_spectra(recipes::all_predictors(), width = 3) |>
    step_spectral_norm(recipes::all_predictors(), method = "area") |>
    step_gap_derivative(recipes::all_predictors(), gap = 3, segment = 1) |>
    recipes::prep()
  dim(recipes::bake(rec, new_data = NULL))
}
#> [1]  368 2385
```
