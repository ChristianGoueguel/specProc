# Wavelet Features Recipe Step

`step_wavelet()` creates a *specification* of a recipe step that
replaces the spectral columns by their wavelet coefficients, computed
with
[`wavelet_features()`](https://christiangoueguel.com/specProc/reference/wavelet_features.md),
optionally keeping only the coefficients of largest variance in the
training data.

## Usage

``` r
step_wavelet(
  recipe,
  ...,
  wavelet = "d4",
  level = 3,
  coefficients = "approximation",
  num_coef = NULL,
  prefix = "wav_",
  keep_original_cols = FALSE,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  res = NULL,
  skip = FALSE,
  id = recipes::rand_id("wavelet")
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

- wavelet:

  The wavelet: `"haar"`, `"d4"` (default), `"d6"`, `"d8"` or `"la8"`.

- level:

  The number of levels of the decomposition. Default is 3.

- coefficients:

  The coefficients returned: `"approximation"` (default) or `"all"`.

- num_coef:

  The number of coefficients to keep, those of largest variance in the
  training data, or `NULL` (default) to keep them all.

- prefix:

  The prefix of the new column names. Default is `"wav_"`.

- keep_original_cols:

  A logical: keep the spectral columns (`FALSE`, default).

- role:

  Not used by this step, since no new variables are created.

- trained:

  A logical indicating whether the step has been trained.

- columns:

  The names of the selected columns, stored once the step has been
  trained.

- res:

  The coefficients kept, stored once the step has been trained. Not to
  be set by the user.

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

The selected columns form one spectrum per row. With `num_coef`, the
variance of each coefficient is computed on the training data when the
recipe is prepped, and the `num_coef` coefficients of largest variance
are kept (Trygg and Wold, 1998); new data get the same coefficients. The
`level` and `num_coef` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html)
(dials parameters
[`wavelet_level()`](https://christiangoueguel.com/specProc/reference/wavelet_level.md)
and `num_terms()`).
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the names of the kept coefficients (`terms`), their `variance`
in the training data (`NA` before the step is trained) and `id`.

## See also

[`wavelet_features()`](https://christiangoueguel.com/specProc/reference/wavelet_features.md),
[`step_savgol()`](https://christiangoueguel.com/specProc/reference/step_savgol.md)

## Examples

``` r
if (rlang::is_installed("recipes")) {
  data(soilLIBS)
  rec <- recipes::recipe(Clay ~ ., data = soilLIBS[-c(1:2, 4:8)]) |>
    step_wavelet(recipes::all_predictors(), wavelet = "la8", level = 4, num_coef = 50) |>
    recipes::prep()
  dim(recipes::bake(rec, new_data = NULL))
}
#> [1] 400  51
```
