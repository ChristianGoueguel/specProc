# Wavelet Denoising Recipe Step

`step_wavelet_denoise()` creates a *specification* of a recipe step that
denoises the spectra with
[`wavelet_denoise()`](https://christiangoueguel.com/specProc/reference/wavelet_denoise.md).

## Usage

``` r
step_wavelet_denoise(
  recipe,
  ...,
  wavelet = "la8",
  level = 2,
  threshold = "sure",
  rule = "hard",
  segments = TRUE,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("wavelet_denoise")
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

  The wavelet: `"la8"` (default; Daubechies' least asymmetric wavelet
  with 8 coefficients, whose nearly symmetric shape suits spectral
  lines), `"haar"`, `"d4"`, `"d6"` or `"d8"` (see
  [`wavelet_features()`](https://christiangoueguel.com/specProc/reference/wavelet_features.md)).

- level:

  The number of levels thresholded. Default is 2: the scales of 2 to 4
  channels.

- threshold:

  The threshold: `"sure"` (default) or `"universal"`.

- rule:

  The thresholding rule: `"hard"` (default) or `"soft"`.

- segments:

  A logical: denoise the segments between gaps of the wavelength axis
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
denoised values. Nothing is estimated from the training data: the noise
and thresholds are those of each spectrum. The `level` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html)
(dials parameter
[`wavelet_level()`](https://christiangoueguel.com/specProc/reference/wavelet_level.md),
from 1 to 4 by default).
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the `terms`, `wavelet`, `level`, `threshold`, `rule` and `id`.

## See also

[`wavelet_denoise()`](https://christiangoueguel.com/specProc/reference/wavelet_denoise.md),
[`step_savgol()`](https://christiangoueguel.com/specProc/reference/step_savgol.md),
[`step_wavelet()`](https://christiangoueguel.com/specProc/reference/step_wavelet.md)

## Examples

``` r
if (rlang::is_installed("recipes")) {
  data(forageShots)
  rec <- recipes::recipe(K ~ ., data = forageShots[-c(1:4)]) |>
    step_wavelet_denoise(recipes::all_predictors()) |>
    recipes::prep()
  recipes::tidy(rec, number = 1)
}
#> # A tibble: 839 × 6
#>    terms       wavelet level threshold rule  id                   
#>    <chr>       <chr>   <dbl> <chr>     <chr> <chr>                
#>  1 380.011254  la8         2 sure      hard  wavelet_denoise_GaisT
#>  2 380.096143  la8         2 sure      hard  wavelet_denoise_GaisT
#>  3 380.202312  la8         2 sure      hard  wavelet_denoise_GaisT
#>  4 380.285294  la8         2 sure      hard  wavelet_denoise_GaisT
#>  5 380.3682761 la8         2 sure      hard  wavelet_denoise_GaisT
#>  6 380.4512581 la8         2 sure      hard  wavelet_denoise_GaisT
#>  7 380.5342402 la8         2 sure      hard  wavelet_denoise_GaisT
#>  8 380.6172222 la8         2 sure      hard  wavelet_denoise_GaisT
#>  9 380.7002042 la8         2 sure      hard  wavelet_denoise_GaisT
#> 10 380.7831863 la8         2 sure      hard  wavelet_denoise_GaisT
#> # ℹ 829 more rows
```
