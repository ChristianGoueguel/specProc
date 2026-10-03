# Robust PCA (ROBPCA) Recipe Step

`step_robpca()` creates a *specification* of a recipe step that converts
the selected variables into robust principal component scores, with
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md).
It is the robust counterpart of
[`recipes::step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html).

## Usage

``` r
step_robpca(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  options = list(),
  prefix = "RPC",
  distances = FALSE,
  keep_original_cols = FALSE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("robpca")
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

  The role of the new score columns. Default is `"predictor"`.

- trained:

  A logical indicating whether the step has been trained.

- num_comp:

  The number of principal components. Default is 2.

- options:

  A list of further arguments passed to
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  such as `alpha` or `ndir`.

- prefix:

  The prefix of the new column names. Default is `"RPC"`.

- distances:

  A logical: add the score and orthogonal distances as columns. Default
  is `FALSE`.

- keep_original_cols:

  A logical: keep the selected columns. Default is `FALSE`.

- res:

  The fitted robust PCA model, stored once the step has been trained.

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

The robust PCA model is estimated on the training data when the recipe
is prepped, and new data are projected onto it when they are baked. The
new columns are named with `prefix` followed by the component number.
With `distances = TRUE`, two more columns hold the score distance
(`<prefix>_SD`) and orthogonal distance (`<prefix>_OD`) of each
observation, which can be used to screen outliers. The selected columns
are removed unless `keep_original_cols = TRUE`.

## Tuning

`num_comp` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html),
using
[`dials::num_comp()`](https://dials.tidymodels.org/reference/num_comp.html)
with values 1 to 4.

## Tidying

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the loadings as a tibble with columns `terms`, `value`,
`component` and `id`.

## See also

[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`step_rospca()`](https://christiangoueguel.com/specProc/reference/step_rospca.md),
[`step_macropca()`](https://christiangoueguel.com/specProc/reference/step_macropca.md)

## Examples

``` r
library(recipes)
data(forageLIBS)
# the 380-430 nm window (Ca II H and K lines)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 380 & wl < 430)]
set.seed(1)
rec <- recipe(~ ., data = spectra) |>
  step_robpca(all_predictors(), num_comp = 3, distances = TRUE)
bake(prep(rec), new_data = NULL)
#> # A tibble: 368 × 5
#>       RPC1    RPC2   RPC3 RPC_SD RPC_OD
#>      <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
#>  1  15595.   -848. -7049.  1.23   4585.
#>  2  22905.   6593.  2477.  1.26  10757.
#>  3  29122.   -938.  1470.  1.06   8361.
#>  4  51124. -16874. 10181.  3.30   8297.
#>  5 -29159.   2879.   458.  1.10   9433.
#>  6  15121.   3329. -1164.  0.718  5557.
#>  7  53124. -20580.  4911.  3.42  17121.
#>  8  -6326.   5495. -6644.  1.29   5966.
#>  9 -27029.   9313.  3557.  1.66   8335.
#> 10  33735.   3858. 10648.  2.11   6894.
#> # ℹ 358 more rows
```
