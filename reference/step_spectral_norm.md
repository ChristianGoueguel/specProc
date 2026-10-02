# Spectral Normalization Recipe Step

`step_spectral_norm()` creates a *specification* of a recipe step that
divides each spectrum by one of its norms, with
[`normalize()`](https://christiangoueguel.com/specProc/reference/normalize.md):
its total area, L1 norm, L2 (Euclidean) norm, or maximum absolute
intensity.

## Usage

``` r
step_spectral_norm(
  recipe,
  ...,
  method = "l1",
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("spectral_norm")
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

- method:

  The norm: `"l1"` (default), `"area"`, `"l2"` or `"max"`.

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
normalized values. Unlike
[`recipes::step_normalize()`](https://recipes.tidymodels.org/reference/step_normalize.html),
which scales each column (variable), this step scales each row
(spectrum), to remove multiplicative variation between spectra such as
shot-to-shot changes of the ablated mass. Nothing is estimated from the
training data. Spectra whose norm is zero are set to `NA`, with a
warning.

The methods are those of
[`normalize()`](https://christiangoueguel.com/specProc/reference/normalize.md):
`"l1"` (sum of absolute values, default), `"area"` (sum of values, equal
to `"l1"` for non-negative spectra), `"l2"` (Euclidean norm, also called
vector normalization) and `"max"` (largest absolute value). The `method`
can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html)
(dials parameter
[`spectral_norm_method()`](https://christiangoueguel.com/specProc/reference/spectral_norm_method.md)).
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the `terms`, the `method` and `id`.

## See also

[`normalize()`](https://christiangoueguel.com/specProc/reference/normalize.md),
[`step_snv()`](https://christiangoueguel.com/specProc/reference/step_snv.md),
[`step_line_ratio()`](https://christiangoueguel.com/specProc/reference/step_line_ratio.md)

## Author

Christian L. Goueguel

## Examples

``` r
if (rlang::is_installed("recipes")) {
  data(forageLIBS)
  # potassium and the K I resonance lines
  wl <- suppressWarnings(as.numeric(names(forageLIBS)))
  dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
  rec <- recipes::recipe(K ~ ., data = dat) |>
    step_baseline(recipes::all_predictors()) |>
    step_spectral_norm(recipes::all_predictors(), method = "l2") |>
    recipes::prep()
  baked <- recipes::bake(rec, new_data = NULL)
  spectra <- baked[setdiff(names(baked), "K")]
  rowSums(spectra[1:3, ]^2)   # 1
}
#> [1] 1 1 1
```
