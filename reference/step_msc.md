# Multiplicative Scatter Correction Recipe Step

`step_msc()` creates a *specification* of a recipe step that corrects
each spectrum for multiplicative and additive effects relative to a
reference spectrum, with
[`msc()`](https://christiangoueguel.com/specProc/reference/msc.md).

## Usage

``` r
step_msc(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  robust = TRUE,
  drop.offset = TRUE,
  window = NULL,
  reference = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("msc")
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

- robust:

  A logical: use the median (`TRUE`, default) or the mean of the
  training spectra as the reference.

- drop.offset:

  A logical: remove the additive offset (`TRUE`, default) or only the
  multiplicative effect.

- window:

  An optional list of column index vectors (within the selected columns)
  for piecewise MSC. See
  [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md).

- reference:

  The reference spectrum, stored once the step has been trained.

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

The reference spectrum (the median or mean of the training spectra) is
estimated when the recipe is prepped, and new spectra are corrected
against it when they are baked. The selected columns form one spectrum
per row. They are replaced by the corrected values.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, the `reference` value of each (`NA` before
the step is trained) and `id`.

## See also

[`msc()`](https://christiangoueguel.com/specProc/reference/msc.md),
[`step_emsc()`](https://christiangoueguel.com/specProc/reference/step_emsc.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_msc(recipes::all_predictors())
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 246
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1         1206.         1198.         1163.         1155.         1132.
#>  2         1308.         1227.         1260.         1257.         1225.
#>  3         1147.         1198.         1217.         1156.         1131.
#>  4         1149.         1186.         1218.         1154.         1127.
#>  5         1131.         1179.         1211.         1249.         1226.
#>  6         1201.         1195.         1196.         1172.         1127.
#>  7         1130.         1184.         1167.         1117.         1073.
#>  8         1221.         1163.         1155.         1163.         1113.
#>  9         1213.         1230.         1226.         1253.         1203.
#> 10         1145.         1179.         1142.         1170.         1170.
#> # ℹ 58 more rows
#> # ℹ 241 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
