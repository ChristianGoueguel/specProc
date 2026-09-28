# Emission Line Intensities Recipe Step

`step_line_intensities()` creates a *specification* of a recipe step
that replaces the spectral columns by the intensities of selected
emission lines, measured with
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md).

## Usage

``` r
step_line_intensities(
  recipe,
  ...,
  lines,
  half_width = 0.15,
  search = 0.2,
  method = "area",
  baseline = FALSE,
  prefix = "line_",
  keep_original_cols = FALSE,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("line_intensities")
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

- lines:

  The lines: a numeric vector of wavelengths (nm), preferably named,
  such as `c(Ca = 393.37, Mg = 279.55)`.

- half_width:

  The half-width of the integration window around the peak, in nm.
  Default is 0.15.

- search:

  The half-width of the window in which the peak is searched around the
  tabulated wavelength, in nm. Default is 0.2.

- method:

  `"area"` (default) or `"height"`.

- baseline:

  A logical: subtract a linear baseline under each line (`FALSE`,
  default).

- prefix:

  The prefix of the new column names. Default is `"line_"`.

- keep_original_cols:

  A logical: keep the spectral columns (`FALSE`, default).

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

The selected columns form one spectrum per row, and their names must be
their wavelengths (in nm). Each line becomes a new column, named by
`prefix` and the name of the line (or its wavelength). Nothing is
estimated from the training data. The Voigt method of
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)
is not offered, as it is too slow for resampling.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the `line` names, their `wavelength`, the `method` and `id`.

## See also

[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md),
[`step_line_ratio()`](https://christiangoueguel.com/specProc/reference/step_line_ratio.md)

## Examples

``` r
if (rlang::is_installed("recipes")) {
  data(soilLIBS)
  rec <- recipes::recipe(Clay ~ ., data = soilLIBS[-c(1:2, 4:8)]) |>
    step_line_intensities(recipes::all_predictors(),
                          lines = c(Mg = 279.55, Ca = 393.37, Al = 396.15)) |>
    recipes::prep()
  head(recipes::bake(rec, new_data = NULL))
}
#> # A tibble: 6 × 4
#>    Clay line_Mg line_Ca line_Al
#>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1    41   3305.   3348.   1772.
#> 2    41   3025.   3198.   1725.
#> 3    41   2966.   2980.   1623.
#> 4    41   2853.   2948.   1702.
#> 5    41   2980.   3206.   1646.
#> 6    41   2864.   3214.   1590.
```
