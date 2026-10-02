# Internal Standard Normalization Recipe Step

`step_line_ratio()` creates a *specification* of a recipe step that
divides each spectrum by the intensity of a reference emission line,
such as a line of a matrix element of constant concentration (internal
standard). This compensates for shot-to-shot changes of the ablated mass
and of the plasma conditions.

## Usage

``` r
step_line_ratio(
  recipe,
  ...,
  reference,
  window = 0.1,
  method = "area",
  baseline = FALSE,
  role = NA,
  trained = FALSE,
  columns = NULL,
  channels = NULL,
  skip = FALSE,
  id = recipes::rand_id("line_ratio")
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

- reference:

  The wavelength(s) of the reference line(s), in nm.

- window:

  The half-width of the window around each reference wavelength, in nm.
  Default is 0.1.

- method:

  The intensity of the reference line: `"area"` (default) or `"height"`.

- baseline:

  A logical: subtract a linear baseline under the reference line
  (`FALSE`, default).

- role:

  Not used by this step, since no new variables are created.

- trained:

  A logical indicating whether the step has been trained.

- columns:

  The names of the selected columns, stored once the step has been
  trained.

- channels:

  The channels of each reference window, found when the recipe is
  prepped. Not to be set by the user.

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
their wavelengths (in nm). The intensity of the reference line is
measured in each spectrum over the channels within `window` nm of
`reference`:

- `method = "area"` (default): the integrated intensity (trapezoidal
  rule), more robust to noise and to small wavelength shifts;

- `method = "height"`: the maximum intensity.

With `baseline = TRUE`, a straight line through the intensities at both
ends of the window is subtracted first, so that the continuum under the
line does not contribute. When several reference wavelengths are given,
their intensities are summed.

Nothing is estimated from the training data: the channels of the window
are found when the recipe is prepped, and each spectrum is normalized
independently when it is baked. The selected columns are replaced by the
normalized values. Spectra whose reference intensity is not positive are
set to `NA`, with a warning.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the `reference` wavelengths, the `window`, the `method` and
`id`.

Choose a line that is well resolved, not self-absorbed and not saturated
(see
[`saturation_summary()`](https://christiangoueguel.com/specProc/reference/saturation_summary.md)
and
[`self_absorption()`](https://christiangoueguel.com/specProc/reference/self_absorption.md)),
and from an upper level close to that of the analyte lines, so that the
ratio depends little on the plasma temperature.

## References

- Body, D., Chadwick, B.L., (2001). Optimization of the spectral data
  processing in a LIBS simultaneous elemental analysis system.
  Spectrochimica Acta Part B, 56(6):725-736.

## See also

[`normalize()`](https://christiangoueguel.com/specProc/reference/normalize.md),
[`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md)

## Author

Christian L. Goueguel

## Examples

``` r
if (rlang::is_installed("recipes")) {
  data(forageLIBS)
  # normalize to the C I 247.86 nm line of the organic matrix
  rec <- recipes::recipe(~ ., data = forageLIBS[-(1:14)]) |>
    step_line_ratio(recipes::all_numeric(), reference = 247.856, window = 0.15) |>
    recipes::prep()
  recipes::tidy(rec, number = 1)
}
#> # A tibble: 1 × 5
#>   reference window method baseline id              
#>       <dbl>  <dbl> <chr>  <lgl>    <chr>           
#> 1      248.   0.15 area   FALSE    line_ratio_g3WPq
```
