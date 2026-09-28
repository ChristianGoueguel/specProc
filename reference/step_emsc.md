# Extended Multiplicative Signal Correction Recipe Step

`step_emsc()` creates a *specification* of a recipe step that corrects
each spectrum for multiplicative effects, a polynomial baseline and,
optionally, known interferents, with
[`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md).

## Usage

``` r
step_emsc(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  degree = 2,
  interferents = NULL,
  wavelength = NULL,
  robust = TRUE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("emsc")
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

- degree:

  The degree of the polynomial baseline. Default is 2.

- interferents:

  An optional numeric matrix or data frame of interferent spectra, one
  per row. If it has column names, the columns matching the selected
  predictors are used; otherwise it must have one column per selected
  predictor, in the same order.

- wavelength:

  An optional numeric vector of wavelengths, one per selected column,
  used to build the polynomials. If `NULL`, the column names are used
  when they are numeric, and the column positions otherwise.

- robust:

  A logical: use the median (`TRUE`, default) or the mean of the
  training spectra as the reference.

- res:

  The fitted EMSC model, stored once the step has been trained.

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

The reference spectrum is estimated from the training spectra when the
recipe is prepped, and new spectra are corrected with the same
reference, polynomials and interferents when they are baked. The
selected columns form one spectrum per row and are replaced by the
corrected values.

`degree` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html),
using
[`dials::degree_int()`](https://dials.tidymodels.org/reference/degree.html)
with values 0 to 4.
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, the `reference` value of each (`NA` before
the step is trained) and `id`.

## See also

[`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md),
[`step_msc()`](https://christiangoueguel.com/specProc/reference/step_msc.md)
