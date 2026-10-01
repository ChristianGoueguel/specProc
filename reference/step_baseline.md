# Baseline Correction Recipe Step

`step_baseline()` creates a *specification* of a recipe step that
subtracts a fitted baseline from each spectrum, with
[`baseline_arpls()`](https://christiangoueguel.com/specProc/reference/baseline_arpls.md),
[`baseline_als()`](https://christiangoueguel.com/specProc/reference/baseline_als.md)
or
[`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md).

## Usage

``` r
step_baseline(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  method = "arpls",
  lambda = 1000,
  degree = 4,
  options = list(),
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("baseline")
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

- method:

  The baseline algorithm: `"arpls"` (default), `"als"` or `"lsp"`.

- lambda:

  The smoothing parameter of the `"arpls"` and `"als"` methods. Default
  is 1000.

- degree:

  The polynomial degree of the `"lsp"` method. Default is 4.

- options:

  A list of further arguments passed to the baseline function: `ratio`
  and `max.iter` for
  [`baseline_arpls()`](https://christiangoueguel.com/specProc/reference/baseline_arpls.md),
  `p` and `max.iter` for
  [`baseline_als()`](https://christiangoueguel.com/specProc/reference/baseline_als.md),
  `tol` and `max.iter` for
  [`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md).

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

The selected columns, in the order in which they appear in the data,
form one spectrum per row, so select all the channels of the spectrum
and only them. The baseline is fitted to each spectrum separately, so
nothing is estimated from the training data and the step gives the same
result whether it is applied before or after the data are split. The
selected columns are replaced by the corrected values.

## Tuning

`lambda` (methods `"arpls"` and `"als"`) can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html),
using
[`baseline_lambda()`](https://christiangoueguel.com/specProc/reference/baseline_lambda.md),
and `degree` (method `"lsp"`) using
[`dials::degree_int()`](https://dials.tidymodels.org/reference/degree.html)
with values 1 to 8.

## Tidying

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns a tibble with columns `terms`, `method` and `id`.

## See also

[`baseline_arpls()`](https://christiangoueguel.com/specProc/reference/baseline_arpls.md),
[`baseline_als()`](https://christiangoueguel.com/specProc/reference/baseline_als.md),
[`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md)

## Author

Christian L. Goueguel

## Examples

``` r
library(recipes)
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step
data(forageLIBS)
spectra <- forageLIBS[c(2, 15:406)]  # sample id and spectral channels

rec <- recipe(~ ., data = spectra[1:16, ]) |>
  update_role(Sample, new_role = "id") |>
  step_baseline(all_predictors(), method = "arpls", lambda = 1e5) |>
  step_snv(all_predictors())
prepped <- prep(rec)
bake(prepped, new_data = spectra[17:24, ])[, 1:6]
#> # A tibble: 8 × 6
#>   Sample   `199.3771616` `199.4644141` `199.5516666` `199.6389192` `199.7261717`
#>   <chr>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#> 1 FEN6585…        -0.363        -0.370        0.213       -0.327          -0.322
#> 2 FEN6585…        -0.329        -0.454        0.198       -0.164          -0.355
#> 3 FEN6585…        -0.396        -0.269        0.320        0.00691        -0.143
#> 4 FEN6585…        -0.296        -0.273        0.0430      -0.258          -0.277
#> 5 FEN6585…        -0.270        -0.374        0.189       -0.316          -0.351
#> 6 FEN6585…        -0.517        -0.544        0.499       -0.361          -0.184
#> 7 FEN6585…        -0.512        -0.211        0.422       -0.275          -0.220
#> 8 FEN6585…        -0.611        -0.528        0.620       -0.183          -0.552
```
