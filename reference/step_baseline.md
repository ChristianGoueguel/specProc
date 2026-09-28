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
data(soilLIBS)
spectra <- soilLIBS[c(1, 9:400)]  # sample id and spectral channels

rec <- recipe(~ ., data = spectra[1:16, ]) |>
  update_role(Sample, new_role = "id") |>
  step_baseline(all_predictors(), method = "arpls", lambda = 1e5) |>
  step_snv(all_predictors())
prepped <- prep(rec)
bake(prepped, new_data = spectra[17:24, ])[, 1:6]
#> # A tibble: 8 × 6
#>   Sample   `199.3771616` `199.4644141` `199.5516666` `199.6389192` `199.7261717`
#>   <chr>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#> 1 LSG-S18…        -0.597       -0.598          0.253        0.427         -0.372
#> 2 LSG-S18…        -0.196        0.0979        -0.458       -0.718         -0.495
#> 3 LSG-S18…        -0.337        0.198         -0.424       -0.514         -0.407
#> 4 LSG-S18…        -0.481       -0.383         -0.169       -0.301         -0.588
#> 5 LSG-S18…        -0.407       -0.365         -0.332       -0.226         -0.202
#> 6 LSG-S18…        -0.607       -0.0601        -0.391        0.228         -0.449
#> 7 LSG-S18…        -0.311       -0.278         -0.336       -0.0780        -0.388
#> 8 LSG-S18…        -0.241       -0.344         -0.535       -0.372         -0.573
```
