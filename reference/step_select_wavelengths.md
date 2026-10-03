# Wavelength Selection Recipe Step

`step_select_wavelengths()` creates a *specification* of a recipe step
that keeps the predictors (wavelengths) selected by
[`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md)
and removes the others. The selection is made on the training data when
the recipe is prepped, so that within
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
it is repeated on the analysis set of every resample, as it must be: a
selection made on all the data would make the cross-validated error too
optimistic.

## Usage

``` r
step_select_wavelengths(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  method = "vip",
  num_terms = NULL,
  threshold = NULL,
  num_comp = 5,
  recursive = FALSE,
  intervals = 40,
  num_intervals = NULL,
  robust = FALSE,
  options = list(),
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("select_wavelengths")
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

- outcome:

  The outcome variable, as a bare name or a selector. If `NULL`
  (default), the single outcome of the recipe is used.

- method:

  The OSC algorithm: `"wold"`, `"sjoblom"` (default) or `"fearn"`. See
  [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md).

- num_terms:

  The number of variables to keep (`"vip"`, `"sr"`). If `NULL`
  (default), the variables above `threshold` are kept.

- threshold:

  The importance above which variables are kept when `num_terms = NULL`.
  If `NULL` (default), 1 for VIP; SR has no default threshold.

- num_comp:

  The number of components of the selection model (`ncomp` of
  [`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md)).
  Default is 5.

- recursive:

  If `TRUE`, remove the variables by backward elimination (`"vip"`,
  `"sr"`; requires `num_terms`). Default is `FALSE`.

- intervals:

  The number of contiguous intervals (`"ipls"`). Default is 40.

- num_intervals:

  The number of intervals to select (`"ipls"`). If `NULL` (default),
  intervals are added while the RMSECV decreases.

- robust:

  If `TRUE`, use the robust PLS model of
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md).
  Default is `FALSE`.

- options:

  A list of further arguments of
  [`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md):
  `prop_drop`, `folds`, and `alpha`, `ndir`, `nsamp` with
  `robust = TRUE`.

- res:

  The selection, stored once the step has been trained.

- columns:

  The names of the selected predictors, stored once the step has been
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

See
[`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md)
for the methods: `"vip"` and `"sr"` keep the `num_terms` most important
predictors (or those above `threshold`), and `"ipls"` selects
`num_intervals` contiguous intervals. The outcome is not needed when new
data are baked.

## Tuning

`num_terms`, `num_intervals` and `num_comp` (the components of the
selection model) can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html).
Their default ranges are
[`dials::num_terms()`](https://dials.tidymodels.org/reference/num_comp.html)
with 20 to 1000 predictors,
[`num_intervals()`](https://christiangoueguel.com/specProc/reference/num_intervals.md)
with 1 to 10 intervals, and
[`dials::num_comp()`](https://dials.tidymodels.org/reference/num_comp.html)
with 1 to 10 components. When the model also has a `num_comp` argument,
give the step's parameter its own id, such as
`num_comp = tune("select_comp")`.

## Tidying

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns a tibble with columns `terms` (the selected predictors),
`selected`, `importance` and `id`.

## See also

[`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md),
[`plot_wavelength_selection()`](https://christiangoueguel.com/specProc/reference/plot_wavelength_selection.md)

## Author

Christian L. Goueguel

## Examples

``` r
library(recipes)
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "Ca"), which(wl > 380 & wl < 430))]
rec <- recipe(Ca ~ ., data = dat[1:300, ]) |>
  step_select_wavelengths(all_predictors(), method = "sr", num_terms = 50)
prepped <- prep(rec)
ncol(bake(prepped, new_data = dat[301:368, -1]))
#> [1] 50
tidy(prepped, number = 1)
#> # A tibble: 594 × 4
#>    terms       selected importance id                      
#>    <chr>       <lgl>         <dbl> <chr>                   
#>  1 380.011254  FALSE     0.0356    select_wavelengths_iuuoY
#>  2 380.096143  FALSE     0.00637   select_wavelengths_iuuoY
#>  3 380.202312  FALSE     0.00273   select_wavelengths_iuuoY
#>  4 380.285294  FALSE     0.00274   select_wavelengths_iuuoY
#>  5 380.3682761 FALSE     0.000626  select_wavelengths_iuuoY
#>  6 380.4512581 FALSE     0.000356  select_wavelengths_iuuoY
#>  7 380.5342402 FALSE     0.00136   select_wavelengths_iuuoY
#>  8 380.6172222 FALSE     0.00337   select_wavelengths_iuuoY
#>  9 380.7002042 FALSE     0.0000189 select_wavelengths_iuuoY
#> 10 380.7831863 FALSE     0.000810  select_wavelengths_iuuoY
#> # ℹ 584 more rows
```
