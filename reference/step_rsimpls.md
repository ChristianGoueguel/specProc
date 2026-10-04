# Robust PLS (RSIMPLS) Recipe Step

`step_rsimpls()` creates a *specification* of a recipe step that
converts the selected variables into robust partial least squares
scores, with
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md).
It is the robust counterpart of
[`recipes::step_pls()`](https://recipes.tidymodels.org/reference/step_pls.html):
outlying spectra and wrong reference values have little influence on the
components.

## Usage

``` r
step_rsimpls(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  outcome = NULL,
  options = list(),
  prefix = "RPLS",
  distances = FALSE,
  keep_original_cols = FALSE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("rsimpls")
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

  The number of PLS components. Default is 2.

- outcome:

  The outcome variable(s), as bare names or a selector. If `NULL`
  (default), the outcomes of the recipe are used.

- options:

  A list of further arguments passed to
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md),
  such as `kmax`, `alpha` or `ndir`.

- prefix:

  The prefix of the new column names. Default is `"RPLS"`.

- distances:

  A logical: add the score and orthogonal distances as columns. Default
  is `FALSE`.

- keep_original_cols:

  A logical: keep the selected columns. Default is `FALSE`.

- res:

  The fitted RSIMPLS model, stored once the step has been trained.

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

The RSIMPLS model is estimated on the training data, with the outcome,
when the recipe is prepped, and new data are projected onto it when they
are baked; the outcome is not needed then. As the step uses the outcome,
it must be estimated on the training data only: in a workflow, it is
re-estimated for every resample. Several outcomes give one model of all
of them, as with
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md).

The new columns are named with `prefix` followed by the component
number, zero-padded as in
[`recipes::step_pls()`](https://recipes.tidymodels.org/reference/step_pls.html).
With `distances = TRUE`, two more columns hold the score distance
(`<prefix>_SD`) and the orthogonal distance (`<prefix>_OD`) of each
observation, which can be used to screen new spectra; the residual
distance needs the outcome and is not given. The selected columns are
removed unless `keep_original_cols = TRUE`.

## Tuning

`num_comp` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html),
using
[`dials::num_comp()`](https://dials.tidymodels.org/reference/num_comp.html)
with values 1 to 4.

## Tidying

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the weight vectors, which give the scores of the centered
predictors, as a tibble with columns `terms`, `value`, `component` and
`id`.

## See also

[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md),
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)

## Examples

``` r
library(recipes)
data(forageLIBS)
# calcium and the Ca II and Ca I lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "Ca"), which(wl > 380 & wl < 430))]
set.seed(1)
rec <- recipe(Ca ~ ., data = dat[1:300, ]) |>
  step_rsimpls(all_predictors(), num_comp = 4, distances = TRUE)
prepped <- prep(rec)
bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 7
#>       Ca   RPLS1  RPLS2   RPLS3   RPLS4 RPLS_SD RPLS_OD
#>    <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 0.431 -71560.  7320.  -5382. -4453.    3.52    5367.
#>  2 0.805 -59703.  1746.   2292.   -84.0   2.20   24442.
#>  3 0.786 -25026.  3078.   4549.  2617.    1.80    8245.
#>  4 0.807 -44758.  6942.   -574.   186.    1.92    7488.
#>  5 0.919 -29623.  8719.   3674.  4068.    2.50    5909.
#>  6 0.323 -50604. -3389. -10679. -5914.    3.89    9985.
#>  7 0.492 -59098.  5377.  -4231. -3331.    2.77    8628.
#>  8 0.737 -39337.  6365.   1124.  1412.    1.80    6731.
#>  9 0.894 -37038. 12018.    671.  3731.    2.77    4884.
#> 10 0.785 -23118.   529.  -1999.   635.    0.937   8914.
#> # ℹ 58 more rows
tidy(prepped, number = 1)
#> # A tibble: 2,376 × 4
#>    terms            value component id           
#>    <chr>            <dbl> <chr>     <chr>        
#>  1 380.011254   0.00132   RPLS1     rsimpls_4dMaH
#>  2 380.096143  -0.000204  RPLS1     rsimpls_4dMaH
#>  3 380.202312  -0.000402  RPLS1     rsimpls_4dMaH
#>  4 380.285294  -0.0000825 RPLS1     rsimpls_4dMaH
#>  5 380.3682761 -0.000886  RPLS1     rsimpls_4dMaH
#>  6 380.4512581 -0.000965  RPLS1     rsimpls_4dMaH
#>  7 380.5342402 -0.000194  RPLS1     rsimpls_4dMaH
#>  8 380.6172222 -0.0000846 RPLS1     rsimpls_4dMaH
#>  9 380.7002042 -0.00103   RPLS1     rsimpls_4dMaH
#> 10 380.7831863 -0.000634  RPLS1     rsimpls_4dMaH
#> # ℹ 2,366 more rows
```
