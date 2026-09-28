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
[`tune::tune()`](https://tune.tidymodels.org/reference/reexports.html),
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
set.seed(1)
x <- matrix(rnorm(80 * 10), 80, 10) %*% diag(10:1)
x[1:4, ] <- x[1:4, ] + 25
dat <- as.data.frame(x)
rec <- recipe(~ ., data = dat) |>
  step_robpca(all_predictors(), num_comp = 3, distances = TRUE)
bake(prep(rec), new_data = NULL)
#> # A tibble: 80 × 5
#>     RPC1   RPC2    RPC3 RPC_SD RPC_OD
#>    <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
#>  1 17.4   9.67   -0.573  2.06   77.9 
#>  2 18.8  11.3   -11.2    2.81   82.2 
#>  3 16.5  24.4     2.43   3.41   80.8 
#>  4 40.9   7.75  -10.5    4.34   65.1 
#>  5 -5.17 -2.26   -5.11   0.944  13.2 
#>  6  2.84 13.1    19.2    3.26   14.6 
#>  7 -1.01  9.28   -4.92   1.35    7.24
#>  8 -3.28 -1.75  -11.8    1.79    7.53
#>  9  3.08  3.83   -4.40   0.854  12.4 
#> 10 -4.04 -0.602   3.40   0.639   8.82
#> # ℹ 70 more rows
```
