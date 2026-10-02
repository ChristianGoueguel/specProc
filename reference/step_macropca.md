# MacroPCA Recipe Step

`step_macropca()` creates a *specification* of a recipe step that
converts the selected variables into robust principal component scores
with
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
which handles cellwise outliers and missing values as well as outlying
observations.

## Usage

``` r
step_macropca(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  options = list(),
  prefix = "MPC",
  distances = FALSE,
  keep_original_cols = FALSE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("macropca")
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
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
  such as `alpha` or MacroPCA parameters.

- prefix:

  The prefix of the new column names. Default is `"MPC"`.

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

As
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md).
Missing values are allowed in the selected columns, both when the recipe
is prepped and when it is baked.

## See also

[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
set.seed(1)
rec <- recipes::recipe(K ~ ., data = dat) |>
  step_macropca(recipes::all_predictors(), num_comp = 2)
recipes::bake(recipes::prep(rec), new_data = NULL)
#> # A tibble: 368 × 3
#>        K    MPC1   MPC2
#>    <dbl>   <dbl>  <dbl>
#>  1  3.68  31163.  1301.
#>  2  2.52  14962. -2339.
#>  3  2.45  27139. 13087.
#>  4  2.3   26085.  9377.
#>  5  2.87 -10016. -1414.
#>  6  2.16   4631. -2662.
#>  7  2.94  37568. 10883.
#>  8  2.43   8490. -4050.
#>  9  1.81 -10530. -2341.
#> 10  2.07  25678.  -538.
#> # ℹ 358 more rows
```
