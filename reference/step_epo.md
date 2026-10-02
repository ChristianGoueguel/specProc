# External Parameter Orthogonalization Recipe Step

`step_epo()` creates a *specification* of a recipe step that projects
the selected predictors onto the space orthogonal to the dominant
directions of a clutter matrix, with
[`epo()`](https://christiangoueguel.com/specProc/reference/epo.md).

## Usage

``` r
step_epo(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  clutter = NULL,
  num_comp = 2,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("epo")
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

- clutter:

  A numeric matrix or data frame of clutter spectra. If it has column
  names, the columns matching the selected predictors are used;
  otherwise it must have one column per selected predictor, in the same
  order.

- num_comp:

  The number of orthogonal components to remove.

- res:

  The fitted filter, stored once the step has been trained.

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

The clutter describes variation that should not affect the outcome, for
example the differences between repeated measurements of the same
samples. It is external information, supplied when the step is
specified, and is not estimated from the training data. If
`clutter = NULL`, the dominant directions of the training data
themselves are removed, as in
[`epo()`](https://christiangoueguel.com/specProc/reference/epo.md). The
outcome is not used.

The selected columns are replaced by the corrected values (not
centered). `num_comp` can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html).
[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the selected `terms`, `num_comp` and `id`.

## See also

[`epo()`](https://christiangoueguel.com/specProc/reference/epo.md),
[`predict.specproc_filter()`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
# the three samples measured twice
twice <- forageLIBS$Sample[duplicated(forageLIBS$Sample)]
first <- match(twice, forageLIBS$Sample)
second <- vapply(twice, function(s) max(which(forageLIBS$Sample == s)), integer(1))
# their differences describe the variation between repeated measurements
clutter <- as.matrix(dat[second, -1]) - as.matrix(dat[first, -1])
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_epo(recipes::all_predictors(), clutter = clutter, num_comp = 1)
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 246
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1          996.         1019.          889.          855.          828.
#>  2          979.          944.          879.          853.          823.
#>  3         1010.         1094.          989.          898.          864.
#>  4          974.         1032.          960.          881.          852.
#>  5          945.         1010.          940.          946.          920.
#>  6         1093.         1122.          999.          942.          888.
#>  7         1032.         1103.          995.          929.          886.
#>  8         1054.         1035.          927.          908.          858.
#>  9          941.          982.          883.          879.          833.
#> 10         1020.         1080.          940.          935.          928.
#> # ℹ 58 more rows
#> # ℹ 241 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
