# Robust Yeo-Johnson Transformation Recipe Step

`step_robust_yeojohnson()` creates a *specification* of a recipe step
that transforms the selected variables toward central normality with the
robust Yeo-Johnson transformation of
[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md).
It is the robust counterpart of
[`recipes::step_YeoJohnson()`](https://recipes.tidymodels.org/reference/step_YeoJohnson.html).

## Usage

``` r
step_robust_yeojohnson(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  quantile = 0.99,
  nbsteps = 2,
  standardize = TRUE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("robust_yeojohnson")
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

- quantile:

  The quantile of the absolute value of a standard normal variable
  beyond which the standardized transformed values are given weight zero
  in the re-weighting steps, strictly between 0 and 1. Default is 0.99.

- nbsteps:

  The number of re-weighting steps, a positive integer. Default is 2.

- standardize:

  A logical value: standardize the transformed variables by the mean and
  standard deviation of the inliers (`TRUE`, default).

- res:

  The fitted transformation, stored once the step has been trained.

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

The transformation parameter \\\lambda\\ of each variable is estimated
on the training data (Raymaekers and Rousseeuw, 2021), as in
[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md),
and applied unchanged to new data. With `standardize = TRUE`, the
transformed variables are also centered and scaled with the mean and
standard deviation of the training inliers. Variables that cannot be
transformed (for example, constant ones) are left unchanged. Missing
values are ignored by the fit and stay missing.

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns a tibble with columns `terms`, `lambda` (`NA` for the variables
left unchanged) and `id`.

## See also

[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md),
[`step_robust_boxcox()`](https://christiangoueguel.com/specProc/reference/step_robust_boxcox.md)

## Author

Christian L. Goueguel

## Examples

``` r
library(recipes)
data(forageLIBS)
spectra <- snv(forageLIBS[-(1:14)])$correction
wl <- as.numeric(names(spectra))
mg <- spectra[which(wl > 279 & wl < 281)]  # the Mg II lines, after SNV
rec <- recipe(~ ., data = mg[1:300, ]) |>
  step_robust_yeojohnson(all_numeric_predictors())
prepped <- prep(rec)
tidy(prepped, number = 1)
#> # A tibble: 23 × 3
#>    terms       lambda id                     
#>    <chr>        <dbl> <chr>                  
#>  1 279.08125    0.652 robust_yeojohnson_eRX5G
#>  2 279.1652408  0.765 robust_yeojohnson_eRX5G
#>  3 279.2492316  0.656 robust_yeojohnson_eRX5G
#>  4 279.3332224  0.644 robust_yeojohnson_eRX5G
#>  5 279.4172132  1.24  robust_yeojohnson_eRX5G
#>  6 279.501204   0.739 robust_yeojohnson_eRX5G
#>  7 279.5851948  0.610 robust_yeojohnson_eRX5G
#>  8 279.6691856  0.584 robust_yeojohnson_eRX5G
#>  9 279.7680459  0.608 robust_yeojohnson_eRX5G
#> 10 279.8523433  0.640 robust_yeojohnson_eRX5G
#> # ℹ 13 more rows
```
