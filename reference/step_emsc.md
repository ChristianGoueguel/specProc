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

## Examples

``` r
data(forageLIBS)
# potassium and the K I resonance lines
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
  step_emsc(recipes::all_predictors(), degree = 2)
prepped <- recipes::prep(rec)
recipes::bake(prepped, new_data = dat[301:368, ])
#> # A tibble: 68 × 246
#>    `760.0161416` `760.1001689` `760.1841961` `760.2682233` `760.3522506`
#>            <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#>  1         1147.         1146.         1117.         1115.         1098.
#>  2         1094.         1024.         1068.         1076.         1055.
#>  3         1213.         1263.         1282.         1221.         1195.
#>  4         1064.         1109.         1150.         1093.         1075.
#>  5         1027.         1084.         1126.         1174.         1160.
#>  6         1183.         1182.         1188.         1168.         1127.
#>  7         1009.         1077.         1074.         1038.         1007.
#>  8         1136.         1088.         1089.         1106.         1065.
#>  9         1110.         1133.         1137.         1170.         1127.
#> 10         1128.         1168.         1136.         1169.         1175.
#> # ℹ 58 more rows
#> # ℹ 241 more variables: `760.4362778` <dbl>, `760.520305` <dbl>,
#> #   `760.6043323` <dbl>, `760.6883595` <dbl>, `760.7723867` <dbl>,
#> #   `760.856414` <dbl>, `760.9404412` <dbl>, `761.0244684` <dbl>,
#> #   `761.1084957` <dbl>, `761.1925229` <dbl>, `761.2765501` <dbl>,
#> #   `761.3605774` <dbl>, `761.4446046` <dbl>, `761.5286319` <dbl>,
#> #   `761.6126591` <dbl>, `761.6966863` <dbl>, `761.7807136` <dbl>, …
```
