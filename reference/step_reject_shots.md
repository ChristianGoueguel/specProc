# Shot Rejection Recipe Step

`step_reject_shots()` creates a *specification* of a recipe step that
removes the outlying laser shots of each sample from the training data,
with
[`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md).

## Usage

``` r
step_reject_shots(
  recipe,
  ...,
  sample,
  method = c("intensity", "correlation"),
  cutoff = 3.5,
  scale = c("floor", "sample", "pooled"),
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = TRUE,
  id = recipes::rand_id("reject_shots")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose the spectral columns (named
  by their wavelengths, see
  [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md)).

- sample:

  The column identifying the sample of each shot, as a bare name or a
  selector. Give it the `"id"` role (see
  [`recipes::update_role()`](https://recipes.tidymodels.org/reference/roles.html))
  so that it is not used as a predictor.

- method:

  The criteria: one or more of `"intensity"`, `"correlation"` (both by
  default) and `"distance"` (see
  [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md)).

- cutoff:

  The robust z-score above which a shot is rejected. Default is 3.5.

- scale:

  The robust scale of the z-scores: `"floor"` (default), `"sample"` or
  `"pooled"` (see Details).

- role:

  Not used by this step, since no new variables are created.

- trained:

  A logical indicating whether the step has been trained.

- columns:

  The names of the selected columns, stored once the step has been
  trained.

- skip:

  A logical: skip the step when new data are baked (`TRUE`, default).
  See details.

- id:

  A character string that is unique to this step.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

Each shot is compared with the other shots of the same sample (the
`sample` column), so nothing is estimated from the training data as a
whole. Because the step removes rows, `skip = TRUE` by default: shots
are rejected when the recipe is prepped, but new data are not filtered
when they are baked (predictions must be made for every row). Use a
resampling scheme grouped by sample, such as
[`rsample::group_vfold_cv()`](https://rsample.tidymodels.org/reference/group_vfold_cv.html),
so that shots of the same sample are not split between analysis and
assessment sets.

To model sample means instead of single shots, average the shots before
the recipe, with
[`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md)
and
[`average()`](https://christiangoueguel.com/specProc/reference/average.md).

[tidy()](https://recipes.tidymodels.org/reference/tidy.recipe.html)
returns the spectral `terms`, the `sample` column, the criteria
(`method`), the `cutoff`, the `scale` and `id`.

## See also

[`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md),
[`average()`](https://christiangoueguel.com/specProc/reference/average.md),
[`step_line_ratio()`](https://christiangoueguel.com/specProc/reference/step_line_ratio.md)

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 760 & wl < 780)]  # the K I resonance lines
set.seed(1)
shots <- spectra[rep(1:4, each = 5), ] * stats::runif(20, 0.95, 1.05)
shots[7, ] <- 0.2 * shots[7, ]
shots <- cbind(sample = rep(c("A", "B", "C", "D"), each = 5), shots)
rec <- recipes::recipe(~ ., data = shots) |>
  step_reject_shots(recipes::all_numeric(), sample = "sample")
prepped <- recipes::prep(rec)
# the rejected shot is removed from the training data
nrow(recipes::bake(prepped, new_data = NULL))
#> [1] 19
```
