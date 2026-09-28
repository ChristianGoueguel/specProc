# Predicting soil clay content from LIBS spectra

Preprocessing is only useful if it improves the analysis it serves. This
vignette uses a concrete goal to evaluate preprocessing choices:
predicting the clay content of the 50 soil samples in `specLIBS` with
partial least squares (PLS) regression, fitted and validated with the
[tidymodels](https://www.tidymodels.org) framework.

Two statistical issues shape the analysis:

- **Clay content is part of a composition.** Clay, sand and silt sum to
  100%, so they should be modeled on a log-ratio scale, not as three
  unrelated percentages.
- **There are 50 samples and 7152 variables.** A model can fit the
  calibration data almost perfectly, so everything depends on estimating
  the prediction error honestly. Common shortcuts bias it downward; the
  vignette quantifies two of them.

``` r

# parsnip attaches the mixOmics engine, which attaches MASS. Attaching it
# first keeps dplyr::select() (attached with recipes) ahead of MASS::select()
suppressPackageStartupMessages(library(mixOmics))
library(specProc)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(rsample)
library(plsmod)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

data(specLIBS)
meta_cols <- c("Sample", "Location", "Clay", "Sand", "Silt", "Texture", "Structure", "Type")
channels <- setdiff(names(specLIBS), meta_cols)
```

## The target: a composition

``` r

texture <- specLIBS |> distinct(Sample, Clay, Sand, Silt)
texture |> select(-Sample) |> summary()
#>       Clay            Sand            Silt      
#>  Min.   : 1.10   Min.   : 5.00   Min.   : 4.00  
#>  1st Qu.:23.57   1st Qu.:19.05   1st Qu.:30.45  
#>  Median :34.55   Median :25.00   Median :39.90  
#>  Mean   :31.83   Mean   :33.61   Mean   :34.56  
#>  3rd Qu.:38.45   3rd Qu.:30.70   3rd Qu.:43.33  
#>  Max.   :81.00   Max.   :92.90   Max.   :65.70
summary(rowSums(texture[c("Clay", "Sand", "Silt")]))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     100     100     100     100     100     100
```

The three fractions sum to 100% (up to rounding), so they carry only two
independent pieces of information. Modeling clay percentage directly
with a linear model ignores this constraint, which has two consequences:

- **Predictions can leave the range 0–100%.** A linear model can predict
  negative clay contents.
- **Errors are treated as equal on an absolute scale.** An error of 5
  points counts the same at 5% clay as at 50% clay, although it is a
  100% error in the first case and a 10% error in the second.

Compositional data analysis (Aitchison, 1986) removes the constraint by
working with log-ratios. We use the **additive log-ratio** (ALR)
transformation with silt as the reference fraction:

z_1 = \log\frac{\text{clay}}{\text{silt}}, \qquad z_2 =
\log\frac{\text{sand}}{\text{silt}}

The two log-ratios are unconstrained real numbers, so they suit linear
models such as PLS. We fit one PLS model for each. Their predictions
\hat z_1, \hat z_2 are transformed back to a composition:

\widehat{\text{clay}} = 100 \\ \frac{e^{\hat z_1}}{1 + e^{\hat z_1} +
e^{\hat z_2}}, \qquad \widehat{\text{sand}} = 100 \\ \frac{e^{\hat
z_2}}{1 + e^{\hat z_1} + e^{\hat z_2}}, \qquad \widehat{\text{silt}} =
100 - \widehat{\text{clay}} - \widehat{\text{sand}}

By construction, the predicted fractions are positive and sum to 100%.
None of the fractions is zero in this data set, so the log-ratios are
defined for every sample. Zeros would need a replacement strategy first.

``` r

targets <- texture |>
  mutate(
    total = Clay + Sand + Silt,        # close to exactly 1
    clay = 100 * Clay / total,
    clay_silt = log(Clay / Silt),
    sand_silt = log(Sand / Silt)
  ) |>
  select(Sample, clay, clay_silt, sand_silt)

back_transform <- function(z1, z2) 100 * exp(z1) / (1 + exp(z1) + exp(z2))

targets |>
  pivot_longer(c(clay, clay_silt), names_to = "scale") |>
  mutate(scale = recode(scale, clay = "Clay (%)", clay_silt = "log(clay / silt)")) |>
  ggplot(aes(value)) +
  geom_histogram(bins = 15, fill = "grey70", colour = "white") +
  facet_wrap(~ scale, scales = "free_x") +
  labs(x = NULL, y = "Samples") +
  theme_bw()
```

![](calibration_files/figure-html/logratio-1.png)

On the percentage scale, clay content is left-skewed, with a few samples
above 55% and several below 10%. On the log-ratio scale, the low-clay
samples are spread out rather than squeezed against zero.

## Preprocessing without leakage

Preprocessing steps fall into two groups:

- **Steps computed from each spectrum alone**
  ([`step_baseline()`](https://christiangoueguel.com/specProc/reference/step_baseline.md),
  [`step_snv()`](https://christiangoueguel.com/specProc/reference/step_snv.md)):
  they use no information from other samples, so they give the same
  result whether they are applied before or after the data are split.
- **Steps estimated from a set of samples**
  ([`step_msc()`](https://christiangoueguel.com/specProc/reference/step_msc.md),
  which estimates a reference spectrum; the EPO, GLSW and OSC-family
  filters; centering and scaling): they must be estimated on the
  calibration part of each cross-validation split only, and then applied
  to the held-out part. Placed in a recipe that is part of a workflow,
  they are re-estimated on every resample automatically.

We apply the per-spectrum steps to the 400 shots now, and average the 8
shots of each sample:

``` r

shot_recipe <- recipe(~ ., data = specLIBS) |>
  update_role(all_of(meta_cols), new_role = "id") |>
  step_baseline(all_predictors(), lambda = 1e5, options = list(max.iter = 20))

baselined_shots <- shot_recipe |> prep() |> bake(new_data = NULL)
snv_shots <- shot_recipe |> step_snv(all_predictors()) |> prep() |> bake(new_data = NULL)

sample_means <- function(shots) {
  shots |>
    select(Sample, all_of(channels)) |>
    average(Sample) |>
    inner_join(targets, by = "Sample") |>
    relocate(Sample, clay, clay_silt, sand_silt)
}
raw_s <- sample_means(specLIBS)
base_s <- sample_means(baselined_shots)
snv_s <- sample_means(snv_shots)
```

## Validation design

We use **repeated 5-fold cross-validation** with the sample as the unit.
Each sample is one row after averaging, so all 8 shots of a sample are
always on the same side of a split. Within each calibration set, a PLS
model is fitted to each log-ratio, and the two predicted log-ratios are
back-transformed to clay percentages.

The number of PLS components (up to 10, the same for both log-ratios) is
chosen by an inner cross-validation within the calibration set, so the
outer folds never influence this choice: this is nested
cross-validation, which
[`rsample::nested_cv()`](https://rsample.tidymodels.org/reference/nested_cv.html)
sets up. For each inner split,
[`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
fits the two log-ratio workflows for 1 to 10 components; their saved
predictions are joined and back-transformed, and the number of
components minimizing the error of the **back-transformed clay
predictions** is kept. This is the same criterion used to evaluate the
model. Choosing it instead on the log-ratio scale would optimize
relative errors, which favors fewer components and gives worse clay
percentages. With only 40 calibration samples per split, the chosen
number of components still varies considerably from split to split.

The whole procedure is repeated 3 times with different fold assignments,
to show how much the error estimate itself varies. (More repeats give a
more precise comparison, at a proportional cost in computing time.)

``` r

set.seed(2024)
folds <- nested_cv(targets,
                   outside = vfold_cv(v = 5, repeats = 3),
                   inside = vfold_cv(v = 5))
folds
#> # Nested resampling:
#> #  outer: 5-fold cross-validation repeated 3 times
#> #  inner: 5-fold cross-validation
#> # A tibble: 15 × 4
#>    splits          id      id2   inner_resamples
#>    <list>          <chr>   <chr> <list>         
#>  1 <split [40/10]> Repeat1 Fold1 <vfold [5 × 2]>
#>  2 <split [40/10]> Repeat1 Fold2 <vfold [5 × 2]>
#>  3 <split [40/10]> Repeat1 Fold3 <vfold [5 × 2]>
#>  4 <split [40/10]> Repeat1 Fold4 <vfold [5 × 2]>
#>  5 <split [40/10]> Repeat1 Fold5 <vfold [5 × 2]>
#>  6 <split [40/10]> Repeat2 Fold1 <vfold [5 × 2]>
#>  7 <split [40/10]> Repeat2 Fold2 <vfold [5 × 2]>
#>  8 <split [40/10]> Repeat2 Fold3 <vfold [5 × 2]>
#>  9 <split [40/10]> Repeat2 Fold4 <vfold [5 × 2]>
#> 10 <split [40/10]> Repeat2 Fold5 <vfold [5 × 2]>
#> 11 <split [40/10]> Repeat3 Fold1 <vfold [5 × 2]>
#> 12 <split [40/10]> Repeat3 Fold2 <vfold [5 × 2]>
#> 13 <split [40/10]> Repeat3 Fold3 <vfold [5 × 2]>
#> 14 <split [40/10]> Repeat3 Fold4 <vfold [5 × 2]>
#> 15 <split [40/10]> Repeat3 Fold5 <vfold [5 × 2]>
```

The splits index rows, so the same splits apply to every version of the
data (raw, baseline-corrected, SNV), which have their samples in the
same order.

``` r

# PLS regression; mixOmics scales every channel by default, scale = FALSE
# only centers them
pls_model <- pls(num_comp = tune()) |>
  set_mode("regression") |>
  set_engine("mixOmics", scale = FALSE)

# A recipe for one log-ratio: the channels are predictors, the other
# targets are identifiers
logratio_recipe <- function(data, outcome) {
  recipe(data) |>
    update_role(all_of(channels), new_role = "predictor") |>
    update_role(all_of(outcome), new_role = "outcome") |>
    update_role(Sample, clay, any_of(setdiff(c("clay_silt", "sand_silt"), outcome)),
                new_role = "id")
}

# The same split, on another table with the samples in the same order
reindex <- function(split, data) {
  make_splits(list(analysis = split$in_id, assessment = complement(split)), data = data)
}

# Number of components minimizing the inner-CV error of the back-transformed
# clay predictions, from the two log-ratio workflows and their inner resamples
select_ncomp <- function(workflows, inner_sets, cal) {
  predictions <- map2(workflows, inner_sets, \(wf, inner) {
    tune_grid(wf, resamples = inner, grid = tibble(num_comp = 1:10),
              control = control_grid(save_pred = TRUE)) |>
      collect_predictions() |>
      select(.row, num_comp, .pred)
  })
  inner_join(predictions$clay_silt, predictions$sand_silt,
             by = c(".row", "num_comp"), suffix = c("_1", "_2")) |>
    mutate(error = back_transform(.pred_1, .pred_2) - cal$clay[.row]) |>
    group_by(num_comp) |>
    summarise(rmse = sqrt(mean(error^2))) |>
    slice_min(rmse, n = 1, with_ties = FALSE) |>
    pull(num_comp)
}

# One outer split: tune on the calibration part, predict the held-out part.
# `pipeline` holds the data (or one table per log-ratio), optional recipe
# steps and, optionally, a fixed number of components.
evaluate_split <- function(pipeline, split, inner) {
  outcomes <- c(clay_silt = "clay_silt", sand_silt = "sand_silt")
  data_for <- function(outcome) {
    if (is.data.frame(pipeline$data)) pipeline$data else pipeline$data[[outcome]]
  }
  splits <- map(outcomes, \(o) reindex(split, data_for(o)))
  cal <- analysis(splits$clay_silt)
  workflows <- map(outcomes, \(o) {
    rec <- logratio_recipe(analysis(splits[[o]]), o)
    if (!is.null(pipeline$steps)) rec <- pipeline$steps(rec, cal$Sample)
    workflow(rec, pls_model)
  })
  ncomp <- pipeline$ncomp
  if (is.null(ncomp)) {
    inner_sets <- map(outcomes, \(o) manual_rset(
      map(inner$splits, \(s) reindex(s, analysis(splits[[o]]))), inner$id))
    ncomp <- select_ncomp(workflows, inner_sets, cal)
  }
  predicted <- map2(workflows, outcomes, \(wf, o) {
    finalize_workflow(wf, tibble(num_comp = ncomp)) |>
      fit(data = analysis(splits[[o]])) |>
      predict(new_data = assessment(splits[[o]])) |>
      pull(.pred)
  })
  tibble(Sample = assessment(splits$clay_silt)$Sample,
         clay = assessment(splits$clay_silt)$clay,
         pred = back_transform(predicted$clay_silt, predicted$sand_silt),
         ncomp = ncomp)
}

# Repeated nested CV of a pipeline
cross_validate <- function(pipeline, folds) {
  folds |>
    mutate(result = map2(splits, inner_resamples, \(s, i) evaluate_split(pipeline, s, i))) |>
    select(-splits, -inner_resamples) |>
    rename(repeat_id = id) |>
    unnest(result)
}
```

## Comparing preprocessing pipelines

We compare six pipelines. They share the fold assignments, so the
comparisons are paired:

``` r

# Clutter for EPO: shot-to-shot deviations of the calibration samples only
shot_clutter <- function(cal_samples) {
  shots <- snv_shots |> filter(Sample %in% cal_samples)
  x <- as.matrix(shots[channels])
  x - (rowsum(x, shots$Sample) / 8)[shots$Sample, ]
}

pipelines <- list(
  `raw counts` = list(data = raw_s),
  `baseline` = list(data = base_s),
  `baseline + SNV` = list(data = snv_s),
  # MSC: the reference spectrum is estimated on the calibration samples only
  `baseline + MSC` = list(data = base_s, steps = \(rec, cal) step_msc(rec, all_predictors())),
  # EPO: clutter from the calibration samples only
  `SNV + EPO` = list(data = snv_s, steps = \(rec, cal) {
    step_epo(rec, all_predictors(), num_comp = 3, clutter = shot_clutter(cal))
  }),
  # OPLS filter (projected OSC) fitted to the calibration samples and to the
  # log-ratio being modeled, followed by a one-component PLS model
  `SNV + OPLS filter` = list(data = snv_s, ncomp = 1, steps = \(rec, cal) {
    step_projected_osc(rec, all_predictors(), num_comp = 3)
  })
)

results <- pipelines |>
  map(\(p) suppressMessages(cross_validate(p, folds))) |>
  bind_rows(.id = "pipeline") |>
  mutate(pipeline = factor(pipeline, levels = names(pipelines)))
```

``` r

per_repeat <- results |>
  group_by(pipeline, repeat_id) |>
  summarise(
    rmse = sqrt(mean((pred - clay)^2)),
    r2 = 1 - sum((pred - clay)^2) / sum((clay - mean(clay))^2),
    ncomp = median(ncomp),
    .groups = "drop"
  )

summary_table <- per_repeat |>
  group_by(pipeline) |>
  summarise(RMSE = mean(rmse), RMSE_sd = sd(rmse), R2 = mean(r2), ncomp = median(ncomp)) |>
  mutate(across(c(RMSE, RMSE_sd, R2), \(v) round(v, 2)))
summary_table
#> # A tibble: 6 × 5
#>   pipeline           RMSE RMSE_sd    R2 ncomp
#>   <fct>             <dbl>   <dbl> <dbl> <dbl>
#> 1 raw counts        10.2     1.24  0.58     3
#> 2 baseline          10.6     1.01  0.56     3
#> 3 baseline + SNV     8.52    0.58  0.71     3
#> 4 baseline + MSC     8.02    0.7   0.75     5
#> 5 SNV + EPO          8.27    0.34  0.73     5
#> 6 SNV + OPLS filter  8.18    0.4   0.74     1

null_rmse <- sqrt(mean((targets$clay - mean(targets$clay))^2))
c(null_model_RMSE = round(null_rmse, 2))
#> null_model_RMSE 
#>           15.96
```

`RMSE` (in % clay) and `R2` are averages over the 3 repeats, `RMSE_sd`
is the standard deviation of the RMSE across repeats, and `ncomp` is the
median number of PLS components chosen. All pipelines predict clay
content far better than the null model, which predicts the mean for
every sample (RMSE 16%). The spectra explain most of the variation in
clay content.

Are the pipelines different from each other? The fold assignments are
shared, so we compare each pipeline with the baseline-only pipeline
repeat by repeat:

``` r

paired <- per_repeat |>
  select(pipeline, repeat_id, rmse) |>
  group_by(repeat_id) |>
  mutate(difference = rmse - rmse[pipeline == "baseline"]) |>
  group_by(pipeline) |>
  summarise(mean_difference = mean(difference), min = min(difference), max = max(difference)) |>
  mutate(across(-pipeline, \(v) round(v, 2)))
paired
#> # A tibble: 6 × 4
#>   pipeline          mean_difference   min   max
#>   <fct>                       <dbl> <dbl> <dbl>
#> 1 raw counts                  -0.36 -0.86  0.08
#> 2 baseline                     0     0     0   
#> 3 baseline + SNV              -2.08 -3.88 -1.07
#> 4 baseline + MSC              -2.58 -3.7  -1.54
#> 5 SNV + EPO                   -2.33 -3.25 -1.36
#> 6 SNV + OPLS filter           -2.42 -3.75 -1.17
```

The comparison separates two questions:

- **Normalizing helps.** The four pipelines that normalize the spectra
  (SNV, MSC, SNV + EPO and SNV + OPLS filter) have a lower RMSE than
  baseline correction alone in every repeat, by about 2.4 points of clay
  on average. This answers the question left open in the preprocessing
  vignette: the between-sample variation that normalization removes is
  mostly unrelated to clay content, so removing it helps.
- **The normalized pipelines cannot be ranked.** Their mean RMSEs lie
  within 0.5 points of each other, which is small compared with the
  variation between repeats. The additional filters (EPO, OPLS) do not
  measurably improve on SNV or MSC alone.

A claim that one normalized pipeline is better than another would need
more samples, or an independent validation set.

## What the log-ratio model gains

For comparison, we fit PLS directly to the clay percentage, with the
same preprocessing (baseline + SNV) and the same fold assignments. The
number of components is chosen by the inner cross-validation with
[`tune::select_best()`](https://tune.tidymodels.org/reference/show_best.html):

``` r

direct_workflow <- workflow(
  recipe(snv_s) |>
    update_role(all_of(channels), new_role = "predictor") |>
    update_role(clay, new_role = "outcome") |>
    update_role(Sample, clay_silt, sand_silt, new_role = "id"),
  pls_model
)

direct <- folds |>
  mutate(result = map2(splits, inner_resamples, \(split, inner) {
    split <- reindex(split, snv_s)
    inner <- manual_rset(map(inner$splits, \(s) reindex(s, analysis(split))), inner$id)
    best <- tune_grid(direct_workflow, resamples = inner, grid = tibble(num_comp = 1:10)) |>
      select_best(metric = "rmse")
    finalize_workflow(direct_workflow, best) |>
      fit(data = analysis(split)) |>
      augment(new_data = assessment(split)) |>
      select(Sample, clay, pred = .pred)
  })) |>
  select(repeat_id = id, result) |>
  unnest(result)

logratio <- results |> filter(pipeline == "baseline + SNV")
compare <- function(predictions) {
  per_sample <- predictions |> group_by(Sample, clay) |> summarise(pred = mean(pred), .groups = "drop")
  tibble(
    RMSE = predictions |> group_by(repeat_id) |> summarise(r = sqrt(mean((pred - clay)^2))) |>
      pull(r) |> mean(),
    median_abs_error = median(abs(per_sample$pred - per_sample$clay)),
    min_prediction = min(predictions$pred),
    RMSE_log_clay = sqrt(mean((log(pmax(per_sample$pred, 0.1)) - log(per_sample$clay))^2))
  )
}
bind_rows(direct = compare(direct), log_ratio = compare(logratio), .id = "model") |>
  mutate(across(-model, \(v) round(v, 2)))
#> # A tibble: 2 × 5
#>   model      RMSE median_abs_error min_prediction RMSE_log_clay
#>   <chr>     <dbl>            <dbl>          <dbl>         <dbl>
#> 1 direct     8.41             4.08          -5.66          0.73
#> 2 log_ratio  8.52             3.86           1.74          0.41
```

The two models have a comparable RMSE (8.41 and 8.52 points). The
log-ratio model is better on the other criteria:

- **Its predictions are always valid percentages.** The direct model
  predicts clay contents as low as -5.7%, while the log-ratio
  predictions are positive by construction.
- **Its typical error is smaller,** as the median absolute error shows.
  The RMSE is dominated by the few large errors at high clay content.
- **Its relative errors are much smaller,** as the RMSE of log(clay)
  shows. The gain comes from the low-clay samples, where the direct
  model’s errors are large relative to the true values.

The model also predicts the complete composition: the sand and silt
predictions come with the same fit and are consistent with the clay
prediction by construction.

Two caveats apply to back-transformed predictions:

- **Back-transformed predictions are not unbiased means.** The model is
  fitted on the log-ratio scale, so the back-transformed prediction
  estimates a typical value on that scale (closer to a median than to a
  mean) of the clay fraction. When residuals are large, the mean clay
  content is slightly different.
- **The choice of reference fraction matters a little.** With separate
  PLS models for each log-ratio, choosing silt as the denominator gives
  slightly different predictions than choosing sand. Isometric
  log-ratios (ILR) avoid this arbitrary choice, but are less directly
  interpretable.

## How much optimism do common shortcuts add?

### Shortcut 1: splitting replicates across folds

If the 400 shots are cross-validated as if they were independent, shots
of the same sample end up in both the calibration and the validation
folds. The model is then partly validated on the samples it was trained
on. We run the same nested procedure on the shots, once:

``` r

shot_data <- snv_shots |>
  select(Sample, all_of(channels)) |>
  inner_join(targets, by = "Sample") |>
  relocate(Sample, clay, clay_silt, sand_silt)

set.seed(2024)
shot_folds <- nested_cv(shot_data, outside = vfold_cv(v = 5), inside = vfold_cv(v = 5))
shot_level <- suppressMessages(cross_validate(list(data = shot_data), shot_folds))
shot_rmse <- sqrt(mean((shot_level$pred - shot_level$clay)^2))
sample_rmse <- summary_table$RMSE[summary_table$pipeline == "baseline + SNV"]
c(shot_level_CV = round(shot_rmse, 2), sample_level_CV = sample_rmse)
#>   shot_level_CV sample_level_CV 
#>            7.78            8.52
```

Shot-level cross-validation reports an RMSE about 9% lower than the
sample-level estimate. The optimism grows as replicates become more
alike, and it can be large for homogeneous materials.

### Shortcut 2: fitting a supervised filter before cross-validating

The OPLS filter uses the response to decide what to remove. If it is
fitted once on all 50 samples and cross-validation is run afterwards,
the validation samples have already influenced the filter. We filter
each log-ratio’s data once, then cross-validate a one-component model on
the filtered data:

``` r

prefiltered <- map(c(clay_silt = "clay_silt", sand_silt = "sand_silt"), \(o) {
  filtered <- projected_osc(snv_s[channels], snv_s[[o]], ncomp = 4)$correction
  bind_cols(select(snv_s, Sample, clay, clay_silt, sand_silt), filtered)
})
leaky <- suppressMessages(cross_validate(list(data = prefiltered, ncomp = 1), folds))
leaky_rmse <- leaky |> group_by(repeat_id) |> summarise(r = sqrt(mean((pred - clay)^2))) |> pull(r)
honest_rmse <- summary_table$RMSE[summary_table$pipeline == "SNV + OPLS filter"]
c(filter_before_CV = round(mean(leaky_rmse), 2), filter_inside_CV = honest_rmse)
#> filter_before_CV filter_inside_CV 
#>             4.97             8.18
```

Fitting the filter before cross-validation gives an RMSE 39% lower than
the honest estimate, a far larger effect than any difference between the
pipelines above. The same bias affects
[`step_osc()`](https://christiangoueguel.com/specProc/reference/step_osc.md),
[`step_direct_osc()`](https://christiangoueguel.com/specProc/reference/step_direct_osc.md),
[`step_direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/step_direct_orthogonal.md)
and the underlying functions, and any variable selection that uses the
response. Placing the filter in the workflow’s recipe, as in the
comparison above, refits it inside every fold, including the inner loop
that chooses the number of components.

## Looking at the predictions

The out-of-fold predictions of the log-ratio model, averaged over the 3
repeats, show where the model succeeds and where it fails:

``` r

mean_predictions <- logratio |>
  group_by(Sample, clay) |>
  summarise(log_ratio = mean(pred), .groups = "drop") |>
  left_join(direct |> group_by(Sample) |> summarise(direct = mean(pred)), by = "Sample")

ggplot(mean_predictions, aes(clay, log_ratio)) +
  geom_abline(linetype = "dashed") +
  geom_point(shape = 21, fill = "grey70", size = 2) +
  coord_equal(xlim = c(0, 85), ylim = c(0, 85)) +
  labs(x = "Measured clay (%)", y = "Cross-validated prediction (%)") +
  theme_bw()
```

![](calibration_files/figure-html/predictions-1.png)

``` r

high <- mean_predictions |> filter(clay > 55) |> mutate(across(-Sample, \(v) round(v, 1)))
high
#> # A tibble: 4 × 4
#>   Sample        clay log_ratio direct
#>   <chr>        <dbl>     <dbl>  <dbl>
#> 1 LSG-S18-0003  81        62.2   60.3
#> 2 LSG-S18-0004  69.1      71.8   70.6
#> 3 LSG-S18-0018  67.1      52.2   47.5
#> 4 MRI011        59.1      43.6   45.8
```

The 4 samples above 55% clay remain the weak point: 2 of them are
underpredicted by more than 15 points with the log-ratio model, and 2
with the direct model. With so few calibration samples in that range,
predictions are pulled toward the bulk of the data. The model should not
be used to predict clay contents above about 55% without more
calibration data there.

## Further considerations

- **Cross-validation estimates error for similar samples.** It says
  nothing about soils from other regions, other instruments or other
  measurement sessions. An independent test set measured later is the
  stronger evidence, and
  [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md) or
  [`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md)
  can help when a model must be transferred between instruments.
- **Report the variability of the error estimate.** A single RMSE from a
  single cross-validation split hides the spread shown in the table
  above.
- **A joint model of both log-ratios** (PLS2, or a multivariate method)
  can exploit the correlation between them. With two separate PLS1
  models, as here, each log-ratio has its own coefficients but shares
  the number of components.

## Reference

Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
Chapman and Hall, London.
