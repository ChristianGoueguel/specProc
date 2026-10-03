# specProc

**specProc** processes and analyzes emission spectra, streamlining the
pipeline from raw spectra to calibrated concentrations. While
specifically developed for laser-induced breakdown spectroscopy (LIBS),
it seamlessly supports other plasma techniques such as ICP-OES. Select
functions can also be used to process Raman and infrared
spectroscopy data. The package structures data with one spectrum per row
and wavelengths as column names, leveraging a C++ backend to power heavy
computations.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("ChristianGoueguel/specProc", build_vignettes = TRUE)
```

A C++ compiler is needed to build the package from source (Rtools on
Windows, Xcode Command Line Tools on macOS).

## A LIBS workflow

The vignettes follow a typical LIBS analysis, on the `forageLIBS`
spectra of forage samples:

1.  **Fitting emission lines**
    ([`vignette("peak-fitting")`](https://christiangoueguel.com/specProc/articles/peak-fitting.md)):
    line identification, wavelength calibration, line profiles and
    overlapping lines.
2.  **Preprocessing with recipe steps**
    ([`vignette("preprocessing")`](https://christiangoueguel.com/specProc/articles/preprocessing.md)):
    baseline correction, normalization and other `step_*()` functions,
    in a tidymodels workflow.
3.  **Calibration curves and figures of merit**
    ([`vignette("calibration")`](https://christiangoueguel.com/specProc/articles/calibration.md)):
    univariate curves, limits of detection and quantification, and the
    net analyte signal of multivariate models.
4.  **Plasma diagnostics**
    ([`vignette("plasma-diagnostics")`](https://christiangoueguel.com/specProc/articles/plasma-diagnostics.md)):
    electron density, temperature, LTE, self-absorption and
    calibration-free LIBS.

They are also available as [articles on the package
website](https://christiangoueguel.com/specProc/articles/).

## Function overview

| Task | Functions |
|----|----|
| Line identification | [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md), [`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md), [`line_finder()`](https://christiangoueguel.com/specProc/reference/line_finder.md) (Shiny app), [`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md), [`apply_calibration()`](https://christiangoueguel.com/specProc/reference/apply_calibration.md) |
| Line fitting | [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md), [`multipeak_fit()`](https://christiangoueguel.com/specProc/reference/multipeak_fit.md), [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md), [`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md), [`voigt_profile()`](https://christiangoueguel.com/specProc/reference/voigt_profile.md), [`pseudo_voigt_profile()`](https://christiangoueguel.com/specProc/reference/pseudo_voigt_profile.md), [`gaussian_profile()`](https://christiangoueguel.com/specProc/reference/gaussian_profile.md), [`lorentzian_profile()`](https://christiangoueguel.com/specProc/reference/lorentzian_profile.md), [`voigt_fwhm()`](https://christiangoueguel.com/specProc/reference/voigt_fwhm.md) |
| Preprocessing | [`baseline_arpls()`](https://christiangoueguel.com/specProc/reference/baseline_arpls.md), [`baseline_als()`](https://christiangoueguel.com/specProc/reference/baseline_als.md), [`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md), [`snv()`](https://christiangoueguel.com/specProc/reference/snv.md), [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md), [`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md), [`normalize()`](https://christiangoueguel.com/specProc/reference/normalize.md), [`savitzky_golay()`](https://christiangoueguel.com/specProc/reference/savitzky_golay.md), [`wavelet_features()`](https://christiangoueguel.com/specProc/reference/wavelet_features.md), [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md), [`average()`](https://christiangoueguel.com/specProc/reference/average.md) |
| Recipe steps | [`step_baseline()`](https://christiangoueguel.com/specProc/reference/step_baseline.md), [`step_snv()`](https://christiangoueguel.com/specProc/reference/step_snv.md), [`step_msc()`](https://christiangoueguel.com/specProc/reference/step_msc.md), [`step_emsc()`](https://christiangoueguel.com/specProc/reference/step_emsc.md), [`step_spectral_norm()`](https://christiangoueguel.com/specProc/reference/step_spectral_norm.md), [`step_line_ratio()`](https://christiangoueguel.com/specProc/reference/step_line_ratio.md), [`step_savgol()`](https://christiangoueguel.com/specProc/reference/step_savgol.md), [`step_wavelet()`](https://christiangoueguel.com/specProc/reference/step_wavelet.md), [`step_line_intensities()`](https://christiangoueguel.com/specProc/reference/step_line_intensities.md), [`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md), [`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md), [`step_o2pls()`](https://christiangoueguel.com/specProc/reference/step_o2pls.md), [`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md), [`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md), and more |
| Orthogonalization | [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md), [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md), [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md), [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md), [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md), [`epo()`](https://christiangoueguel.com/specProc/reference/epo.md), [`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md), [`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md) |
| Calibration | [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md), [`plot_calibration()`](https://christiangoueguel.com/specProc/reference/plot_calibration.md), [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md), [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md) |
| Plasma diagnostics | [`saturation_summary()`](https://christiangoueguel.com/specProc/reference/saturation_summary.md), [`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md), [`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md), [`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md), [`plot_boltzmann()`](https://christiangoueguel.com/specProc/reference/plot_boltzmann.md), [`mcwhirter_criterion()`](https://christiangoueguel.com/specProc/reference/mcwhirter_criterion.md), [`self_absorption()`](https://christiangoueguel.com/specProc/reference/self_absorption.md), [`correct_self_absorption()`](https://christiangoueguel.com/specProc/reference/correct_self_absorption.md), [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md), [`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md), [`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md) |
| Outliers and robust PCA | [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md), [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md), [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md), [`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md), [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md), [`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md), [`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md), [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md), [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md), [`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md) |
| Robust statistics | [`summary_stats()`](https://christiangoueguel.com/specProc/reference/summary_stats.md), [`biweight_location()`](https://christiangoueguel.com/specProc/reference/biweight_location.md), [`biweight_scale()`](https://christiangoueguel.com/specProc/reference/biweight_scale.md), [`rousseeuw_croux()`](https://christiangoueguel.com/specProc/reference/rousseeuw_croux.md), [`umad()`](https://christiangoueguel.com/specProc/reference/umad.md), [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md), [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md) |
| Visualization | [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md), [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md), [`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md), [`plot_contributions()`](https://christiangoueguel.com/specProc/reference/plot_contributions.md) |

See the [reference](https://christiangoueguel.com/specProc/reference/)
for the full list.

## Comparing preprocessing pipelines

Which preprocessing and which model suit a LIBS calibration is an
empirical question. Since every specProc step is a recipe step,
candidate pipelines can be compared on an equal footing: each recipe is
re-estimated in every resample, together with the model. Here we predict
calcium from all 368 `forageLIBS` spectra. Calcium has strong lines in
these spectra (Ca II 393.37 and 396.85 nm, Ca I 422.67 nm), a laboratory
value for every sample, and a calibration error that reaches its minimum
within a few components, so the trade-offs are easy to see.

The pipelines combine the steps that matter for LIBS spectra:
normalization to the total emission
([`step_spectral_norm()`](https://christiangoueguel.com/specProc/reference/step_spectral_norm.md))
or to an internal standard
([`step_line_ratio()`](https://christiangoueguel.com/specProc/reference/step_line_ratio.md)),
here the C I 247.86 nm line, as carbon is the main element of the
organic matrix; compression by a wavelet transform
([`step_wavelet()`](https://christiangoueguel.com/specProc/reference/step_wavelet.md));
classical or robust principal components (`step_pca()`,
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md));
and orthogonal filtering
([`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md)).
The models are PLS, principal component regression (PCR), the elastic
net and a support vector regression (SVR).

``` r

suppressPackageStartupMessages(library(mixOmics)) # the PLS engine
library(specProc)
library(tidymodels)
library(plsmod)
library(patchwork)
tidymodels_prefer()

data("forageLIBS")
spectra_id <- c("Measurement", "Sample")
minerals <- names(forageLIBS)[3:14]

# steps that transform each spectrum on its own estimate nothing from the
# training data: apply them once, outside the resampling loop
per_spectrum <- \(data, step, ...) {
  recipe(~ ., data = data) |>
    update_role(all_of(c(spectra_id, minerals)), new_role = "id") |>
    step(all_predictors(), ...) |>
    prep() |>
    bake(new_data = NULL)
}
corrected <- per_spectrum(forageLIBS, step_baseline, lambda = 1e5,
                          options = list(max.iter = 20))
area <- per_spectrum(corrected, step_spectral_norm, method = "area")
carbon <- per_spectrum(corrected, step_line_ratio, reference = 247.856, window = 0.15)
compressed <- per_spectrum(carbon, step_wavelet, level = 3) # 8 times fewer variables

ca_recipe <- \(data) {
  recipe(Ca ~ ., data = data) |>
    update_role(all_of(spectra_id), all_of(setdiff(minerals, "Ca")), new_role = "id")
}
```

The baseline is removed in every pipeline. The steps estimated from the
training data (principal components, orthogonal filter) and the model
parameters are tuned together:

``` r

# mixOmics scales every predictor by default; scale = FALSE only centers
pls_model <- pls(num_comp = tune()) |>
  set_mode("regression") |>
  set_engine("mixOmics", scale = FALSE)
pcr_model <- linear_reg() |> set_engine("lm")
enet_model <- linear_reg(penalty = tune(), mixture = tune()) |> set_engine("glmnet")
svr_model <- svm_rbf(cost = tune(), rbf_sigma = tune()) |>
  set_mode("regression") |>
  set_engine("kernlab")
components <- tibble(num_comp = 1:20)

pipelines <- list(
  "PLS" = list(ca_recipe(corrected), pls_model, components),
  "Area + PLS" = list(ca_recipe(area), pls_model, components),
  "C I + PLS" = list(ca_recipe(carbon), pls_model, components),
  "C I + wavelet + PLS" = list(ca_recipe(compressed), pls_model, components),
  "C I + wavelet + PCR" = list(
    ca_recipe(compressed) |>
      step_center(all_predictors()) |>
      step_pca(all_predictors(), num_comp = tune()),
    pcr_model, components),
  "C I + wavelet + robust PCR" = list(
    ca_recipe(compressed) |>
      step_robpca(all_predictors(), num_comp = tune(), options = list(kmax = 20)),
    pcr_model, components),
  "C I + wavelet + OPLS + elastic net" = list(
    ca_recipe(compressed) |> step_opls(all_predictors(), num_comp = tune()),
    enet_model,
    expand_grid(num_comp = 1:4, penalty = 10^seq(-4, -1, by = 0.5),
                mixture = c(0.1, 0.5, 1))),
  "C I + wavelet + SVR" = list(
    ca_recipe(compressed), svr_model,
    expand_grid(cost = 2^seq(0, 8, by = 2), rbf_sigma = 10^seq(-4.5, -2.5, by = 0.5)))
)
```

Each pipeline is cross-validated in five repeats of a 5-fold split that
keeps the spectra of a sample in the same fold. The 25 resamples are
spread over parallel workers with the
[future](https://future.futureverse.org) package; tune gives each
resample the same random numbers in parallel as in sequence, so the
results do not depend on the number of workers:

``` r

# the same seed gives the same folds whatever the preprocessed data
cv_folds <- \(data) {
  set.seed(2026)
  group_vfold_cv(data, group = Sample, v = 5, repeats = 5)
}

future::plan("multisession", workers = parallelly::availableCores(omit = 1))
results <- map(pipelines, \(p) {
  rec <- p[[1]]
  tune_grid(workflow(rec, p[[2]]), resamples = cv_folds(rec$template), grid = p[[3]],
            metrics = metric_set(rmse, rsq), control = control_grid(save_pred = TRUE))
})
future::plan("sequential")
```

With repeated cross-validation, every sample is predicted once per
repeat, by models trained on different subsets. The cross-validated mean
squared error (MSE) then splits exactly into two terms: the squared
bias, from the distance between the mean prediction of a sample and its
laboratory value, and the variance, from the spread of its predictions
around that mean. Both terms are reported in the table below.

``` r

decompose <- \(res) {
  collect_predictions(res, summarize = FALSE) |>
    summarise(bias2 = (mean(.pred) - Ca[1])^2,
              variance = mean((.pred - mean(.pred))^2), .by = c(.config, .row)) |>
    summarise(across(c(bias2, variance), mean), .by = .config) |>
    mutate(mse = bias2 + variance)
}

metrics <- map(results, \(res) {
  collect_metrics(res) |>
    select(-.estimator, -n) |>
    pivot_wider(names_from = .metric, values_from = c(mean, std_err)) |>
    left_join(decompose(res), by = ".config")
}) |>
  bind_rows(.id = "pipeline") |>
  mutate(pipeline = factor(pipeline, levels = names(pipelines)))

best <- slice_min(metrics, mean_rmse, by = pipeline, with_ties = FALSE)
```

The cross-validated RMSE of the component-based pipelines as a function
of the number of components: on the left, the normalizations; on the
right, the models of the wavelet coefficients, with the best elastic net
and SVR models (on the same coefficients) as references:

``` r

groups <- list(
  "Normalization" = c("PLS", "Area + PLS", "C I + PLS"),
  "Wavelet compression" = c("C I + wavelet + PLS", "C I + wavelet + PCR",
                            "C I + wavelet + robust PCR")
)
others <- filter(best, !pipeline %in% unlist(groups))

panel <- \(title, members, references = NULL) {
  p <- metrics |>
    filter(pipeline %in% members) |>
    mutate(pipeline = factor(pipeline, levels = members)) |>
    ggplot(aes(num_comp, mean_rmse, colour = pipeline))
  if (!is.null(references)) {
    p <- p +
      geom_hline(aes(yintercept = mean_rmse), data = references,
                 colour = "grey50", linetype = "dashed") +
      annotate("text", x = 1, y = references$mean_rmse,
               label = sub("C I + wavelet + ", "", references$pipeline, fixed = TRUE),
               hjust = 0, vjust = 1.4, size = 3, colour = "grey30")
  }
  p +
    geom_line(linewidth = 0.8) +
    geom_point(data = filter(best, pipeline %in% members), size = 2.5) +
    scale_colour_manual(values = c("#2a78d6", "#eb6834", "#1baf7a")) +
    coord_cartesian(ylim = c(0.088, 0.12)) +
    labs(x = "Components", y = "Cross-validated RMSE (% Ca)", colour = NULL,
         subtitle = title) +
    guides(colour = guide_legend(ncol = 1)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.key.spacing.y = unit(0, "pt"),
          panel.grid.minor = element_blank())
}

panel(names(groups)[1], groups[[1]]) +
  panel(names(groups)[2], groups[[2]], references = others) +
  plot_layout(axes = "collect") +
  plot_annotation(
    title = "Cross-validated error of the Ca models",
    subtitle = "Dots: lowest RMSE; dashed lines: best elastic net and SVR models",
    theme = theme(plot.title = element_text(face = "bold"))
  )
```

![](reference/figures/README-compare-rmse-1.png)

The best model of each pipeline:

``` r

summary_table <- best |>
  mutate(Tuning = case_when(
    !is.na(cost) ~ sprintf("C = %g, σ = %.1e", cost, rbf_sigma),
    !is.na(penalty) ~ sprintf("%d orthogonal, λ = %.1e, α = %g", num_comp, penalty, mixture),
    .default = sprintf("%d components", num_comp)
  )) |>
  transmute(Pipeline = pipeline, Tuning, `RMSECV (% Ca)` = mean_rmse,
            SE = std_err_rmse, `R²` = mean_rsq, `Bias²` = bias2, Variance = variance) |>
  arrange(`RMSECV (% Ca)`)

knitr::kable(summary_table, digits = c(0, 0, 4, 4, 3, 5, 5))
```

| Pipeline | Tuning | RMSECV (% Ca) | SE | R² | Bias² | Variance |
|:---|:---|---:|---:|---:|---:|---:|
| C I + PLS | 8 components | 0.0899 | 0.0017 | 0.762 | 0.00800 | 0.00015 |
| C I + wavelet + PLS | 14 components | 0.0900 | 0.0016 | 0.762 | 0.00776 | 0.00040 |
| C I + wavelet + OPLS + elastic net | 3 orthogonal, λ = 1.0e-02, α = 1 | 0.0903 | 0.0016 | 0.759 | 0.00809 | 0.00013 |
| C I + wavelet + PCR | 13 components | 0.0908 | 0.0016 | 0.754 | 0.00822 | 0.00009 |
| C I + wavelet + robust PCR | 16 components | 0.0915 | 0.0015 | 0.750 | 0.00832 | 0.00011 |
| PLS | 8 components | 0.0918 | 0.0018 | 0.751 | 0.00837 | 0.00012 |
| C I + wavelet + SVR | C = 4, σ = 1.0e-04 | 0.0935 | 0.0017 | 0.740 | 0.00865 | 0.00017 |
| Area + PLS | 7 components | 0.0935 | 0.0016 | 0.740 | 0.00871 | 0.00010 |

- **The internal standard helps; the total emission does not.** Dividing
  by the C I line lowers the RMSE of PLS from 0.0918 to 0.0899% Ca,
  while normalizing to the total area raises it to 0.0935%: the total
  emission depends on the composition of the forage, not only on the
  ablated mass.
- **The wavelet compression loses nothing.** With 8 times fewer
  variables, PLS reaches the same error, and its error curve stays flat
  beyond the optimum: the choice of the number of components matters
  less. The compression also makes PCA, robust PCA and SVR much faster.
- **The models differ little.** All pipelines fall within 2.2 standard
  errors of each other. PCR needs more components than PLS, since its
  components ignore calcium, but has the lowest variance. Robust PCA
  brings no gain on these spectra; it pays off when the training set
  holds outlying spectra. The OPLS filter followed by a lasso (α = 1)
  matches PLS. The SVR adds nothing either, a sign that the response of
  the Ca lines is close to linear over this range.
- **The error is mostly bias.** At the best tuning, the variance makes
  up only 1–4.9% of the MSE. The squared bias includes the error of the
  laboratory reference values and the matrix effects the spectra do not
  resolve, which no model removes. The variance sets the number of
  components: beyond the dots in the figure, the error rises again, as
  the bias no longer falls while the variance keeps growing.

## License

MIT © Christian L. Goueguel
