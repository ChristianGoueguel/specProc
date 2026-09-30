# specProc

**specProc** preprocesses and explores spectroscopic data. It was
developed for laser-induced breakdown spectroscopy (LIBS) but works with
other techniques such as Raman, infrared, and inductively coupled plasma
optical emission spectroscopy. It takes raw detector counts through to a
data matrix ready for modeling. Its computationally heavy steps run in
C++ (via Rcpp), so it stays fast on data sets with thousands of spectra
and high-resolution wavelength channels.

## Installation

Install the development version from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("ChristianGoueguel/specProc", build_vignettes = TRUE)
```

A C++ compiler is needed to build the package from source (Rtools on
Windows, Xcode Command Line Tools on macOS).
[`opls()`](https://christiangoueguel.com/specProc/reference/opls.md)
additionally needs the Bioconductor package ropls:
`BiocManager::install("ropls")`.

## Design principles

- **Tidy in, tidy out.** Spectra are stored one per row, and the column
  names are the wavelengths. Functions accept matrices, data frames or
  tibbles and return tibbles.
- **Built for tidymodels.** Preprocessing, filtering and robust PCA are
  available as recipe steps, so they are estimated on training data only
  and can be tuned with the model.
- **Preprocessing parameters are returned, not hidden.** Centers,
  scales, reference spectra, filter matrices and loadings come back with
  the result. You can then estimate a transformation on calibration data
  and apply exactly the same transformation to new data.
- **Robust alternatives are available alongside the classical
  estimators.** Emission spectra contain outlying shots, saturated
  channels and heavy tails, and the robust estimators are designed for
  them.

## Learn more

The vignettes develop the example above in more depth:

- [`vignette("preprocessing", package = "specProc")`](https://christiangoueguel.com/specProc/articles/preprocessing.md):
  choosing and checking each preprocessing step against replicate data.
- [`vignette("line-fitting", package = "specProc")`](https://christiangoueguel.com/specProc/articles/line-fitting.md):
  choosing a line profile, and the sources of uncertainty in fitted line
  areas.
- [`vignette("calibration", package = "specProc")`](https://christiangoueguel.com/specProc/articles/calibration.md):
  predicting soil clay content with a compositional (log-ratio) PLS
  model, and estimating prediction error without leakage.
- [`vignette("orthogonalization", package = "specProc")`](https://christiangoueguel.com/specProc/articles/orthogonalization.md):
  what each orthogonalization method removes from LIBS spectra of forage
  samples, whether it improves potassium predictions, the net analyte
  signal and figures of merit, and the same analysis as a tidymodels
  workflow.
- [`vignette("plasma-diagnostics", package = "specProc")`](https://christiangoueguel.com/specProc/articles/plasma-diagnostics.md):
  detector saturation, electron density from H-alpha and Stark
  broadening, Boltzmann and Saha-Boltzmann temperatures with NIST atomic
  data, and self-absorption.

They are also available as [articles on the package
website](https://christiangoueguel.com/specProc/articles/).

## Function overview

| Task | Functions |
|----|----|
| Baseline correction | [`baseline_arpls()`](https://christiangoueguel.com/specProc/reference/baseline_arpls.md), [`baseline_als()`](https://christiangoueguel.com/specProc/reference/baseline_als.md), [`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md) |
| Normalization | [`snv()`](https://christiangoueguel.com/specProc/reference/snv.md), [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md), [`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md), [`normalize()`](https://christiangoueguel.com/specProc/reference/normalize.md) |
| Scaling and centering | [`center()`](https://christiangoueguel.com/specProc/reference/center.md), [`pareto_scale()`](https://christiangoueguel.com/specProc/reference/pareto_scale.md), [`poisson_scale()`](https://christiangoueguel.com/specProc/reference/poisson_scale.md), [`minmax()`](https://christiangoueguel.com/specProc/reference/minmax.md) |
| Orthogonal filtering | [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md), [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md), [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md), [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md), [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md), [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md), [`predict()`](https://rdrr.io/r/stats/predict.html) |
| Interference removal | [`epo()`](https://christiangoueguel.com/specProc/reference/epo.md), [`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md), [`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md) |
| Calibration transfer | [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md) |
| Robust PCA | [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md), [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md), [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md), [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md), [`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md), [`flagged_regions()`](https://christiangoueguel.com/specProc/reference/flagged_regions.md), [`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md), [`loading_peaks()`](https://christiangoueguel.com/specProc/reference/loading_peaks.md), [`contributions()`](https://christiangoueguel.com/specProc/reference/contributions.md), [`plot_contributions()`](https://christiangoueguel.com/specProc/reference/plot_contributions.md) |
| Figures of merit | [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md) |
| Recipe steps (tidymodels) | [`step_baseline()`](https://christiangoueguel.com/specProc/reference/step_baseline.md), [`step_snv()`](https://christiangoueguel.com/specProc/reference/step_snv.md), [`step_msc()`](https://christiangoueguel.com/specProc/reference/step_msc.md), [`step_emsc()`](https://christiangoueguel.com/specProc/reference/step_emsc.md), [`step_pareto_scale()`](https://christiangoueguel.com/specProc/reference/step_pareto_scale.md), [`step_poisson_scale()`](https://christiangoueguel.com/specProc/reference/step_poisson_scale.md), [`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md), [`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md), [`step_osc()`](https://christiangoueguel.com/specProc/reference/step_osc.md), [`step_direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/step_direct_orthogonal.md), [`step_direct_osc()`](https://christiangoueguel.com/specProc/reference/step_direct_osc.md), [`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md), [`step_y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/step_y_gradient_glsw.md), [`step_robust_bcyj()`](https://christiangoueguel.com/specProc/reference/step_robust_bcyj.md), [`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md), [`step_rospca()`](https://christiangoueguel.com/specProc/reference/step_rospca.md), [`step_macropca()`](https://christiangoueguel.com/specProc/reference/step_macropca.md) |
| Line profiles | [`voigt_profile()`](https://christiangoueguel.com/specProc/reference/voigt_profile.md), [`pseudo_voigt_profile()`](https://christiangoueguel.com/specProc/reference/pseudo_voigt_profile.md), [`gaussian_profile()`](https://christiangoueguel.com/specProc/reference/gaussian_profile.md), [`lorentzian_profile()`](https://christiangoueguel.com/specProc/reference/lorentzian_profile.md) |
| Line fitting | [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md), [`multipeak_fit()`](https://christiangoueguel.com/specProc/reference/multipeak_fit.md), [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md), [`voigt_fwhm()`](https://christiangoueguel.com/specProc/reference/voigt_fwhm.md) |
| Line identification | [`line_finder()`](https://christiangoueguel.com/specProc/reference/line_finder.md) (Shiny app), [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md), [`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md) |
| Plasma diagnostics | [`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md), [`nist_ionization_energy()`](https://christiangoueguel.com/specProc/reference/nist_ionization_energy.md), [`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md), [`read_starkb()`](https://christiangoueguel.com/specProc/reference/read_starkb.md), [`stark_table()`](https://christiangoueguel.com/specProc/reference/stark_table.md), [`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md), [`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md), [`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md), [`saha_boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann_plot.md), [`plot_boltzmann()`](https://christiangoueguel.com/specProc/reference/plot_boltzmann.md), [`mcwhirter_criterion()`](https://christiangoueguel.com/specProc/reference/mcwhirter_criterion.md), [`self_absorption()`](https://christiangoueguel.com/specProc/reference/self_absorption.md), [`saturation_summary()`](https://christiangoueguel.com/specProc/reference/saturation_summary.md) |
| Location and scale | [`biweight_location()`](https://christiangoueguel.com/specProc/reference/biweight_location.md), [`biweight_scale()`](https://christiangoueguel.com/specProc/reference/biweight_scale.md), [`biweight_midvariance()`](https://christiangoueguel.com/specProc/reference/biweight_midvariance.md), [`rousseeuw_croux()`](https://christiangoueguel.com/specProc/reference/rousseeuw_croux.md), [`umad()`](https://christiangoueguel.com/specProc/reference/umad.md) |
| Association | [`correlation()`](https://christiangoueguel.com/specProc/reference/correlation.md), [`biweight_midcovariance()`](https://christiangoueguel.com/specProc/reference/biweight_midcovariance.md), [`biweight_midcorrelation()`](https://christiangoueguel.com/specProc/reference/biweight_midcorrelation.md) |
| Skewness and tail weight | [`medcouple_weight()`](https://christiangoueguel.com/specProc/reference/medcouple_weight.md), [`quantile_weight()`](https://christiangoueguel.com/specProc/reference/quantile_weight.md), [`tukey_gh()`](https://christiangoueguel.com/specProc/reference/tukey_gh.md) |
| Outlier detection | [`zscore()`](https://christiangoueguel.com/specProc/reference/zscore.md), [`iqr_outliers()`](https://christiangoueguel.com/specProc/reference/iqr_outliers.md), [`directional_outlyingness()`](https://christiangoueguel.com/specProc/reference/directional_outlyingness.md), [`plot_outliers()`](https://christiangoueguel.com/specProc/reference/plot_outliers.md) |
| Exploration | [`summary_stats()`](https://christiangoueguel.com/specProc/reference/summary_stats.md), [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md), [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md), [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md), [`average()`](https://christiangoueguel.com/specProc/reference/average.md) |
| Transformation to normality | [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md) |

### Robust estimators at a glance

For estimators of scale, the breakdown point is the largest fraction of
contaminated observations the estimator can tolerate. The efficiency is
its asymptotic efficiency relative to the standard deviation for normal
data.

| Estimator | Function | Breakdown point | Efficiency |
|----|----|----|----|
| Standard deviation | [`stats::sd()`](https://rdrr.io/r/stats/sd.html) | 0% | 100% |
| MAD (bias-corrected) | [`umad()`](https://christiangoueguel.com/specProc/reference/umad.md) | 50% | 37% |
| Rousseeuw–Croux Sn | `rousseeuw_croux(estimator = "Sn")` | 50% | 58% |
| Rousseeuw–Croux Qn | `rousseeuw_croux(estimator = "Qn")` | 50% | 82% |
| Biweight midvariance | [`biweight_midvariance()`](https://christiangoueguel.com/specProc/reference/biweight_midvariance.md), [`biweight_scale()`](https://christiangoueguel.com/specProc/reference/biweight_scale.md) | high | ≈ 87% |

[`umad()`](https://christiangoueguel.com/specProc/reference/umad.md) and
[`rousseeuw_croux()`](https://christiangoueguel.com/specProc/reference/rousseeuw_croux.md)
include finite-sample bias corrections (Park et al., 2020; Akinshin,
2022), so they are also unbiased at the normal distribution for small
samples, such as the 8 replicates per sample.

## References

The main methodological references are:

- Baek, S.-J., et al. (2015). Baseline correction using asymmetrically
  reweighted penalized least squares smoothing. *Analyst*, 140, 250–257.
- Eilers, P.H.C., Boelens, H.F.M. (2005). Baseline correction with
  asymmetric least squares smoothing.
- Roger, J.-M., Chauchard, F., Bellon-Maurel, V. (2003). EPO-PLS
  external parameter orthogonalisation of PLS. *Chemometr. Intell. Lab.
  Syst.*, 66, 191–204.
- Martens, H., et al. (2003). Pre-whitening of data by
  covariance-weighted pre-processing. *J. Chemometrics*, 17, 153–165.
- Trygg, J., Wold, S. (2002). Orthogonal projections to latent
  structures (O-PLS). *J. Chemometrics*, 16, 119–128.
- Weideman, J.A.C. (1994). Computation of the complex error function.
  *SIAM J. Numer. Anal.*, 31, 1497–1518.
- Rousseeuw, P.J., Croux, C. (1993). Alternatives to the median absolute
  deviation. *JASA*, 88, 1273–1283.
- Hubert, M., Vandervieren, E. (2008). An adjusted boxplot for skewed
  distributions. *Comput. Stat. Data Anal.*, 52, 5186–5201.
- Bruffaerts, C., Verardi, V., Vermandele, C. (2014). A generalized
  boxplot for skewed and heavy-tailed distributions. *Stat. Probab.
  Lett.*, 95, 110–117.

Each function’s help page gives the full references for its method.

## Contributing

Bug reports and feature requests are welcome on the [issue
tracker](https://github.com/ChristianGoueguel/specProc/issues). Please
include a minimal reproducible example.

## License

MIT © Christian L. Goueguel
