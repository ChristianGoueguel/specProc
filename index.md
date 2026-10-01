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
| Plasma diagnostics | [`saturation_summary()`](https://christiangoueguel.com/specProc/reference/saturation_summary.md), [`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md), [`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md), [`saha_boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md), [`plot_boltzmann()`](https://christiangoueguel.com/specProc/reference/plot_boltzmann.md), [`mcwhirter_criterion()`](https://christiangoueguel.com/specProc/reference/mcwhirter_criterion.md), [`self_absorption()`](https://christiangoueguel.com/specProc/reference/self_absorption.md), [`correct_self_absorption()`](https://christiangoueguel.com/specProc/reference/correct_self_absorption.md), [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md), [`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md), [`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md) |
| Outliers and robust PCA | [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md), [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md), [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md), [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md), [`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md), [`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md), [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md), [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md), [`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md) |
| Robust statistics | [`summary_stats()`](https://christiangoueguel.com/specProc/reference/summary_stats.md), [`biweight_location()`](https://christiangoueguel.com/specProc/reference/biweight_location.md), [`biweight_scale()`](https://christiangoueguel.com/specProc/reference/biweight_scale.md), [`rousseeuw_croux()`](https://christiangoueguel.com/specProc/reference/rousseeuw_croux.md), [`umad()`](https://christiangoueguel.com/specProc/reference/umad.md), [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md), [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md) |
| Visualization | [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md), [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md), [`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md), [`plot_contributions()`](https://christiangoueguel.com/specProc/reference/plot_contributions.md) |

See the [reference](https://christiangoueguel.com/specProc/reference/)
for the full list.

## License

MIT © Christian L. Goueguel
