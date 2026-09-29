# Changelog

## specProc (development version)

### Breaking changes

- The data sets are renamed: `specLIBS` is now `soilLIBS`, and
  `fourrage` is now `forageLIBS`. Replace `data(specLIBS)` with
  `data(soilLIBS)` and `data(fourrage)` with `data(forageLIBS)`.

### New features

- [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md)
  computes the distance to the model in the space of the variables
  (DModX, as in SIMCA) of a PCA model, normalized or absolute, for the
  calibration samples or new ones. Its limits use the effective number
  of residual dimensions by default (`df = "effective"`), because
  SIMCA’s degrees of freedom (`df = "simca"`) flag a large share of
  ordinary samples when spectra have far more channels than samples.
  [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md)
  draws DModX or Q against T-squared.

- [`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md),
  [`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md),
  [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md),
  [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md)
  and the T-squared ellipses of
  [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  take any confidence level(s) (`conf_level`, `t2_level`;
  `c(0.95, 0.99)` by default, or a single one), and the Beta
  distribution of T-squared for the samples of the model
  (`method = "beta"`).
  [`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md)
  and the T-squared ellipses of
  [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  now use the HotellingEllipse package (1.3.0 or later).

- [`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md)
  fits a correction of the wavelength axis from reference lines (for
  example, from the NIST database), located to a fraction of a channel
  by Gaussian interpolation, with one correction per detector segment
  (constant, linear or quadratic) and automatic rejection of edge, weak,
  saturated and outlying lines.
  [`apply_calibration()`](https://christiangoueguel.com/specProc/reference/apply_calibration.md)
  relabels the wavelengths of spectra without changing their
  intensities, [`predict()`](https://rdrr.io/r/stats/predict.html)
  corrects any wavelength, and
  [`plot_wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/plot_wavelength_calibration.md)
  draws the offsets and the fit.

- [`savitzky_golay()`](https://christiangoueguel.com/specProc/reference/savitzky_golay.md)
  also splits the axis where overlapping detectors make the wavelengths
  step back.

- The periodic table of
  [`line_finder()`](https://christiangoueguel.com/specProc/reference/line_finder.md)
  can be hidden (button **Hide table**, or `show_table = FALSE` at
  start) to give the spectrum the whole height of the window; the
  selected elements stay listed in the panel header.

- [`savitzky_golay()`](https://christiangoueguel.com/specProc/reference/savitzky_golay.md)
  and
  [`step_savgol()`](https://christiangoueguel.com/specProc/reference/step_savgol.md):
  Savitzky-Golay smoothing and first or second derivatives, keeping
  every channel (polynomial fits at the edges) and filtering the
  segments between detector gaps separately. The window, polynomial
  order and derivative are tunable (new dials parameter
  [`savgol_derivative()`](https://christiangoueguel.com/specProc/reference/savgol_derivative.md)).

- [`normalize()`](https://christiangoueguel.com/specProc/reference/normalize.md)
  gains the L1 (`"l1"`), L2 or vector (`"l2"`) and maximum (`"max"`)
  norms, and
  [`step_spectral_norm()`](https://christiangoueguel.com/specProc/reference/step_spectral_norm.md)
  normalizes each spectrum in a recipe by its L1 norm, total area, L2
  norm or maximum, with the method tunable (new dials parameter
  [`spectral_norm_method()`](https://christiangoueguel.com/specProc/reference/spectral_norm_method.md)).

- [`wavelet_features()`](https://christiangoueguel.com/specProc/reference/wavelet_features.md)
  and
  [`step_wavelet()`](https://christiangoueguel.com/specProc/reference/step_wavelet.md):
  discrete wavelet transform of spectra (Haar, Daubechies d4/d6/d8 and
  least asymmetric la8 wavelets) as compressed features: the
  approximation at a level or all coefficients, optionally the
  coefficients of largest variance in the training data. The level and
  number of coefficients are tunable (new dials parameter
  [`wavelet_level()`](https://christiangoueguel.com/specProc/reference/wavelet_level.md)).

- [`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md)
  computes Hotelling’s T-squared and the Q residual (SPE) of each sample
  for a PCA model, for the calibration samples or new ones, with their
  95% and 99% limits (Jackson-Mudholkar or Box for Q), and classifies
  the samples as regular, extreme, residual or both.
  [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md)
  draws Q against T-squared with the limits.

- [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md)
  gains prediction intervals:
  [`plot_calibration()`](https://christiangoueguel.com/specProc/reference/plot_calibration.md)
  draws the confidence band, the prediction band or both (`interval`),
  and shows new samples (`newdata`) at their predicted concentrations
  with their intervals; `predict(type = "signal")` gives the expected
  signal at given concentrations with a confidence or prediction
  interval; the coefficients have confidence intervals.

- The plots of
  [`correlation()`](https://christiangoueguel.com/specProc/reference/correlation.md)
  are redesigned. For spectra (variables named by wavelength), the plot
  is a correlation spectrum with the 5% significance thresholds;
  otherwise, a sorted chart colored by sign and labeled with the values,
  with `top` to show only the strongest correlations. The interactive
  versions are built directly with plotly (WebGL for spectra) instead of
  converted from ggplot2. `color` now takes one color or two (positive
  and negative correlations).

- [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  draws two-dimensional embeddings (UMAP from
  [`embed::step_umap()`](https://embed.tidymodels.org/reference/step_umap.html),
  principal components from
  [`recipes::step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html),
  [`prcomp()`](https://rdrr.io/r/stats/prcomp.html) or
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)),
  colored by a variable, with optional confidence ellipses per group
  (classical or robust, normal or Hotelling quantiles) from the
  ConfidenceEllipse package, and Hotelling’s T-squared 95% and 99%
  ellipses with the outlying samples labeled (`hotelling`, for all
  samples or within groups, on `k` components). The preprocessing
  vignette uses it to compare PCA and UMAP maps of the samples.

- [`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md)
  computes Hotelling’s T-squared of each sample on `k` components of an
  embedding, with its 95% and 99% limits, for all samples or within
  groups.

## specProc 0.6.0

### New features

- [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md):
  calibration-free LIBS quantification (Ciucci et al., 1999). The
  temperature is estimated from the common slope of parallel Boltzmann
  plots (one per species) or, when the electron density is known,
  Saha-Boltzmann plots (one per element, much more precise). The
  densities follow from the intercepts and the partition functions, and
  the ionization stages not observed from the Saha equation. The
  composition (atomic and mass fractions) is normalized by closure or by
  the known concentration of one element.
  [`plot_boltzmann()`](https://christiangoueguel.com/specProc/reference/plot_boltzmann.md)
  draws the plots of the fit. The plasma diagnostics vignette applies it
  to the forage samples and compares it with their laboratory values.
- [`nist_levels()`](https://christiangoueguel.com/specProc/reference/nist_levels.md)
  retrieves energy levels from the NIST Atomic Spectra Database, and
  [`partition_function()`](https://christiangoueguel.com/specProc/reference/nist_levels.md)
  computes partition functions from them, optionally truncated at a
  maximum energy (such as the lowered ionization energy).
- [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md)
  flags the outlying laser shots of each sample (total intensity,
  correlation with or distance to the median spectrum, by robust
  z-scores), before they are averaged with
  [`average()`](https://christiangoueguel.com/specProc/reference/average.md).
- [`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)
  measures the area (or height, or Voigt-fitted area) of emission lines
  in spectra, searching each peak near its tabulated wavelength, with a
  signal-to-noise ratio and saturation check. It keeps the columns of a
  table of lines, so its result feeds
  [`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md),
  [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md)
  and
  [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md).
  [`step_line_intensities()`](https://christiangoueguel.com/specProc/reference/step_line_intensities.md)
  does the same in a recipe.
- [`correct_self_absorption()`](https://christiangoueguel.com/specProc/reference/correct_self_absorption.md)
  corrects line intensities for self-absorption by the internal
  reference method of Sun and Yu (2009), with the temperature given or
  estimated from a Saha-Boltzmann plot.
- [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md)
  fits univariate (linear or quadratic, optionally weighted) calibration
  curves, with the sensitivity, LOD and LOQ, Mandel’s and lack-of-fit
  tests of linearity, inverse prediction of concentrations with
  confidence intervals
  ([`predict()`](https://rdrr.io/r/stats/predict.html)) and
  [`plot_calibration()`](https://christiangoueguel.com/specProc/reference/plot_calibration.md).
- New recipe steps:
  [`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md)
  removes the outlying shots from the training data (skipped on new
  data), and
  [`step_line_ratio()`](https://christiangoueguel.com/specProc/reference/step_line_ratio.md)
  normalizes each spectrum to the area or height of a reference line
  (internal standard).
- [`line_finder()`](https://christiangoueguel.com/specProc/reference/line_finder.md):
  a Shiny app to identify the emission lines of LIBS spectra. Elements
  are selected on a periodic table, and the strongest lines of their
  ionization stages, from the NIST Atomic Spectra Database, are overlaid
  on a spectrum in an interactive plotly graph (one color per stage),
  with controls for the temperature, wavelength range, number of lines,
  wavelength shift and spectrum (mean, group mean or single spectrum).
  Fetched lines can be saved and reloaded for offline use.
- [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md)
  lists the lines of selected species expected to be the strongest in a
  plasma at a given temperature (relative intensities in LTE), and
  [`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md)
  overlays them on a spectrum (plotly or ggplot2).

## specProc 0.5.0

This release adds plasma diagnostics for LIBS: electron density from
Stark broadening, excitation temperature from Boltzmann and
Saha-Boltzmann plots, and checks of LTE, self-absorption and detector
saturation, with atomic data from the NIST Atomic Spectra Database and
Stark parameters from STARK-B.

### New features

- Plasma diagnostics for LIBS:
  - [`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md)
    retrieves Stark widths and shifts of the lines of an atom or ion
    from the STARK-B database (Sahal-Bréchot, Dimitrijević and Moreau)
    through its VAMDC service, on demand;
    [`read_starkb()`](https://christiangoueguel.com/specProc/reference/read_starkb.md)
    reads STARK-B data saved as XSAMS files, for offline and
    reproducible work;
    [`stark_table()`](https://christiangoueguel.com/specProc/reference/stark_table.md)
    builds the same table from user-supplied widths or from fitted
    temperature laws.
    [`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)
    interpolates the width at a given temperature and electron density,
    and scales multiplet data to a line of the multiplet (lambda-squared
    rule).
  - [`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md)
    estimates the electron density from the Stark (Lorentzian) width of
    a line, with STARK-B data or a reference width, or from the H-alpha
    line (Gigosos et al., 2003).
  - [`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md)
    and
    [`saha_boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann_plot.md)
    estimate the excitation temperature from line intensities and atomic
    data, and
    [`plot_boltzmann()`](https://christiangoueguel.com/specProc/reference/plot_boltzmann.md)
    draws the plots.
  - [`mcwhirter_criterion()`](https://christiangoueguel.com/specProc/reference/mcwhirter_criterion.md)
    checks the McWhirter criterion for LTE, and
    [`self_absorption()`](https://christiangoueguel.com/specProc/reference/self_absorption.md)
    computes self-absorption coefficients from line widths (El Sherbini
    et al., 2005).
  - [`saturation_summary()`](https://christiangoueguel.com/specProc/reference/saturation_summary.md)
    finds channels at the saturation limit of the detector.
  - [`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)
    and
    [`nist_ionization_energy()`](https://christiangoueguel.com/specProc/reference/nist_ionization_energy.md)
    retrieve transition probabilities, level energies, statistical
    weights and ionization energies from the NIST Atomic Spectra
    Database, on demand.
- [`voigt_fwhm()`](https://christiangoueguel.com/specProc/reference/voigt_fwhm.md)
  computes the full width at half maximum of a Voigt profile (Olivero
  and Longbothum, 1977).
- New vignette “Plasma diagnostics: electron density, temperature and
  self-absorption”, on `specLIBS` and `fourrage` spectra.

## specProc 0.4.0

This release adds robust PCA, a proper net analyte signal with figures
of merit, and rewrites the documentation around tidymodels.

### Breaking changes

- [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md) now
  computes the net analyte signal (Lorber, Faber and Kowalski, 1997;
  Faber, 1998) of an inverse PLS or PCR calibration model, with its
  figures of merit: sensitivity, selectivity and, given the noise level,
  analytical sensitivity, limits of detection and quantification, and
  signal-to-noise ratios. A
  [`predict()`](https://rdrr.io/r/stats/predict.html) method gives the
  NAS, selectivity and predicted concentration of new samples.
  Previously,
  [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md)
  returned spectra filtered by direct orthogonalization, identical to
  [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md),
  which remains available for that purpose.

### New features

- Robust PCA:
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
  (ROBPCA, Hubert, Rousseeuw and Vanden Branden,
  2005. and
        [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
        (robust sparse PCA, Hubert, Reynkens, Schmitt and Verdonck,
        2016), implemented from the published algorithms with C++
        kernels for the Stahel-Donoho outlyingness, FAST-MCD and the
        grid-search sparse PCA; and
        [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
        a wrapper around
        [`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html)
        for cellwise outliers and missing values. All three return score
        and orthogonal distances with cut-offs, an outlier
        classification, and a
        [`predict()`](https://rdrr.io/r/stats/predict.html) method for
        new observations.
- [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
  draws the outlier map (score distance against orthogonal distance) of
  any of the three, optionally with new observations;
  [`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)
  draws the cell map of a MacroPCA fit.
- New recipe steps
  [`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md),
  [`step_rospca()`](https://christiangoueguel.com/specProc/reference/step_rospca.md)
  and
  [`step_macropca()`](https://christiangoueguel.com/specProc/reference/step_macropca.md),
  robust counterparts of
  [`recipes::step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html)
  that can also add the score and orthogonal distances as columns, and
  [`step_robust_bcyj()`](https://christiangoueguel.com/specProc/reference/step_robust_bcyj.md),
  a robust counterpart of
  [`recipes::step_BoxCox()`](https://recipes.tidymodels.org/reference/step_BoxCox.html)
  and
  [`recipes::step_YeoJohnson()`](https://recipes.tidymodels.org/reference/step_YeoJohnson.html).

### Documentation

- The four vignettes and the README use recipe steps, tidymodels
  (workflows, tune, rsample) and the tidyverse. Preprocessing pipelines
  are written as recipes; the calibration vignette runs its repeated
  nested cross-validation with
  [`rsample::nested_cv()`](https://rsample.tidymodels.org/reference/nested_cv.html)
  and
  [`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
  and the orthogonalization vignette tunes every filter together with
  the PLS model. Vignettes that need suggested packages stop with a
  message when those are not installed.

## specProc 0.3.0

This release connects specProc to the tidymodels framework.
Preprocessing and orthogonalization can now be estimated on calibration
data and applied to new spectra, either directly with
[`predict()`](https://rdrr.io/r/stats/predict.html) or as recipe steps
that are re-estimated on every resample and tuned together with the
model.

### New features

- The orthogonalization filters can now be applied to new spectra:
  [`epo()`](https://christiangoueguel.com/specProc/reference/epo.md),
  [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md),
  [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md),
  [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md),
  [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
  and
  [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md)
  return classed objects with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method, which
  corrects new data with the centers, scales and components estimated on
  the calibration data. Existing fields are unchanged.
- New recipe steps for tidymodels:
  [`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md),
  [`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md),
  [`step_osc()`](https://christiangoueguel.com/specProc/reference/step_osc.md),
  [`step_direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/step_direct_orthogonal.md),
  [`step_direct_osc()`](https://christiangoueguel.com/specProc/reference/step_direct_osc.md),
  [`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md)
  and
  [`step_y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/step_y_gradient_glsw.md),
  with [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`tunable()`](https://generics.r-lib.org/reference/tunable.html)
  methods, and the dials parameter
  [`glsw_alpha()`](https://christiangoueguel.com/specProc/reference/glsw_alpha.md).
  In a workflow, the filters are re-estimated on every resample and can
  be tuned with the model. The GLSW steps store the filter in factored
  form instead of a p x p matrix. recipes and dials are suggested, not
  required.
- New
  [`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md):
  extended multiplicative signal correction (Martens and Stark, 1991),
  with a polynomial baseline and optional interferent spectra, and a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method for new
  spectra. With `degree = 0` it equals
  [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md).
- New recipe steps for spectral preprocessing:
  [`step_baseline()`](https://christiangoueguel.com/specProc/reference/step_baseline.md)
  (arPLS, ALS or LSP),
  [`step_snv()`](https://christiangoueguel.com/specProc/reference/step_snv.md),
  [`step_msc()`](https://christiangoueguel.com/specProc/reference/step_msc.md),
  [`step_emsc()`](https://christiangoueguel.com/specProc/reference/step_emsc.md),
  [`step_pareto_scale()`](https://christiangoueguel.com/specProc/reference/step_pareto_scale.md)
  and
  [`step_poisson_scale()`](https://christiangoueguel.com/specProc/reference/step_poisson_scale.md).
  MSC and EMSC estimate the reference spectrum, and the scaling steps
  the column scales, on the training data only. The baseline `lambda` or
  `degree` and the EMSC `degree` are tunable; the new dials parameter
  [`baseline_lambda()`](https://christiangoueguel.com/specProc/reference/baseline_lambda.md)
  covers `lambda`.
- New data set `fourrage`: LIBS spectra of 365 forage samples with
  reference contents of 12 elements, and a vignette based on it,
  “Removing unwanted variation: a comparison of orthogonalization
  methods” (what EPO, GLSW, the OSC family, DO/NAS, DOSC, POSC/OPLS and
  y-gradient GLSW remove, whether it improves potassium predictions, and
  which methods are equivalent).

## specProc 0.2.0

This release fixes a large number of bugs so that every exported
function now runs and returns numerically correct results. It adds a
test suite (400+ expectations), and several results are checked against
independent reference implementations.

### Breaking changes

- Functions were renamed to a consistent snake_case scheme. The old
  names still work but give a deprecation warning (see
  [`?"specProc-deprecated"`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)):
  [`whittaker()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`baseline_als()`](https://christiangoueguel.com/specProc/reference/baseline_als.md),
  [`lorentzian()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`lorentzian_profile()`](https://christiangoueguel.com/specProc/reference/lorentzian_profile.md),
  [`pseudo_voigt()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`pseudo_voigt_profile()`](https://christiangoueguel.com/specProc/reference/pseudo_voigt_profile.md),
  [`peakfit()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md),
  [`multipeakfit()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`multipeak_fit()`](https://christiangoueguel.com/specProc/reference/multipeak_fit.md),
  [`plotfit()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md),
  [`plotSpec()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md),
  [`outlierplot()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`plot_outliers()`](https://christiangoueguel.com/specProc/reference/plot_outliers.md),
  [`directOutlyingness()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`directional_outlyingness()`](https://christiangoueguel.com/specProc/reference/directional_outlyingness.md),
  [`iqrMethod()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`iqr_outliers()`](https://christiangoueguel.com/specProc/reference/iqr_outliers.md),
  [`robustBCYJ()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md),
  [`rousseeuwCroux()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`rousseeuw_croux()`](https://christiangoueguel.com/specProc/reference/rousseeuw_croux.md),
  [`summaryStats()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`summary_stats()`](https://christiangoueguel.com/specProc/reference/summary_stats.md),
  [`tukeyGH()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`tukey_gh()`](https://christiangoueguel.com/specProc/reference/tukey_gh.md),
  [`yGradientglsw()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md),
  [`pareto()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  -\>
  [`pareto_scale()`](https://christiangoueguel.com/specProc/reference/pareto_scale.md).

- [`gaussian()`](https://rdrr.io/r/stats/family.html) is renamed to
  [`gaussian_profile()`](https://christiangoueguel.com/specProc/reference/gaussian_profile.md)
  **without** an alias, because exporting
  [`gaussian()`](https://rdrr.io/r/stats/family.html) masked
  [`stats::gaussian()`](https://rdrr.io/r/stats/family.html), which
  breaks `glm(family = gaussian)` when specProc is loaded.

- [`baseline_arpls()`](https://christiangoueguel.com/specProc/reference/baseline_arpls.md),
  [`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md)
  and
  [`whittaker()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  return tibbles (they previously returned plain lists of columns). They
  no longer replace negative corrected values with 1.

- [`average()`](https://christiangoueguel.com/specProc/reference/average.md)
  with `.group_by` now returns the group labels as the first column.

- [`biweight_location()`](https://christiangoueguel.com/specProc/reference/biweight_location.md)
  and
  [`biweight_scale()`](https://christiangoueguel.com/specProc/reference/biweight_scale.md)
  use the unscaled MAD, following the standard definition (Beers et al.,
  1990), so that `biweight_scale()^2` equals
  [`biweight_midvariance()`](https://christiangoueguel.com/specProc/reference/biweight_midvariance.md).
  [`biweight_scale()`](https://christiangoueguel.com/specProc/reference/biweight_scale.md)
  loses its unused `tol` and `max_iter` arguments. `loc = NULL` now
  means “median of `x`”.

- [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md)
  loses its unused `max_iter` and `tol` arguments, and
  [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md)
  loses `max_iter`. `tol` now sets the pseudo-inverse tolerance.

- [`epo()`](https://christiangoueguel.com/specProc/reference/epo.md)
  gains a `clutter` argument (the matrix describing the external
  variation). Its default `ncomp` is now 2, as documented. It now
  removes the *dominant* singular directions; before, it removed the
  smallest ones.

- [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md)
  has a new interface: `o2pls(x, y, ncomp, nx, ny, center, scale)`.

- [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md)
  returns `transfer_matrix` (was `transfert_matrix`), `alpha` now blends
  the local and global models, and progress is no longer printed.

- [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
  always returns the corrected training data. New data are preprocessed
  with the training centers and scales.

- [`quantile_weight()`](https://christiangoueguel.com/specProc/reference/quantile_weight.md)
  uses whole-distribution quantiles, as in Brys et al. (2006), and
  defaults to `p = 0.125`, `q = 0.875`.

- `summaryStats(robust = TRUE)` drops the `rsd` column. It applied the
  1.4826 consistency factor twice, and `mad` already holds the correct
  value.

- [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md)
  now requires the Bioconductor package ropls to be installed (it moved
  to Suggests). It no longer prints output or draws plots.

- R \>= 4.1.0 is required.

### Bug fixes

- Fixed crashes in
  [`baseline_arpls()`](https://christiangoueguel.com/specProc/reference/baseline_arpls.md),
  [`whittaker()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md),
  [`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md)
  (data frames), `correlation(method = "bicor")`,
  `directOutlyingness(maxRatio = )`,
  [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md),
  [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md),
  [`multipeakfit()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md),
  [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md),
  [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md),
  [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md)
  (`"wold"`, `"fearn"`),
  [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md),
  [`peakfit()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md),
  [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
  and
  [`yGradientglsw()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md).
- [`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md)
  and
  [`yGradientglsw()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  squared the eigenvalues of the covariance matrix when building the
  filter.
- [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md)
  regressed each wavelength instead of each spectrum on the reference.
- [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  transformed the ranks instead of the data. This made the g-and-h
  estimates meaningless. It also failed when the two tails had different
  numbers of outliers.
- [`pseudo_voigt()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  counted the area and the offset twice.
- [`biweight_scale()`](https://christiangoueguel.com/specProc/reference/biweight_scale.md)
  used a wrong exponent in the denominator. It also ignored
  `reduced = TRUE`.
- [`biweight_location()`](https://christiangoueguel.com/specProc/reference/biweight_location.md)
  did not iterate.
- [`average()`](https://christiangoueguel.com/specProc/reference/average.md)
  did not ignore missing values. It also read out of bounds for factors
  with unused levels or missing group values.
- `normalize(method = "background")` rejected data frames for `bkg`.
- [`plotSpec()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  connected all spectra into a single line when `id` was not given.
- [`outlierplot()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  no longer changes the user’s random number generator state.
- [`iqrMethod()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  no longer changes global options. Missing values no longer cause
  errors.

### New features

- Three vignettes based on `specLIBS`: “Preprocessing LIBS spectra”
  (baseline, normalization judged by replicate RSD and intraclass
  correlation, robust screening of shots), “Fitting emission lines”
  (profile choice, sources of uncertainty, identifiability) and
  “Predicting soil clay content from LIBS spectra” (a compositional
  log-ratio PLS model with nested, repeated cross-validation by sample,
  and the optimism of common shortcuts).

- [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md)
  draws the fitted profiles on a fine wavelength grid.

- Functions taking a response now accept 1-d arrays, such as the output
  of [`tapply()`](https://rdrr.io/r/base/tapply.html).

- New
  [`voigt_profile()`](https://christiangoueguel.com/specProc/reference/voigt_profile.md):
  the exact Voigt profile, computed in C++ from the Faddeeva function
  with Weideman’s (1994) rational approximation (relative error below
  1e-10).
  [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)
  and
  [`multipeak_fit()`](https://christiangoueguel.com/specProc/reference/multipeak_fit.md)
  now use it for `profile = "voigt"`; the pseudo-Voigt approximation is
  available as `profile = "pseudo_voigt"`.

- New data set `specLIBS`: 400 LIBS spectra (50 soil samples x 8
  locations, 7152 channels, 199-822 nm) with the clay, sand and silt
  content of each sample.

- [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  whiskers now end at the most extreme observations within the fences,
  as in standard boxplots. The fences are returned as `lower_fence` and
  `upper_fence`.

- The README has been rewritten around a complete worked example on
  `specLIBS`.

### Improvements

- Asymmetric least squares and arPLS baselines are solved in C++ with a
  banded Cholesky factorization. The cost is O(n) per spectrum, so they
  scale to spectra with tens of thousands of channels.
- EPO and GLSW use Eigen’s divide-and-conquer SVD and never form the p x
  p covariance or projection matrices.
- [`peakfit()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  and
  [`multipeakfit()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  estimate starting values from the data when `wL`, `wG` or `A` are not
  given.
  [`multipeakfit()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  fits all lines simultaneously and returns each line’s contribution. A
  spectrum that fails to fit produces a warning instead of stopping the
  whole batch.
- The orthogonalization functions return the centers and scales, so the
  same preprocessing can be applied to new data.
- Fewer dependencies: corrr, cowplot, forcats, mt, broom and ropls are
  no longer imported.

## specProc 0.1.0

- Added a `NEWS.md` file to track changes to the package.
