# Package index

## Univariate filtering

Functions used to assess individual variables or features independently.

- [`zscore()`](https://christiangoueguel.com/specProc/reference/zscore.md)
  : Classical or Robust Z-Score
- [`iqr_outliers()`](https://christiangoueguel.com/specProc/reference/iqr_outliers.md)
  : Interquartile Range Method
- [`directional_outlyingness()`](https://christiangoueguel.com/specProc/reference/directional_outlyingness.md)
  : Directional Outlyingness for Skewed Distribution

## Multivariate filtering

### robust PCA

Principal component analysis that resists outlying observations (and,
for MacroPCA and cellPCA, outlying cells and missing values).

- [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
  : Robust Principal Component Analysis (ROBPCA)
- [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  : Robust Sparse Principal Component Analysis (ROSPCA)
- [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  : Robust PCA for Cellwise and Casewise Outliers (MacroPCA)
- [`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md)
  : Robust PCA by Casewise and Cellwise Weighting (cellPCA)
- [`predict(`*`<specproc_macropca>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_robpca.md)
  [`predict(`*`<specproc_cellpca>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_robpca.md)
  [`predict(`*`<specproc_robpca>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_robpca.md)
  : Scores and Distances of New Observations
- [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
  : Outlier Map of a Robust PCA or Robust PLS Model
- [`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)
  : Cell Map of a MacroPCA or cellPCA Fit
- [`flagged_regions()`](https://christiangoueguel.com/specProc/reference/flagged_regions.md)
  : Flagged Regions of a MacroPCA or cellPCA Fit

### self-organizing maps

Nonlinear maps of the spectra onto a grid of prototype spectra, for
sorting samples and flagging novel ones.

- [`som()`](https://christiangoueguel.com/specProc/reference/som.md) :
  Self-Organizing Map of Spectra
- [`predict(`*`<specproc_som>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_som.md)
  : Map New Spectra onto a Self-Organizing Map
- [`plot_som()`](https://christiangoueguel.com/specProc/reference/plot_som.md)
  : Plot a Self-Organizing Map
- [`som_stability()`](https://christiangoueguel.com/specProc/reference/som_stability.md)
  : Stability of a Self-Organizing Map

### orthogonalization methods

Functions used to identify and remove unwanted covariance structures and
sources of variance from the multivariate data.

- [`epo()`](https://christiangoueguel.com/specProc/reference/epo.md) :
  External Parameter Orthogonalization
- [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md) :
  Orthogonal Signal Correction
- [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md) :
  Orthogonal Projections to Latent Structures
- [`predict(`*`<specproc_opls>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_opls.md)
  : Predict with an OPLS Model
- [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md)
  : Modified Orthogonal Projections to Latent Structures
- [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md)
  : Direct Orthogonal Signal Correction
- [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
  : Projected Orthogonal Signal Correction
- [`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md)
  : y-Gradient Generalized Least Squares Weighting
- [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md)
  : Direct Orthogonalization
- [`predict(`*`<specproc_epo>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
  [`predict(`*`<specproc_osc>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
  [`predict(`*`<specproc_direct_orthogonal>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
  [`predict(`*`<specproc_direct_osc>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
  [`predict(`*`<specproc_projected_osc>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
  [`predict(`*`<o2pls>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
  : Apply an Orthogonalization Filter to New Spectra

### robust PLS and PCR regression, wavelength selection

Robust PLS and principal component regression that resist outlying
spectra and wrong reference values, their robust cross-validation, and
the selection of the informative wavelengths (VIP, selectivity ratio,
interval PLS).

- [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
  : Robust Partial Least Squares Regression (RSIMPLS)
- [`predict(`*`<specproc_rsimpls>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_rsimpls.md)
  : Predictions of a Robust PLS Model
- [`pls_rsimpls`](https://christiangoueguel.com/specProc/reference/pls_rsimpls.md)
  : Robust PLS Regression in tidymodels (parsnip Engine)
- [`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md) :
  Robust Principal Component Regression (RPCR)
- [`predict(`*`<specproc_rpcr>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_rpcr.md)
  : Predictions of a Robust PCR Model
- [`robust_rmsecv()`](https://christiangoueguel.com/specProc/reference/robust_rmsecv.md)
  : Robust Cross-Validation of a Robust Calibration Model
- [`lts_fit()`](https://christiangoueguel.com/specProc/reference/linear_reg_lts.md)
  : Robust Linear Regression in tidymodels (LTS parsnip Engine)
- [`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md)
  : Wavelength Selection for PLS Regression
- [`plot_wavelength_selection()`](https://christiangoueguel.com/specProc/reference/plot_wavelength_selection.md)
  : Plot of a Wavelength Selection

### robust classification

Classification that resists outlying observations of the training data:
robust SIMCA for whole spectra, and robust linear and quadratic
discriminant analysis for a few variables (line intensities, robust
principal component scores).

- [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)
  : Robust SIMCA Classification (RSIMCA)
- [`predict(`*`<specproc_rsimca>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_rsimca.md)
  : Predictions of a Robust SIMCA Model
- [`plot_coomans()`](https://christiangoueguel.com/specProc/reference/plot_coomans.md)
  : Coomans Plot of a Robust SIMCA Model
- [`simca()`](https://christiangoueguel.com/specProc/reference/simca.md)
  [`update(`*`<simca>`*`)`](https://christiangoueguel.com/specProc/reference/simca.md)
  : SIMCA Classification Model (parsnip)
- [`simca_gamma()`](https://christiangoueguel.com/specProc/reference/simca_gamma.md)
  [`simca_squared()`](https://christiangoueguel.com/specProc/reference/simca_gamma.md)
  : Tuning Parameters of the SIMCA Model
- [`robust_da()`](https://christiangoueguel.com/specProc/reference/robust_da.md)
  : Robust Linear and Quadratic Discriminant Analysis
- [`predict(`*`<specproc_robust_da>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_robust_da.md)
  : Predictions of a Robust Discriminant Analysis

### figures of merit

Net analyte signal of a multivariate calibration model, with its
sensitivity, selectivity and limits of detection and quantification.

- [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md) :
  Net Analyte Signal and Figures of Merit
- [`predict(`*`<specproc_nas>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_nas.md)
  : Net Analyte Signal of New Samples

### calibration transfer

Functions used to to transfer a calibration model developed on one
instrument to another instrument.

- [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md) :
  Piecewise Direct Standardization
- [`ds()`](https://christiangoueguel.com/specProc/reference/ds.md) :
  Direct Standardization
- [`sst()`](https://christiangoueguel.com/specProc/reference/sst.md) :
  Spectral Space Transformation
- [`predict(`*`<specproc_transfer>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_transfer.md)
  : Transfer Spectra to Another Instrument
- [`slope_bias()`](https://christiangoueguel.com/specProc/reference/slope_bias.md)
  : Slope and Bias Correction of Predictions
- [`predict(`*`<specproc_slope_bias>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_slope_bias.md)
  : Correct Predictions for Slope and Bias
- [`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md) :
  Generalized Least Squares Weighting

## Recipe steps (tidymodels)

Steps for the recipes package, so that preprocessing and
orthogonalization are estimated on the training data of each resample
and can be tuned within a tidymodels workflow.

### preprocessing

- [`step_baseline()`](https://christiangoueguel.com/specProc/reference/step_baseline.md)
  : Baseline Correction Recipe Step
- [`step_savgol()`](https://christiangoueguel.com/specProc/reference/step_savgol.md)
  : Savitzky-Golay Recipe Step
- [`step_gap_derivative()`](https://christiangoueguel.com/specProc/reference/step_gap_derivative.md)
  : Gap-Segment Derivative Recipe Step
- [`step_bin_spectra()`](https://christiangoueguel.com/specProc/reference/step_bin_spectra.md)
  : Binning Recipe Step
- [`step_wavelet()`](https://christiangoueguel.com/specProc/reference/step_wavelet.md)
  : Wavelet Features Recipe Step
- [`step_snv()`](https://christiangoueguel.com/specProc/reference/step_snv.md)
  : Standard Normal Variate Recipe Step
- [`step_spectral_norm()`](https://christiangoueguel.com/specProc/reference/step_spectral_norm.md)
  : Spectral Normalization Recipe Step
- [`step_msc()`](https://christiangoueguel.com/specProc/reference/step_msc.md)
  : Multiplicative Scatter Correction Recipe Step
- [`step_emsc()`](https://christiangoueguel.com/specProc/reference/step_emsc.md)
  : Extended Multiplicative Signal Correction Recipe Step
- [`step_pareto_scale()`](https://christiangoueguel.com/specProc/reference/step_pareto_scale.md)
  : Pareto Scaling Recipe Step
- [`step_poisson_scale()`](https://christiangoueguel.com/specProc/reference/step_poisson_scale.md)
  : Poisson Scaling Recipe Step

### LIBS lines and shots

- [`step_line_intensities()`](https://christiangoueguel.com/specProc/reference/step_line_intensities.md)
  : Emission Line Intensities Recipe Step
- [`step_line_ratio()`](https://christiangoueguel.com/specProc/reference/step_line_ratio.md)
  : Internal Standard Normalization Recipe Step
- [`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md)
  : Shot Rejection Recipe Step

### robust transformation, PCA and PLS

- [`step_robust_bcyj()`](https://christiangoueguel.com/specProc/reference/step_robust_bcyj.md)
  : Robust Box-Cox and Yeo-Johnson Transformation Recipe Step
- [`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)
  : Robust PCA (ROBPCA) Recipe Step
- [`step_rospca()`](https://christiangoueguel.com/specProc/reference/step_rospca.md)
  : Robust Sparse PCA (ROSPCA) Recipe Step
- [`step_macropca()`](https://christiangoueguel.com/specProc/reference/step_macropca.md)
  : MacroPCA Recipe Step
- [`step_cellpca()`](https://christiangoueguel.com/specProc/reference/step_cellpca.md)
  : cellPCA Recipe Step
- [`step_rsimpls()`](https://christiangoueguel.com/specProc/reference/step_rsimpls.md)
  : Robust PLS (RSIMPLS) Recipe Step

### orthogonalization

- [`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md)
  : External Parameter Orthogonalization Recipe Step
- [`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md)
  : Generalized Least Squares Weighting Recipe Step
- [`step_osc()`](https://christiangoueguel.com/specProc/reference/step_osc.md)
  : Orthogonal Signal Correction Recipe Step
- [`step_direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/step_direct_orthogonal.md)
  : Direct Orthogonalization Recipe Step
- [`step_direct_osc()`](https://christiangoueguel.com/specProc/reference/step_direct_osc.md)
  : Direct Orthogonal Signal Correction Recipe Step
- [`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md)
  : Projected Orthogonal Signal Correction (OPLS Filter) Recipe Step
- [`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md)
  : Orthogonal Projections to Latent Structures (OPLS) Recipe Step
- [`step_o2pls()`](https://christiangoueguel.com/specProc/reference/step_o2pls.md)
  : O2PLS Filter Recipe Step for One or Several Outcomes
- [`step_y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/step_y_gradient_glsw.md)
  : y-Gradient Generalized Least Squares Weighting Recipe Step

### wavelength selection

- [`step_select_wavelengths()`](https://christiangoueguel.com/specProc/reference/step_select_wavelengths.md)
  : Wavelength Selection Recipe Step

### tuning parameters

- [`baseline_lambda()`](https://christiangoueguel.com/specProc/reference/baseline_lambda.md)
  : Baseline Smoothness Parameter
- [`glsw_alpha()`](https://christiangoueguel.com/specProc/reference/glsw_alpha.md)
  : Relative GLSW Weighting Parameter
- [`savgol_derivative()`](https://christiangoueguel.com/specProc/reference/savgol_derivative.md)
  : Derivative Order of a Savitzky-Golay Filter
- [`wavelet_level()`](https://christiangoueguel.com/specProc/reference/wavelet_level.md)
  : Number of Levels of a Wavelet Decomposition
- [`spectral_norm_method()`](https://christiangoueguel.com/specProc/reference/spectral_norm_method.md)
  : Norm of a Spectral Normalization
- [`num_intervals()`](https://christiangoueguel.com/specProc/reference/num_intervals.md)
  : Number of Intervals

## Line identification

Candidate emission lines from the NIST Atomic Spectra Database, overlaid
on spectra, an interactive app to identify lines, and the calibration of
the wavelength axis on reference lines.

- [`line_finder()`](https://christiangoueguel.com/specProc/reference/line_finder.md)
  : Interactive Identification of Emission Lines
- [`molecular_bands()`](https://christiangoueguel.com/specProc/reference/molecular_bands.md)
  : Band Heads of Diatomic Molecules
- [`molecular_spectrum()`](https://christiangoueguel.com/specProc/reference/molecular_spectrum.md)
  : Simulated Emission Spectra of Diatomic Molecules
- [`exomol_datasets()`](https://christiangoueguel.com/specProc/reference/exomol_datasets.md)
  : Line Lists of a Molecule in the ExoMol Database
- [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md)
  : Candidate Emission Lines for LIBS Spectra
- [`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md)
  : Overlay Emission Lines on a Spectrum
- [`wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/wavelength_calibration.md)
  : Wavelength Calibration from Reference Lines
- [`apply_calibration()`](https://christiangoueguel.com/specProc/reference/apply_calibration.md)
  : Apply a Wavelength Calibration
- [`predict(`*`<specproc_wavelength_calibration>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_wavelength_calibration.md)
  : Correct Wavelengths with a Wavelength Calibration
- [`plot_wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/plot_wavelength_calibration.md)
  : Plot a Wavelength Calibration

## Spectral response

The relative spectral response of the instrument, measured on a
calibration lamp with its uncertainty, to correct spectra and line
intensities.

- [`response_calibration()`](https://christiangoueguel.com/specProc/reference/response_calibration.md)
  : Spectral Response from a Calibration Lamp
- [`correct_response()`](https://christiangoueguel.com/specProc/reference/correct_response.md)
  : Correct Spectra or Line Intensities for the Spectral Response
- [`plot_response()`](https://christiangoueguel.com/specProc/reference/plot_response.md)
  : Plot a Spectral Response

## Plasma diagnostics

Electron density from Stark broadening (with the STARK-B database),
excitation temperature from Boltzmann and Saha-Boltzmann plots, and
checks of LTE, self-absorption and detector saturation.

- [`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)
  : Atomic Line Data from the NIST Atomic Spectra Database
- [`nist_ionization_energy()`](https://christiangoueguel.com/specProc/reference/nist_ionization_energy.md)
  : Ionization Energies from the NIST Atomic Spectra Database
- [`nist_levels()`](https://christiangoueguel.com/specProc/reference/nist_levels.md)
  [`partition_function()`](https://christiangoueguel.com/specProc/reference/nist_levels.md)
  : Energy Levels and Partition Functions from the NIST Atomic Spectra
  Database
- [`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md)
  : Stark Broadening Parameters from the STARK-B Database
- [`read_starkb()`](https://christiangoueguel.com/specProc/reference/read_starkb.md)
  : Read Saved STARK-B Data
- [`stark_table()`](https://christiangoueguel.com/specProc/reference/stark_table.md)
  : Build a Table of Stark Broadening Parameters
- [`stark_width()`](https://christiangoueguel.com/specProc/reference/stark_width.md)
  : Stark Width of a Line at Given Plasma Conditions
- [`electron_density()`](https://christiangoueguel.com/specProc/reference/electron_density.md)
  : Electron Density from Stark Broadening
- [`electron_density_uncertainty()`](https://christiangoueguel.com/specProc/reference/electron_density_uncertainty.md)
  : Uncertainty of the Electron Density from Stark Broadening
- [`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)
  : Plasma Temperature from a Boltzmann Plot
- [`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md)
  : Plasma Temperature from a Saha-Boltzmann Plot
- [`plot_boltzmann()`](https://christiangoueguel.com/specProc/reference/plot_boltzmann.md)
  : Draw a Boltzmann or Saha-Boltzmann Plot
- [`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md)
  : Monte Carlo Uncertainty of Plasma Diagnostics
- [`aki_uncertainty()`](https://christiangoueguel.com/specProc/reference/aki_uncertainty.md)
  : Uncertainty of Transition Probabilities from NIST Accuracy Grades
- [`mcwhirter_criterion()`](https://christiangoueguel.com/specProc/reference/mcwhirter_criterion.md)
  : McWhirter Criterion for Local Thermodynamic Equilibrium
- [`lte_criteria()`](https://christiangoueguel.com/specProc/reference/lte_criteria.md)
  : Criteria of Local Thermodynamic Equilibrium in Transient Plasmas
- [`self_absorption()`](https://christiangoueguel.com/specProc/reference/self_absorption.md)
  : Self-Absorption Coefficient from Line Widths
- [`correct_self_absorption()`](https://christiangoueguel.com/specProc/reference/correct_self_absorption.md)
  : Self-Absorption Correction with an Internal Reference Line
- [`curve_of_growth()`](https://christiangoueguel.com/specProc/reference/curve_of_growth.md)
  : Self-Absorption by the Curve of Growth
- [`curve_of_growth_model()`](https://christiangoueguel.com/specProc/reference/curve_of_growth_model.md)
  : Intensities of Emission Lines on a Curve of Growth
- [`plot_curve_of_growth()`](https://christiangoueguel.com/specProc/reference/plot_curve_of_growth.md)
  : Plot a Curve of Growth
- [`saturation_summary()`](https://christiangoueguel.com/specProc/reference/saturation_summary.md)
  : Detect Saturated Channels in Spectra

## Line intensities and quantification

Intensities of emission lines, univariate calibration curves with limits
of detection, and calibration-free composition (CF-LIBS).

- [`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)
  : Intensities of Emission Lines
- [`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md)
  : Univariate Calibration Curve
- [`predict(`*`<specproc_calibration>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_calibration.md)
  : Predict from a Calibration Curve
- [`plot_calibration()`](https://christiangoueguel.com/specProc/reference/plot_calibration.md)
  : Plot a Calibration Curve
- [`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md)
  : Calibration-Free LIBS Quantification

## Applicability domain and drift

Flags for the spectra outside the calibration space, and control charts
(Shewhart, EWMA, CUSUM) to detect drifts of the instrument.

- [`applicability_domain()`](https://christiangoueguel.com/specProc/reference/applicability_domain.md)
  : Applicability Domain of a Calibration
- [`predict(`*`<specproc_domain>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_domain.md)
  : Flag Spectra Outside the Applicability Domain
- [`control_chart()`](https://christiangoueguel.com/specProc/reference/control_chart.md)
  : Control Chart for Instrument Drift
- [`plot_control_chart()`](https://christiangoueguel.com/specProc/reference/plot_control_chart.md)
  : Plot a Control Chart

## Peak fitting

Functions used to characterize spectral lines. The choice of the fitting
function depends on the specific broadening mechanisms and physical
conditions of the emitting environment.

- [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)
  : Peak Fitting
- [`multipeak_fit()`](https://christiangoueguel.com/specProc/reference/multipeak_fit.md)
  : Multiple Peaks Fitting
- [`voigt_fwhm()`](https://christiangoueguel.com/specProc/reference/voigt_fwhm.md)
  : Full Width at Half Maximum of a Voigt Profile
- [`doppler_width()`](https://christiangoueguel.com/specProc/reference/doppler_width.md)
  : Doppler Width of an Emission Line

## Data visualization

Functions that help assess data visually.

- [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md)
  : Plotting of Fitted Spectral Line
- [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md)
  : Plotting of Spectra
- [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  : Plot a Two-Dimensional Embedding
- [`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md)
  : Loadings of a PCA as Spectra
- [`loading_peaks()`](https://christiangoueguel.com/specProc/reference/loading_peaks.md)
  : Peaks of the Loadings of a PCA
- [`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md)
  : Hotelling's T-squared Statistic of Samples in an Embedding
- [`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md)
  : Hotelling's T-squared and Q Residuals of a PCA Model
- [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md)
  : Distance to the Model (DModX) of a PCA Model
- [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md)
  : Influence Plot of a PCA Model
- [`contributions()`](https://christiangoueguel.com/specProc/reference/contributions.md)
  : Q and T-squared Contributions of a PCA Model
- [`plot_contributions()`](https://christiangoueguel.com/specProc/reference/plot_contributions.md)
  : Plot Q and T-squared Contributions
- [`plot_outliers()`](https://christiangoueguel.com/specProc/reference/plot_outliers.md)
  : Univariate Representation of Multivariate Outliers
- [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md)
  : Adjusted Boxplot
- [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  : Generalized Boxplot

## Smoothing and derivatives

Functions used to increase the signal-to-noise ratio, or to compute
derivatives of the spectra.

- [`savitzky_golay()`](https://christiangoueguel.com/specProc/reference/savitzky_golay.md)
  : Savitzky-Golay Smoothing and Derivatives
- [`gap_derivative()`](https://christiangoueguel.com/specProc/reference/gap_derivative.md)
  : Gap-Segment Derivatives
- [`bin_spectra()`](https://christiangoueguel.com/specProc/reference/bin_spectra.md)
  : Binning of Adjacent Channels
- [`wavelet_features()`](https://christiangoueguel.com/specProc/reference/wavelet_features.md)
  : Wavelet Coefficients of Spectra
- [`average()`](https://christiangoueguel.com/specProc/reference/average.md)
  : Fast Average for Large Spectral Dataset
- [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md)
  : Rejection of Outlying Laser Shots
- [`plot_shots()`](https://christiangoueguel.com/specProc/reference/plot_shots.md)
  : Plots of the Laser Shots and of their Rejection

## Baseline correction

Functions used to fit and remove the background emission.

- [`baseline_als()`](https://christiangoueguel.com/specProc/reference/baseline_als.md)
  : Asymmetric Least Squares
- [`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md)
  : Least-Squares Polynomial
- [`baseline_arpls()`](https://christiangoueguel.com/specProc/reference/baseline_arpls.md)
  : Asymmetrically Reweighted Penalized Least Squares

## Normalization

### samples normalization

Functions used to adjust the systematic differences among samples.

- [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md) :
  Multiplicative Signal Correction
- [`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md) :
  Extended Multiplicative Signal Correction
- [`predict(`*`<specproc_emsc>`*`)`](https://christiangoueguel.com/specProc/reference/predict.specproc_emsc.md)
  : Apply EMSC to New Spectra
- [`normalize()`](https://christiangoueguel.com/specProc/reference/normalize.md)
  : Spectra Normalization

### data scaling

Functions used to adjust each variable/feature by a scaling factor
computed based on the dispersion of the variable.

- [`snv()`](https://christiangoueguel.com/specProc/reference/snv.md) :
  Standard Normal Variate
- [`pareto_scale()`](https://christiangoueguel.com/specProc/reference/pareto_scale.md)
  : Pareto Scaling
- [`minmax()`](https://christiangoueguel.com/specProc/reference/minmax.md)
  : Min-Max Normalization
- [`center()`](https://christiangoueguel.com/specProc/reference/center.md)
  : Data Centering
- [`poisson_scale()`](https://christiangoueguel.com/specProc/reference/poisson_scale.md)
  : Poisson Scaling

### data transformation

Function used to transform variable(s) toward central normality.

- [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md)
  : Robust Box-Cox and Yeo-Johnson Transformation

## Robust statistical estimators

### biweight

Functions that use Tukey’s biweight formalism.

- [`biweight_scale()`](https://christiangoueguel.com/specProc/reference/biweight_scale.md)
  : Biweight Scale
- [`biweight_location()`](https://christiangoueguel.com/specProc/reference/biweight_location.md)
  : Biweight Location
- [`biweight_midvariance()`](https://christiangoueguel.com/specProc/reference/biweight_midvariance.md)
  : Biweight Midvariance
- [`biweight_midcovariance()`](https://christiangoueguel.com/specProc/reference/biweight_midcovariance.md)
  : Biweight Midcovariance
- [`biweight_midcorrelation()`](https://christiangoueguel.com/specProc/reference/biweight_midcorrelation.md)
  : Biweight Midcorrelation

### tail weight

Functions used to measure the tail weight of a distribution.

- [`quantile_weight()`](https://christiangoueguel.com/specProc/reference/quantile_weight.md)
  : Quantile Tail Weight Measure
- [`medcouple_weight()`](https://christiangoueguel.com/specProc/reference/medcouple_weight.md)
  : Medcouple Tail Weight Measure

### scale

Function used to compute highly robust estimators for scale (or
dispersion).

- [`umad()`](https://christiangoueguel.com/specProc/reference/umad.md) :
  Unbiased Median Absolute Deviation
- [`rousseeuw_croux()`](https://christiangoueguel.com/specProc/reference/rousseeuw_croux.md)
  : Rousseeuw-Croux Scale Estimators

## Special functions

Functions used for charcaterizing plasma emission in laser spectroscopy.

- [`tukey_gh()`](https://christiangoueguel.com/specProc/reference/tukey_gh.md)
  : Tukey g-and-h Parametric Distribution
- [`voigt_profile()`](https://christiangoueguel.com/specProc/reference/voigt_profile.md)
  : Voigt Function
- [`gaussian_profile()`](https://christiangoueguel.com/specProc/reference/gaussian_profile.md)
  : Gaussian Function
- [`lorentzian_profile()`](https://christiangoueguel.com/specProc/reference/lorentzian_profile.md)
  : Lorentzian Function
- [`pseudo_voigt_profile()`](https://christiangoueguel.com/specProc/reference/pseudo_voigt_profile.md)
  : Pseudo-Voigt Function

## Miscellaneous

- [`download_cache()`](https://christiangoueguel.com/specProc/reference/download_cache.md)
  [`clear_download_cache()`](https://christiangoueguel.com/specProc/reference/download_cache.md)
  : Persistent Cache of Downloaded Data
- [`tidy(`*`<specproc_peak_fit>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`glance(`*`<specproc_peak_fit>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`tidy(`*`<specproc_calibration>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`glance(`*`<specproc_calibration>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`tidy(`*`<specproc_wavelength_calibration>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`glance(`*`<specproc_wavelength_calibration>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`tidy(`*`<specproc_slope_bias>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`glance(`*`<specproc_slope_bias>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`tidy(`*`<specproc_boltzmann>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`glance(`*`<specproc_boltzmann>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`tidy(`*`<specproc_cflibs>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`glance(`*`<specproc_cflibs>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`tidy(`*`<specproc_uncertainty>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`glance(`*`<specproc_uncertainty>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`tidy(`*`<specproc_curve_of_growth>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  [`glance(`*`<specproc_curve_of_growth>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy.md)
  : Tidy and Glance Methods for Fits, Calibrations and Plasma
  Diagnostics
- [`tidy(`*`<specproc_robpca>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_robpca>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_rsimpls>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_rsimpls>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_rpcr>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_rpcr>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_lts>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_lts>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_rsimca>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_rsimca>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_robust_da>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_robust_da>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_opls>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_opls>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_som>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_som>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_wavelength_selection>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_wavelength_selection>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_nas>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_nas>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`tidy(`*`<specproc_domain>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  [`glance(`*`<specproc_domain>`*`)`](https://christiangoueguel.com/specProc/reference/specproc-tidy-models.md)
  : Tidy and Glance Methods for Chemometric Models
- [`correlation()`](https://christiangoueguel.com/specProc/reference/correlation.md)
  : Correlation Coefficients: Pearson, Spearman, Kendall, Chatterjee,
  and Biweight Midcorrelation
- [`summary_stats()`](https://christiangoueguel.com/specProc/reference/summary_stats.md)
  : Classical or Robust Descriptive Statistics

## Data

- [`forageLIBS`](https://christiangoueguel.com/specProc/reference/forageLIBS.md)
  : LIBS Spectra of Forage Samples
- [`forageShots`](https://christiangoueguel.com/specProc/reference/forageShots.md)
  : LIBS Laser Shots of Forage Samples
