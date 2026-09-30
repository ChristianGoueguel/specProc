# specProc (development version)

## Performance

* Column medians are computed in C++ (`reject_shots()`, `msc()` and
  `emsc()` with `robust = TRUE`, `center(method = "median")`, `robpca()`):
  `reject_shots()` is about 20 times faster on 400 spectra of 7152 channels.
* `q_residuals()` and `dmodx()` compute the residuals of the calibration
  samples from the scores of the components left out, without
  reconstructing the data (about 100 times faster), and
  `line_intensities()` measures all the spectra of a line at once (about 3
  times faster). The results are unchanged.

## Breaking changes

* The data sets are renamed: `specLIBS` is now `soilLIBS`, and `fourrage`
  is now `forageLIBS`. Replace `data(specLIBS)` with `data(soilLIBS)` and
  `data(fourrage)` with `data(forageLIBS)`.
* `macropca()` (and `step_macropca()`) now only centers the variables by
  default, instead of also scaling them as `cellWise::MacroPCA()` does:
  scaling spectra gives noise and continuum channels as much weight as
  emission lines. Use `macropca(x, scale = TRUE)` for the previous
  behavior.
* `plot_cell_map()` is redrawn with ggplot2 and patchwork instead of
  `cellWise::cellMap()`, for spectra: the map fills the plot, with a
  wavelength axis, cells combined into at most `resolution` blocks (within
  detector segments), flagged cells colored by the sign and size of their
  residual, a strip of the outlier type of each observation, rows
  optionally sorted by orthogonal distance (`order = "od"`), and a top
  panel of the share of flagged cells at each wavelength (`profile`). The
  arguments `nrowsinblock`, `ncolumnsinblock` and `...` are replaced by
  `resolution`. The profile shows the mean spectrum in grey (of the
  imputed data, or of `spectra`) and labels the most flagged regions, with
  the emission lines they match (`lines`), and `order = "cluster"` groups
  the observations with similar flagged cells.
* `flagged_regions()` lists the wavelength regions where many observations
  have cells flagged by `macropca()` (at least `threshold` of them), with
  their extent, share, direction and matching emission lines.

## New features

* `plot_embedding()` is drawn in the style of SIMCA score plots: grey
  outside the outermost ellipse (of T-squared, or of each group) and white
  inside, with no grid, a fixed `aspect_ratio` (0.7) and lines through the
  origin. The axis titles give the explained variance of the components of
  PCA fits, and the T-squared ellipses of the groups (`hotelling =
  "group"`) are filled with their color. `size` can be a variable (such as
  a concentration) that sets the size of the points, and `biplot = TRUE`
  draws the loadings of a PCA over the scores, with the `biplot_top` most
  important variables (emission lines, for spectra) as labeled arrows.
  With `hotelling = "all"`, the T-squared of a PCA model is that of the
  model (its eigenvalues, centered at 0): for a robust fit, the ellipses are
  no longer inflated or tilted by the outlying samples, and the flagged
  samples are the leverage points of `plot_outlier_map()`. Without groups,
  `ellipse = TRUE` with `hotelling = "all"` draws only the T-squared
  ellipse, with a warning, instead of two ellipses of all the samples. The
  samples beyond the T-squared limit are counted in the subtitle, circled
  in red only with `flag = TRUE`, and labeled only with `label` (`TRUE`, or
  a column or vector of labels), independently of each other.
* `plot_outlier_map()` takes a size per sample (`size`), and
  `colour_by = "distance"` colors its points by their reduced distance from
  the origin (the larger of SD and OD over their cut-offs), on a rainbow
  from dark red to blue; with `relative = TRUE`, where both cut-offs are at
  1, yellow marks the cut-offs, between the regular observations (dark red
  to orange) and the outlying ones (green to blue).
* The legends of `plot_embedding()` and `plot_outlier_map()` are more
  compact: smaller text and keys, three sizes, the T-squared levels written
  on their ellipses instead of in a legend, and no key for the kind of
  samples without new samples.
* `contributions()` computes the contributions of each variable to the Q
  residual or to Hotelling's T-squared of samples (the definitions of the
  PLS_Toolbox, whose squares add up to Q and T-squared), for a `prcomp()`
  fit or a robust fit (where they decompose the orthogonal and score
  distances), and relative contributions against reference samples
  (`reference`, or `"regular"` for all regular samples).
  `plot_contributions()` draws them against wavelength with the largest
  labeled and matched to emission lines.
* `plot_loadings()` plots the loadings of a PCA (`prcomp()`, `robpca()`,
  `rospca()` or `macropca()`) against wavelength, one panel per component,
  and labels the wavelengths that contribute most to each component, with
  the emission lines they match in a line list from `libs_lines()`. The
  mean spectrum can be drawn behind the loadings, `type = "contribution"`
  plots the variance of each wavelength explained by the components, and
  `interactive = TRUE` makes a plotly figure. `loading_peaks()` returns the
  labeled peaks as a table, with their candidate lines and a flag for
  derivative shapes (line shifts).
* `plot_outlier_map()` gains `relative`, to plot the reduced distances
  (divided by their cut-offs) so that maps of different models share one
  scale, `shade`, to shade the good leverage, orthogonal outlier and bad
  leverage regions in greys of increasing darkness and name them, and
  `log`, for logarithmic axes. The points are outlined in black, and
  `...` passes styling such as `alpha` or `size` to `geom_point()`.
* `dmodx()` computes the distance to the model in the space of the
  variables (DModX, as in SIMCA) of a PCA model, normalized or absolute,
  for the calibration samples or new ones. Its limits use the effective
  number of residual dimensions by default (`df = "effective"`), because
  SIMCA's degrees of freedom (`df = "simca"`) flag a large share of
  ordinary samples when spectra have far more channels than samples.
  `plot_influence()` draws DModX or Q against T-squared.
* `hotelling_t2()`, `q_residuals()`, `dmodx()`, `plot_influence()` and the
  T-squared ellipses of `plot_embedding()` take any confidence level(s)
  (`conf_level`, 0.975 by default; in `plot_embedding()`, one `conf_level`
  sets both its confidence and T-squared ellipses),
  and the Beta distribution of T-squared for the samples of the model
  (`method = "beta"`). `hotelling_t2()` and the T-squared ellipses of
  `plot_embedding()` now use the HotellingEllipse package (1.3.0 or later).
* `wavelength_calibration()` fits a correction of the wavelength axis from
  reference lines (for example, from the NIST database), located to a
  fraction of a channel by Gaussian interpolation, with one correction per
  detector segment (constant, linear or quadratic) and automatic rejection
  of edge, weak, saturated and outlying lines. `apply_calibration()`
  relabels the wavelengths of spectra without changing their intensities,
  `predict()` corrects any wavelength, and `plot_wavelength_calibration()`
  draws the offsets and the fit.
* `savitzky_golay()` also splits the axis where overlapping detectors make
  the wavelengths step back.
* The periodic table of `line_finder()` can be hidden (button **Hide
  table**, or `show_table = FALSE` at start) to give the spectrum the whole
  height of the window; the selected elements stay listed in the panel
  header.
* `savitzky_golay()` and `step_savgol()`: Savitzky-Golay smoothing and first
  or second derivatives, keeping every channel (polynomial fits at the
  edges) and filtering the segments between detector gaps separately. The
  window, polynomial order and derivative are tunable (new dials parameter
  `savgol_derivative()`).
* `normalize()` gains the L1 (`"l1"`), L2 or vector (`"l2"`) and maximum
  (`"max"`) norms, and `step_spectral_norm()` normalizes each spectrum in a
  recipe by its L1 norm, total area, L2 norm or maximum, with the method
  tunable (new dials parameter `spectral_norm_method()`).
* `wavelet_features()` and `step_wavelet()`: discrete wavelet transform of
  spectra (Haar, Daubechies d4/d6/d8 and least asymmetric la8 wavelets) as
  compressed features: the approximation at a level or all coefficients,
  optionally the coefficients of largest variance in the training data. The
  level and number of coefficients are tunable (new dials parameter
  `wavelet_level()`).
* `q_residuals()` computes Hotelling's T-squared and the Q residual (SPE) of
  each sample for a PCA model, for the calibration samples or new ones,
  with their 95% and 99% limits (Jackson-Mudholkar or Box for Q), and
  classifies the samples as regular, extreme, residual or both.
  `plot_influence()` draws Q against T-squared with the limits.
* `calibration_curve()` gains prediction intervals: `plot_calibration()`
  draws the confidence band, the prediction band or both (`interval`), and
  shows new samples (`newdata`) at their predicted concentrations with
  their intervals; `predict(type = "signal")` gives the expected signal at
  given concentrations with a confidence or prediction interval; the
  coefficients have confidence intervals.
* The plots of `correlation()` are redesigned. For spectra (variables named
  by wavelength), the plot is a correlation spectrum with the 5%
  significance thresholds; otherwise, a sorted chart colored by sign and
  labeled with the values, with `top` to show only the strongest
  correlations. The interactive versions are built directly with plotly
  (WebGL for spectra) instead of converted from ggplot2. `color` now takes
  one color or two (positive and negative correlations).

* `plot_embedding()` draws two-dimensional embeddings (UMAP from
  `embed::step_umap()`, principal components from `recipes::step_pca()`,
  `prcomp()` or `robpca()`), colored by a variable, with optional
  confidence ellipses per group (classical or robust, normal or Hotelling
  quantiles) from the ConfidenceEllipse package, and Hotelling's T-squared
  95% and 99% ellipses with the outlying samples labeled (`hotelling`, for
  all samples or within groups, on `k` components). The preprocessing
  vignette uses it to compare PCA and UMAP maps of the samples.
* `hotelling_t2()` computes Hotelling's T-squared of each sample on `k`
  components of an embedding, with its 95% and 99% limits, for all samples
  or within groups.

## Bug fixes

* `macropca()` failed when `k` was not given: `cellWise::MacroPCA()` with
  `k = 0` only reports the explained variance. `k` is now chosen from the
  new arguments `var_explained` (0.8) and `kmax` (10), as in `robpca()`, and
  MacroPCA's message and scree plot are no longer shown.

# specProc 0.6.0

## New features

* `cf_libs()`: calibration-free LIBS quantification (Ciucci et al., 1999).
  The temperature is estimated from the common slope of parallel Boltzmann
  plots (one per species) or, when the electron density is known,
  Saha-Boltzmann plots (one per element, much more precise). The densities
  follow from the intercepts and the partition functions, and the
  ionization stages not observed from the Saha equation. The composition
  (atomic and mass fractions) is normalized by closure or by the known
  concentration of one element. `plot_boltzmann()` draws the plots of the
  fit. The plasma diagnostics vignette applies it to the forage samples and
  compares it with their laboratory values.
* `nist_levels()` retrieves energy levels from the NIST Atomic Spectra
  Database, and `partition_function()` computes partition functions from
  them, optionally truncated at a maximum energy (such as the lowered
  ionization energy).
* `reject_shots()` flags the outlying laser shots of each sample (total
  intensity, correlation with or distance to the median spectrum, by robust
  z-scores), before they are averaged with `average()`.
* `line_intensities()` measures the area (or height, or Voigt-fitted area)
  of emission lines in spectra, searching each peak near its tabulated
  wavelength, with a signal-to-noise ratio and saturation check. It keeps
  the columns of a table of lines, so its result feeds `boltzmann_plot()`,
  `cf_libs()` and `calibration_curve()`. `step_line_intensities()` does the
  same in a recipe.
* `correct_self_absorption()` corrects line intensities for
  self-absorption by the internal reference method of Sun and Yu (2009),
  with the temperature given or estimated from a Saha-Boltzmann plot.
* `calibration_curve()` fits univariate (linear or quadratic, optionally
  weighted) calibration curves, with the sensitivity, LOD and LOQ, Mandel's
  and lack-of-fit tests of linearity, inverse prediction of concentrations
  with confidence intervals (`predict()`) and `plot_calibration()`.
* New recipe steps: `step_reject_shots()` removes the outlying shots from
  the training data (skipped on new data), and `step_line_ratio()`
  normalizes each spectrum to the area or height of a reference line
  (internal standard).
* `line_finder()`: a Shiny app to identify the emission lines of LIBS
  spectra. Elements are selected on a periodic table, and the strongest lines
  of their ionization stages, from the NIST Atomic Spectra Database, are
  overlaid on a spectrum in an interactive plotly graph (one color per
  stage), with controls for the temperature, wavelength range, number of
  lines, wavelength shift and spectrum (mean, group mean or single spectrum).
  Fetched lines can be saved and reloaded for offline use.
* `libs_lines()` lists the lines of selected species expected to be the
  strongest in a plasma at a given temperature (relative intensities in
  LTE), and `plot_lines()` overlays them on a spectrum (plotly or ggplot2).

# specProc 0.5.0

This release adds plasma diagnostics for LIBS: electron density from Stark
broadening, excitation temperature from Boltzmann and Saha-Boltzmann plots,
and checks of LTE, self-absorption and detector saturation, with atomic data
from the NIST Atomic Spectra Database and Stark parameters from STARK-B.

## New features

* Plasma diagnostics for LIBS:
  - `starkb_lines()` retrieves Stark widths and shifts of the lines of an
    atom or ion from the STARK-B database (Sahal-Bréchot, Dimitrijević and
    Moreau) through its VAMDC service, on demand; `read_starkb()` reads
    STARK-B data saved as XSAMS files, for offline and reproducible work;
    `stark_table()` builds the same table from user-supplied widths or from
    fitted temperature laws. `stark_width()` interpolates the width at a
    given temperature and electron density, and scales multiplet data to a
    line of the multiplet (lambda-squared rule).
  - `electron_density()` estimates the electron density from the Stark
    (Lorentzian) width of a line, with STARK-B data or a reference width, or
    from the H-alpha line (Gigosos et al., 2003).
  - `boltzmann_plot()` and `saha_boltzmann_plot()` estimate the excitation
    temperature from line intensities and atomic data, and
    `plot_boltzmann()` draws the plots.
  - `mcwhirter_criterion()` checks the McWhirter criterion for LTE, and
    `self_absorption()` computes self-absorption coefficients from line
    widths (El Sherbini et al., 2005).
  - `saturation_summary()` finds channels at the saturation limit of the
    detector.
  - `nist_lines()` and `nist_ionization_energy()` retrieve transition
    probabilities, level energies, statistical weights and ionization
    energies from the NIST Atomic Spectra Database, on demand.
* `voigt_fwhm()` computes the full width at half maximum of a Voigt profile
  (Olivero and Longbothum, 1977).
* New vignette "Plasma diagnostics: electron density, temperature and
  self-absorption", on `specLIBS` and `fourrage` spectra.

# specProc 0.4.0

This release adds robust PCA, a proper net analyte signal with figures of
merit, and rewrites the documentation around tidymodels.

## Breaking changes

* `nas()` now computes the net analyte signal (Lorber, Faber and Kowalski,
  1997; Faber, 1998) of an inverse PLS or PCR calibration model, with its
  figures of merit: sensitivity, selectivity and, given the noise level,
  analytical sensitivity, limits of detection and quantification, and
  signal-to-noise ratios. A `predict()` method gives the NAS, selectivity and
  predicted concentration of new samples. Previously, `nas()` returned
  spectra filtered by direct orthogonalization, identical to
  `direct_orthogonal()`, which remains available for that purpose.

## New features

* Robust PCA: `robpca()` (ROBPCA, Hubert, Rousseeuw and Vanden Branden,
  2005) and `rospca()` (robust sparse PCA, Hubert, Reynkens, Schmitt and
  Verdonck, 2016), implemented from the published algorithms with C++
  kernels for the Stahel-Donoho outlyingness, FAST-MCD and the grid-search
  sparse PCA; and `macropca()`, a wrapper around `cellWise::MacroPCA()` for
  cellwise outliers and missing values. All three return score and
  orthogonal distances with cut-offs, an outlier classification, and a
  `predict()` method for new observations.
* `plot_outlier_map()` draws the outlier map (score distance against
  orthogonal distance) of any of the three, optionally with new
  observations; `plot_cell_map()` draws the cell map of a MacroPCA fit.
* New recipe steps `step_robpca()`, `step_rospca()` and `step_macropca()`,
  robust counterparts of `recipes::step_pca()` that can also add the score
  and orthogonal distances as columns, and `step_robust_bcyj()`, a robust
  counterpart of `recipes::step_BoxCox()` and `recipes::step_YeoJohnson()`.

## Documentation

* The four vignettes and the README use recipe steps, tidymodels
  (workflows, tune, rsample) and the tidyverse. Preprocessing pipelines are
  written as recipes; the calibration vignette runs its repeated nested
  cross-validation with `rsample::nested_cv()` and `tune::tune_grid()`, and
  the orthogonalization vignette tunes every filter together with the PLS
  model. Vignettes that need suggested packages stop with a message when
  those are not installed.

# specProc 0.3.0

This release connects specProc to the tidymodels framework. Preprocessing and
orthogonalization can now be estimated on calibration data and applied to new
spectra, either directly with `predict()` or as recipe steps that are
re-estimated on every resample and tuned together with the model.

## New features

* The orthogonalization filters can now be applied to new spectra:
  `epo()`, `osc()`, `direct_orthogonal()`, `direct_osc()`, `projected_osc()`
  and `o2pls()` return classed objects with a `predict()` method, which
  corrects new data with the centers, scales and components estimated on the
  calibration data. Existing fields are unchanged.
* New recipe steps for tidymodels: `step_epo()`, `step_glsw()`, `step_osc()`,
  `step_direct_orthogonal()`, `step_direct_osc()`, `step_projected_osc()` and
  `step_y_gradient_glsw()`, with `tidy()` and `tunable()` methods, and the
  dials parameter `glsw_alpha()`. In a workflow, the filters are re-estimated
  on every resample and can be tuned with the model. The GLSW steps store the
  filter in factored form instead of a p x p matrix. recipes and dials are
  suggested, not required.
* New `emsc()`: extended multiplicative signal correction (Martens and Stark,
  1991), with a polynomial baseline and optional interferent spectra, and a
  `predict()` method for new spectra. With `degree = 0` it equals `msc()`.
* New recipe steps for spectral preprocessing: `step_baseline()` (arPLS, ALS
  or LSP), `step_snv()`, `step_msc()`, `step_emsc()`, `step_pareto_scale()`
  and `step_poisson_scale()`. MSC and EMSC estimate the reference spectrum,
  and the scaling steps the column scales, on the training data only. The
  baseline `lambda` or `degree` and the EMSC `degree` are tunable; the new
  dials parameter `baseline_lambda()` covers `lambda`.
* New data set `fourrage`: LIBS spectra of 365 forage samples with reference
  contents of 12 elements, and a vignette based on it, "Removing unwanted
  variation: a comparison of orthogonalization methods" (what EPO, GLSW, the
  OSC family, DO/NAS, DOSC, POSC/OPLS and y-gradient GLSW remove, whether it
  improves potassium predictions, and which methods are equivalent).

# specProc 0.2.0

This release fixes a large number of bugs so that every exported function now
runs and returns numerically correct results. It adds a test suite (400+
expectations), and several results are checked against independent reference
implementations.

## Breaking changes

* Functions were renamed to a consistent snake_case scheme. The old names still
  work but give a deprecation warning (see `?"specProc-deprecated"`):
  `whittaker()` -> `baseline_als()`, `lorentzian()` -> `lorentzian_profile()`,
  `pseudo_voigt()` -> `pseudo_voigt_profile()`, `peakfit()` -> `peak_fit()`,
  `multipeakfit()` -> `multipeak_fit()`, `plotfit()` -> `plot_fit()`,
  `plotSpec()` -> `plot_spectra()`, `outlierplot()` -> `plot_outliers()`,
  `directOutlyingness()` -> `directional_outlyingness()`,
  `iqrMethod()` -> `iqr_outliers()`, `robustBCYJ()` -> `robust_bcyj()`,
  `rousseeuwCroux()` -> `rousseeuw_croux()`, `summaryStats()` -> `summary_stats()`,
  `tukeyGH()` -> `tukey_gh()`, `yGradientglsw()` -> `y_gradient_glsw()`,
  `pareto()` -> `pareto_scale()`.
* `gaussian()` is renamed to `gaussian_profile()` **without** an alias, because
  exporting `gaussian()` masked `stats::gaussian()`, which breaks
  `glm(family = gaussian)` when specProc is loaded.

* `baseline_arpls()`, `baseline_lsp()` and `whittaker()` return tibbles (they
  previously returned plain lists of columns). They no longer replace negative
  corrected values with 1.
* `average()` with `.group_by` now returns the group labels as the first column.
* `biweight_location()` and `biweight_scale()` use the unscaled MAD, following
  the standard definition (Beers et al., 1990), so that `biweight_scale()^2`
  equals `biweight_midvariance()`. `biweight_scale()` loses its unused `tol`
  and `max_iter` arguments. `loc = NULL` now means "median of `x`".
* `direct_orthogonal()` loses its unused `max_iter` and `tol` arguments, and
  `direct_osc()` loses `max_iter`. `tol` now sets the pseudo-inverse tolerance.
* `epo()` gains a `clutter` argument (the matrix describing the external
  variation). Its default `ncomp` is now 2, as documented. It now removes the
  *dominant* singular directions; before, it removed the smallest ones.
* `o2pls()` has a new interface: `o2pls(x, y, ncomp, nx, ny, center, scale)`.
* `pds()` returns `transfer_matrix` (was `transfert_matrix`), `alpha` now blends
  the local and global models, and progress is no longer printed.
* `projected_osc()` always returns the corrected training data. New data are
  preprocessed with the training centers and scales.
* `quantile_weight()` uses whole-distribution quantiles, as in Brys et al.
  (2006), and defaults to `p = 0.125`, `q = 0.875`.
* `summaryStats(robust = TRUE)` drops the `rsd` column. It applied the 1.4826
  consistency factor twice, and `mad` already holds the correct value.
* `opls()` now requires the Bioconductor package ropls to be installed
  (it moved to Suggests). It no longer prints output or draws plots.
* R >= 4.1.0 is required.

## Bug fixes

* Fixed crashes in `baseline_arpls()`, `whittaker()`, `baseline_lsp()` (data
  frames), `correlation(method = "bicor")`, `directOutlyingness(maxRatio = )`,
  `direct_osc()`, `msc()`, `multipeakfit()`, `nas()`, `o2pls()`, `osc()`
  (`"wold"`, `"fearn"`), `pds()`, `peakfit()`, `projected_osc()` and
  `yGradientglsw()`.
* `glsw()` and `yGradientglsw()` squared the eigenvalues of the covariance
  matrix when building the filter.
* `msc()` regressed each wavelength instead of each spectrum on the reference.
* `generalized_boxplot()` transformed the ranks instead of the data. This made
  the g-and-h estimates meaningless. It also failed when the two tails had
  different numbers of outliers.
* `pseudo_voigt()` counted the area and the offset twice.
* `biweight_scale()` used a wrong exponent in the denominator. It also ignored
  `reduced = TRUE`.
* `biweight_location()` did not iterate.
* `average()` did not ignore missing values. It also read out of bounds for
  factors with unused levels or missing group values.
* `normalize(method = "background")` rejected data frames for `bkg`.
* `plotSpec()` connected all spectra into a single line when `id` was not given.
* `outlierplot()` no longer changes the user's random number generator state.
* `iqrMethod()` no longer changes global options. Missing values no longer
  cause errors.

## New features

* Three vignettes based on `specLIBS`: "Preprocessing LIBS spectra"
  (baseline, normalization judged by replicate RSD and intraclass
  correlation, robust screening of shots), "Fitting emission lines"
  (profile choice, sources of uncertainty, identifiability) and
  "Predicting soil clay content from LIBS spectra" (a compositional
  log-ratio PLS model with nested, repeated cross-validation by sample, and
  the optimism of common shortcuts).
* `plot_fit()` draws the fitted profiles on a fine wavelength grid.
* Functions taking a response now accept 1-d arrays, such as the output of
  `tapply()`.

* New `voigt_profile()`: the exact Voigt profile, computed in C++ from the
  Faddeeva function with Weideman's (1994) rational approximation (relative
  error below 1e-10). `peak_fit()` and `multipeak_fit()` now use it for
  `profile = "voigt"`; the pseudo-Voigt approximation is available as
  `profile = "pseudo_voigt"`.
* New data set `specLIBS`: 400 LIBS spectra (50 soil samples x 8 locations,
  7152 channels, 199-822 nm) with the clay, sand and silt content of each
  sample.
* `generalized_boxplot()` whiskers now end at the most extreme observations
  within the fences, as in standard boxplots. The fences are returned as
  `lower_fence` and `upper_fence`.
* The README has been rewritten around a complete worked example on
  `specLIBS`.

## Improvements

* Asymmetric least squares and arPLS baselines are solved in C++ with a banded
  Cholesky factorization. The cost is O(n) per spectrum, so they scale to
  spectra with tens of thousands of channels.
* EPO and GLSW use Eigen's divide-and-conquer SVD and never form the p x p
  covariance or projection matrices.
* `peakfit()` and `multipeakfit()` estimate starting values from the data when
  `wL`, `wG` or `A` are not given. `multipeakfit()` fits all lines
  simultaneously and returns each line's contribution. A spectrum that fails
  to fit produces a warning instead of stopping the whole batch.
* The orthogonalization functions return the centers and scales, so the same
  preprocessing can be applied to new data.
* Fewer dependencies: corrr, cowplot, forcats, mt, broom and ropls are no longer
  imported.

# specProc 0.1.0

* Added a `NEWS.md` file to track changes to the package.
