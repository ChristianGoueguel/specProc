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
