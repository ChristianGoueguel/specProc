# Changelog

## specProc 0.2.0

This release fixes a large number of bugs so that every exported
function now runs and returns numerically correct results. It adds a
test suite (400+ expectations), and several results are checked against
independent reference implementations.

### Breaking changes

- [`baseline_arpls()`](https://christiangoueguel.com/specProc/reference/baseline_arpls.md),
  [`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md)
  and
  [`whittaker()`](https://christiangoueguel.com/specProc/reference/whittaker.md)
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
  [`whittaker()`](https://christiangoueguel.com/specProc/reference/whittaker.md),
  [`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md)
  (data frames), `correlation(method = "bicor")`,
  `directOutlyingness(maxRatio = )`,
  [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md),
  [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md),
  [`multipeakfit()`](https://christiangoueguel.com/specProc/reference/multipeakfit.md),
  [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md),
  [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md),
  [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md)
  (`"wold"`, `"fearn"`),
  [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md),
  [`peakfit()`](https://christiangoueguel.com/specProc/reference/peakfit.md),
  [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
  and
  [`yGradientglsw()`](https://christiangoueguel.com/specProc/reference/yGradientglsw.md).
- [`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md)
  and
  [`yGradientglsw()`](https://christiangoueguel.com/specProc/reference/yGradientglsw.md)
  squared the eigenvalues of the covariance matrix when building the
  filter.
- [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md)
  regressed each wavelength instead of each spectrum on the reference.
- [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  transformed the ranks instead of the data. This made the g-and-h
  estimates meaningless. It also failed when the two tails had different
  numbers of outliers.
- [`pseudo_voigt()`](https://christiangoueguel.com/specProc/reference/pseudo_voigt.md)
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
- [`plotSpec()`](https://christiangoueguel.com/specProc/reference/plotSpec.md)
  connected all spectra into a single line when `id` was not given.
- [`outlierplot()`](https://christiangoueguel.com/specProc/reference/outlierplot.md)
  no longer changes the user’s random number generator state.
- [`iqrMethod()`](https://christiangoueguel.com/specProc/reference/iqrMethod.md)
  no longer changes global options. Missing values no longer cause
  errors.

### Improvements

- Asymmetric least squares and arPLS baselines are solved in C++ with a
  banded Cholesky factorization. The cost is O(n) per spectrum, so they
  scale to spectra with tens of thousands of channels.
- EPO and GLSW use Eigen’s divide-and-conquer SVD and never form the p x
  p covariance or projection matrices.
- [`peakfit()`](https://christiangoueguel.com/specProc/reference/peakfit.md)
  and
  [`multipeakfit()`](https://christiangoueguel.com/specProc/reference/multipeakfit.md)
  estimate starting values from the data when `wL`, `wG` or `A` are not
  given.
  [`multipeakfit()`](https://christiangoueguel.com/specProc/reference/multipeakfit.md)
  fits all lines simultaneously and returns each line’s contribution. A
  spectrum that fails to fit produces a warning instead of stopping the
  whole batch.
- The orthogonalization functions return the centers and scales, so the
  same preprocessing can be applied to new data.
- Fewer dependencies: corrr, cowplot, forcats, mt, broom and ropls are
  no longer imported.

## specProc 0.1.0

- Added a `NEWS.md` file to track changes to the package.
