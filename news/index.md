# Changelog

## specProc (development version)

### New features

- [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md):
  robust PLS regression by the RSIMPLS algorithm of Hubert and Vanden
  Branden (2003), the SIMPLS algorithm computed from a ROBPCA covariance
  matrix of the spectra and the responses, followed by a robust
  regression on the scores. Outlying spectra and wrong reference values
  have little influence on the model. One or several responses; the
  response is block-scaled before ROBPCA so that wrong reference values
  are detected whatever its units. The fit gives the score, residual and
  orthogonal distances of each observation, and the robust R² of the
  model. [`predict()`](https://rdrr.io/r/stats/predict.html) gives the
  predictions or scores of new spectra, with their score and orthogonal
  distances, and
  [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
  draws the regression outlier map (vertical outliers, good and bad
  leverage points) or, with `map = "score"`, the score outlier map of
  the spectra.
  [`step_rsimpls()`](https://christiangoueguel.com/specProc/reference/step_rsimpls.md)
  is its recipe step, the robust counterpart of
  [`recipes::step_pls()`](https://recipes.tidymodels.org/reference/step_pls.html):
  it replaces the spectra by their robust PLS scores, and optionally
  their score and orthogonal distances.
- The `"rsimpls"` engine of
  [`parsnip::pls()`](https://parsnip.tidymodels.org/reference/pls.html)
  fits
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
  models in tidymodels, with one or several outcomes; the other
  arguments of
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
  are engine arguments (`set_engine("rsimpls", kmax = , alpha = )`).
  [`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.html)
  gives the predictions of the models with fewer components from the
  same fit, so
  [`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
  fits one model per resample for a grid of `num_comp`.
  [`predict()`](https://rdrr.io/r/stats/predict.html) of an
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
  model gains `ncomp`, for the models with 1 to `kmax` components, which
  the fit now keeps (`models`).
- [`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md):
  robust principal component regression (RPCR) of Hubert and Verboven
  (2003), the robust PCA of the spectra
  ([`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md))
  followed by the LTS regression of the response on the scores, or the
  MCD regression of several responses. It has the outputs,
  [`predict()`](https://rdrr.io/r/stats/predict.html) method and outlier
  maps of
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md).
- [`robust_rmsecv()`](https://christiangoueguel.com/specProc/reference/robust_rmsecv.md):
  the robust RMSECV of
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
  or
  [`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md)
  models with 1 to `kmax` components, which leaves the outliers out of
  the cross-validated error, and the robust component selection (RCS)
  criterion of Engelen and Hubert (2005), to choose the number of
  components. The folds are fitted in parallel when a
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is set.
- The `"lts"` engine of
  [`parsnip::linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)
  fits the reweighted LTS regression
  ([`lts_fit()`](https://christiangoueguel.com/specProc/reference/linear_reg_lts.md),
  from
  [`robustbase::ltsReg()`](https://rdrr.io/pkg/robustbase/man/ltsReg.html));
  after
  [`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)
  in a workflow, it gives robust PCR in tidymodels.
- [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md):
  robust SIMCA classification of Vanden Branden and Hubert (2005), a
  robust PCA model of each class and the assignment to the closest class
  from the scaled score and orthogonal distances, for whole spectra.
  [`predict()`](https://rdrr.io/r/stats/predict.html) gives the classes,
  or the distances to the classes and whether a spectrum is outlying for
  all of them, and
  [`plot_coomans()`](https://christiangoueguel.com/specProc/reference/plot_coomans.md)
  draws the distances to two classes against each other (Coomans plot),
  with the class and classification boundaries.
  [`simca()`](https://christiangoueguel.com/specProc/reference/simca.md)
  is a new parsnip model of SIMCA classification, with
  [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)
  as its `"rsimca"` engine, to fit and tune it in tidymodels:
  `num_comp`, and the rule of classification, `gamma` and `squared`,
  with the new dials parameters
  [`simca_gamma()`](https://christiangoueguel.com/specProc/reference/simca_gamma.md)
  and
  [`simca_squared()`](https://christiangoueguel.com/specProc/reference/simca_gamma.md).
- [`robust_da()`](https://christiangoueguel.com/specProc/reference/robust_da.md):
  robust linear and quadratic discriminant analysis of Hubert and Van
  Driessen (2004), from the MCD estimates of the classes, for a few
  variables such as line intensities or robust principal component
  scores. [`predict()`](https://rdrr.io/r/stats/predict.html) gives the
  classes or posterior probabilities. It is the `"mcd"` engine of
  [`parsnip::discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
  and
  [`parsnip::discrim_quad()`](https://parsnip.tidymodels.org/reference/discrim_quad.html).
- [`select_wavelengths()`](https://christiangoueguel.com/specProc/reference/select_wavelengths.md):
  selection of the informative wavelengths for PLS regression, by the
  variable importance in projection (VIP), the selectivity ratio (SR),
  backward elimination on either, or forward interval PLS (iPLS). With
  `robust = TRUE`, the selection is made from an
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
  model, without the regression outliers. The candidate intervals of
  iPLS are evaluated in parallel when a
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is set.
  [`plot_wavelength_selection()`](https://christiangoueguel.com/specProc/reference/plot_wavelength_selection.md)
  shows the selected regions on the mean spectrum with the importance of
  each variable.
- [`plot_shots()`](https://christiangoueguel.com/specProc/reference/plot_shots.md):
  views of the laser shots and of their rejection by
  [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md),
  to judge the rejection before averaging the shots: a map of the
  samples by the shot number, colored by the robust z-scores in classes
  up to the cutoff, the rejected shots outlined (`"heatmap"`, the
  default; `acquisition` orders the samples as measured,
  `arrange = "extreme"` puts the most extreme first, and `smooth` gives
  running medians along the acquisition, which show the drift of the
  shots during a session); the robust z-scores of the shots with the
  cutoff (`"criteria"`); the shots of a sample with the rejected ones in
  color (`"spectra"`); the intensity, shape and rejected share along the
  shot number, which shows cleaning shots and the drift of the ablation
  (`"order"`); and the rejected shots per sample with the RSD of the
  samples before and after the rejection (`"samples"`). `plot = FALSE`
  returns the table of each view.
- `forageShots`: the single laser shots (8 per measurement, in firing
  order) of 20 of the measurements of `forageLIBS`, in the windows
  380-430 nm and 760-780 nm, to study the shot-to-shot variability.
- [`step_select_wavelengths()`](https://christiangoueguel.com/specProc/reference/step_select_wavelengths.md):
  the selection as a recipe step, repeated on every resample;
  `num_terms`, `num_intervals` (new dials parameter
  [`num_intervals()`](https://christiangoueguel.com/specProc/reference/num_intervals.md))
  and `num_comp` can be tuned.

### Improvements

- The labels of the plots no longer overlap: they are placed by ggrepel
  (now imported), away from each other and from the points, with a
  segment to their point when moved far. This applies to the outlying
  samples of
  [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md),
  [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md),
  [`plot_coomans()`](https://christiangoueguel.com/specProc/reference/plot_coomans.md),
  [`plot_outliers()`](https://christiangoueguel.com/specProc/reference/plot_outliers.md)
  and
  [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  (and its biplot loadings), the peaks of
  [`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md),
  [`plot_contributions()`](https://christiangoueguel.com/specProc/reference/plot_contributions.md),
  [`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)
  and
  [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md),
  the outliers of
  [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md)
  and
  [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  (where many crowd together, some are left unlabeled), the rejected
  shots of
  [`plot_shots()`](https://christiangoueguel.com/specProc/reference/plot_shots.md),
  the spectra of `plot_spectra(label_spectra = TRUE)`, and the lines of
  [`plot_wavelength_calibration()`](https://christiangoueguel.com/specProc/reference/plot_wavelength_calibration.md),
  which are now all labeled.
- [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
  and
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  are faster on wide data (up to about 4 times on full LIBS spectra):
  the reduction to the subspace spanned by the observations is now
  computed in C++ from the smaller cross-product matrix instead of an
  SVD. The results are unchanged up to numerical precision.
- [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md)
  has new arguments: `panel`, a grouping variable that splits the
  spectra into panels, arranged one above the other or side by side
  (`layout = "vertical"` or `"horizontal"`), with the `offset`
  restarting in each panel; `grid`, to draw grid lines; and
  `color_as = "fill"`, to show the colors of `colvar` (or `id`) as the
  fill of the area under each spectrum instead of the line color.
  Spectra stacked with a vertical offset are then filled with opaque
  colors, the front ones hiding the back ones, as in a waterfall plot.
- [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md)
  makes figures ready for publication:
  - `legend` shows the legend of `colvar` (a color bar) or `id`, on any
    side or inside the plot, titled by `legend_title` (such as
    `"K (%)"`);
  - `xlab`, `ylab` and `title` set the axis titles and the plot title
    (bold, split into a title and a subtitle when long), and the
    intensities are written in plain notation (“100,000” rather than
    “1e+05”);
  - `lines`, a line list from
    [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md),
    marks the emission lines with dashed lines, labeled with their
    species and wavelength above the plot (close lines share a label);
  - `palette` sets the colors: a viridis palette (`"viridis"`, the
    default, `"magma"`, `"cividis"`, …) or any colors;
  - `base_size` and `linewidth` size the text and the lines for the
    printed figure (such as 8 points and 0.3 for a journal column);
  - `xlim` limits the wavelength range, and `scales = "free_y"` fits the
    intensity axis to each panel;
  - `panel_tags` tags the panels (a), (b), (c), …;
  - `label_spectra` labels each spectrum at its right end, with its `id`
    or value of `colvar`;
  - `summary = "mean"` or `"median"` draws the mean spectrum of each
    group (panel and `id`) with a band of one standard deviation, or the
    median with the quartiles;
  - `rasterize` draws the spectra as an image within an otherwise vector
    plot, for small PDF files of many long spectra (needs the ragg
    package).

  The help page gives the sizes for journal figures. The colors of
  `colvar` now go from dark purple to yellow-green (viridis), instead of
  from blue to red, readable by colorblind readers and in grayscale; use
  `palette = c("blue", "red")` for the former colors. The axis lines are
  slightly thinner.
- [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md)
  and
  [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  make figures ready for publication:
  - each variable is drawn in its own panel, with its own value axis
    (`scales = "free_y"`, the default), so that variables of different
    scales stay readable; `scales = "fixed"` keeps a common axis;
  - `group` draws the boxes of groups (sites, treatments, …) side by
    side, and `id` names the observations: the `outliers` table gives
    the `row` and `id` of every outlying value, and `label_outliers`
    labels them;
  - the number of values is written below each box (`show_n`), and
    `show_mean` marks the means;
  - `points = "all"` draws every observation (`"outliers"` by default,
    spread sideways so that equal values do not hide each other, or
    `"none"`);
  - a caption names the boxplot and its whiskers, which readers would
    take for Tukey’s otherwise (`caption`); the help pages give a figure
    legend;
  - `xlab`, `ylab`, `title` (bold), `base_size`, `horizontal`, `log` (a
    logarithmic axis) and `fill` (one color, or one per group or
    variable);
  - the `stats` table gives `n`, the fences, the notch limits, the
    `mean` and `n_outliers` of each variable (and group);
  - `annotate` writes statistics with each box: the shape measured by
    the method (medcouple, or g and h), the number of flagged values
    with the number expected in clean data, the median with its
    distribution-free 95% confidence interval, the IQR and the robust
    coefficient of variation; it draws the fences and the biweight
    location, gives the p-value of a Kruskal-Wallis test between the
    groups, and the number of missing values. The `stats` table gains
    these values (`n_missing`, `median_lower`, `median_upper`, `iqr`,
    `rcv`, `biweight`, `expected_outliers`), and a `tests` table gives
    the tests.
- [`plot_outliers()`](https://christiangoueguel.com/specProc/reference/plot_outliers.md)
  makes figures ready for publication: readable variable names, a
  “Robust z-score” axis, outliers in red among grey samples (“Regular”
  and “Outlier”), dashed univariate limits (z = -2.5 and 2.5), and a
  caption saying how the outliers are flagged and how to read the plot.
  `type = "distance"` draws the robust distance of each sample with the
  cutoffs. `id` names the samples, in the table and with
  `label_outliers`; `color_by` (`"outlier"`, `"distance"` or `"both"`),
  `plot = FALSE` (the table: row, id, distance, cutoff, outlier, weight
  and robust z-scores), `title`, `ylab` and `base_size`. Rows with
  missing values are left out, with a message, instead of stopping.
- [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md)
  gives, above each panel, the center, FWHM and area of each peak with
  their standard errors (rounded to two significant digits of them), and
  R^2 and RMSE. The FWHM of a Voigt profile combines its widths (Olivero
  and Longbothum, 1977), with a standard error from the covariance of
  the estimates. `show_fwhm` draws the FWHM at half maximum; the
  baseline is dotted, the peaks of a multi-peak fit are numbered, the
  residuals have dashed limits at plus or minus twice the RMSE, and a
  caption names the profiles. The data are small open circles, with a
  legend; `xlab`, `ylab`, `base_size`, `ncol` (several spectra: a pair
  of panels each) and `caption` are added.
- [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  makes figures ready for publication: plain axis numbers (“-100,000”
  rather than “-1e+05”); `base_size`, which scales the text, points,
  lines and labels; a caption saying what the ellipses and limits are;
  `legend_title`; a point shape per group (`shapes`), for grayscale
  printing; `palette`; `aspect_ratio = "equal"`; `legend` (position) and
  `panel = "white"`. The labels of the flagged samples alternate above
  and below the points, and the short loadings of a biplot are drawn
  without labels, which overlapped near the origin.
- [`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md)
  and
  [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  give the limit of a new sample, `method = "new"` (the F limit times
  (n + 1) / n). The docs no longer call the F limit the limit of a new
  sample.
- The axis titles of
  [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md)
  and
  [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md)
  give the units in parentheses, as in the other plots: “Wavelength
  (nm)” and “Intensity (arb. units)”.
- specProc now requires ggplot2 3.5.0 or later.

### Breaking changes

- [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  flags fewer observations: its default detection rate `alpha` is now
  about 0.7% (`2 * pnorm(-4 * qnorm(0.75))`), that of Tukey’s boxplot
  for normal data, as in the authors’ implementation (the Stata command
  `robbox`), instead of 5%. With 5%, about 18 of the 368 `forageLIBS`
  samples were flagged per mineral even in clean data; use
  `alpha = 0.05` for the former fences (see also the bug fixes).
- [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md)
  and
  [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  draw one panel per variable, boxes in light grey (the colors of each
  variable added nothing, and depended on whether ggsci was installed)
  and horizontal labels. `scales = "fixed"` and `fill` give the former
  layout and colors.
- The arguments `xlabels.angle`, `box.width`, `notchwidth` and
  `staplewidth` of
  [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md)
  and
  [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  are renamed `x_labels_angle`, `box_width`, `notch_width` and
  `staple_width`, and `xlabels.hjust` and `xlabels.vjust` are deprecated
  (the justification follows the angle). The former names still work,
  with a warning.
- The `outliers` table of
  [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md)
  has the columns `row` and `out` (the tail), as that of
  [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md).
- [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md)
  (and
  [`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md))
  rejects far fewer regular shots. With the few shots of a sample, the
  MAD of the sample is unstable: shots that differed by little from very
  similar shots got large z-scores, and the correlations near 1, bounded
  and skewed, made small differences look extreme. On the 2944 shots of
  the forage data set, 235 shots were rejected, many with a correlation
  above 0.99 with the median spectrum of their sample; in simulations,
  2.5% to 5% of regular shots were. The z-scores of the correlation are
  now those of Fisher’s z, the intensity and the distance are on a log
  scale, and the scale of a sample is its MAD but not less than the
  pooled MAD of all the samples (`scale = "floor"`, the default;
  `"sample"` and `"pooled"` are the others): 10 shots of the forage data
  set are rejected (weak or strong plasmas of other shapes), and 0.1% to
  0.5% of regular shots in simulations, while the shots of a plasma
  weakened by a quarter or more are still detected. The result also
  gives the shot number `.shot` (new argument `shot`), the raw criteria
  `.intensity` (relative to the median shot of the sample),
  `.correlation` and `.distance`, and its settings.
- The arguments of the points and lines of
  [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md)
  are renamed `point_size`, `point_color`, `line_width` and `fit_color`
  (from `pt.size`, `pt.colour`, `line.size` and `line.colour`);
  `pt.shape`, `pt.fill`, `linetype` and the `resid.*` arguments are
  deprecated and ignored.
- [`plot_outliers()`](https://christiangoueguel.com/specProc/reference/plot_outliers.md)
  flags the samples beyond the adaptive cutoff of Filzmoser, Garrett and
  Reimann (2005), as `aq.plot()` of the mvoutlier package, instead of
  the fixed 97.5% chi-squared quantile, which flags about 2.5% of clean
  data (the adaptive part had no effect: the cutoff was the smaller of
  the two). In `forageLIBS`, 29 of the 368 samples are flagged on Ca,
  Mg, P and K instead of 38. `cutoff = "quantile"` gives the former
  flags. `show.outlier` and `show.mahal` are deprecated in favor of
  `color_by` and `plot = FALSE`, and the table gains the columns `row`,
  `cutoff` and `weight`.
- The loadings of
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
  now have a fixed sign, their largest element positive, as those of
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  and
  [`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md).
  The signs of loadings and scores may differ from those of earlier
  versions.

### Bug fixes

- [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  now follows Bruffaerts et al. (2014) and the authors’ implementation:
  the transformed data are standardized by their interquartile range
  divided by `diff(qnorm(c(0.25, 0.75)))` = 1.349 (was 1.3426); a
  negative tail parameter `h` (tails lighter than normal) is kept
  instead of being set to 0, which widened the fences of such variables
  (in `forageLIBS`, at alpha = 0.7%, the upper fence of Ca was 1.25
  instead of 1.19); and the fences are never inside the box.
- `notch = TRUE` failed in
  [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md)
  and
  [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  (“object ‘notchlower’ not found”); the notches span the median plus or
  minus 1.58 IQR / sqrt(n) (McGill et al., 1978).
- [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md)
  drew straight lines across the gaps between detectors (in
  `forageLIBS`, 781.5 to 789.2 nm and 800.7 to 813.9 nm), and joined the
  channels of overlapping detectors in a zigzag (near 766 nm, on the K I
  766.49 nm line). Each detector is now drawn as its own line.
- [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
  and
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  failed, or flagged most observations as orthogonal outliers, when `k`
  equalled the rank of the data (for example `k = 5` with 5 variables,
  `k = n - 1` with more variables than observations, or a `k` chosen by
  `var_explained` for data with few variables): the orthogonal distances
  to a subspace that is the whole space are rounding errors, and their
  cut-off was zero. As in Hubert, Rousseeuw and Vanden Branden (2005),
  ROBPCA then reduces to the reweighted MCD of the data: the reweighting
  on the orthogonal distances is skipped, and H1 is the subset H0 of the
  least outlying observations (as in the rospca package). The orthogonal
  distances and their cut-off are now zero, with no orthogonal outliers,
  in
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  and
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
  and for new observations of
  [`predict()`](https://rdrr.io/r/stats/predict.html) that lie in the
  subspace.
- [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  and
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  no longer depend on the units of the data: their numerical tolerances
  (singular subsets of FAST-MCD, zero scales of the outlyingness, the
  cut-off of the orthogonal distances, the rank and the C-steps of
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
  and the constant variables of the detection of deviating cells) are
  relative to the scale of the data instead of absolute. Robust PCA
  failed on data with a small scale (“No non-singular subset found” for
  values around 1e-8, or a zero cut-off of the orthogonal distances when
  they were all below 1.5e-8), and
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  gave different results. With the same seed, `robpca(x * c)` now gives
  the same subsets and outlier types as `robpca(x)` for any `c > 0`. The
  final step of
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  now always uses the deterministic MCD when it succeeds: cellWise
  compares its log-determinant with the determinant of the C-steps, a
  comparison that depends on the units, and keeps it in the usual case.

## specProc 0.8.3

### New features

- [`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md):
  robust PCA by casewise and cellwise weighting (cellPCA, Centofanti,
  Hubert and Rousseeuw). It handles outlying observations, outlying
  cells and missing values by minimizing a single objective, with
  weights between 0 and 1 for every cell and observation. The
  iteratively reweighted least squares and the predictions of new data
  are computed in C++; the results agree with the reference code of the
  authors to numerical precision.
  [`predict()`](https://rdrr.io/r/stats/predict.html),
  [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
  (enhanced outlier map),
  [`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)
  and
  [`flagged_regions()`](https://christiangoueguel.com/specProc/reference/flagged_regions.md)
  accept its fits, and
  [`step_cellpca()`](https://christiangoueguel.com/specProc/reference/step_cellpca.md)
  is its recipe step.

### Breaking changes

- The detection of deviating cells (DDC) behind
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  and
  [`cellpca()`](https://christiangoueguel.com/specProc/reference/cellpca.md)
  now follows the DDC algorithm of the cellWise package: 1-step M
  estimators for the standardization and the residual scales, the robust
  Gnanadesikan-Kettenring correlations for the neighbors (wrapped
  correlations, found exactly, for more than 750 variables), robust
  slopes refined by least squares, each cell included in its own
  prediction, and the standalone variables. It flags the same cells as
  cellWise, and more of the outlying cells with far fewer false flags
  than before.
- [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  now follows the MacroPCA algorithm of Hubert, Rousseeuw and Van den
  Bossche (2019), built on ROBPCA, as implemented in cellWise, whose
  results it reproduces (with `scale = FALSE`): projection pursuit on
  the data where only the observations with the fewest flagged cells are
  cell-imputed, iterative subspace estimation on a fixed set of
  observations, one reweighting step on the orthogonal distances,
  concentration steps and the deterministic MCD within the subspace, and
  distances from the data with only the missing cells imputed. New data
  are analyzed as by MacroPCApredict (with the DDC model of the fit).
  Its results change. `tol` is now the largest angle between successive
  subspaces as a fraction of a right angle (default 0.005, as in the
  paper), the cut-offs of the score and orthogonal distances are at the
  99% level as in the paper, the residual scales are 1-step M scales,
  and the fit returns the observations set aside by DDC
  (`flagged_rows`).

### Bug fixes

- [`baseline_lsp()`](https://christiangoueguel.com/specProc/reference/baseline_lsp.md)
  uses the improved modified polynomial fit (IModPoly) of Zhao et
  al. (2007) by default (`method = "imodpoly"`). The modified polynomial
  fit of Lieber and Mahadevan-Jansen (2003), still available with
  `method = "modpoly"`, is pulled up by strong lines, and its baseline
  could then fall far below the background at the ends of the spectrum
  (for example above 290 nm in the 240-300 nm window of the `forageLIBS`
  spectra). The default `max.iter` is now 100.

### Dependencies

- `ggsci`, `prospectr` and `XICOR` move from Imports to Suggests.
  [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  uses the default ggplot2 palette when `ggsci` is not installed, and
  `correlation(method = "chatterjee")` asks for `XICOR`.
  [`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md)
  computes its Savitzky-Golay derivatives with the package’s own filter
  (same results).
- `mt` is no longer suggested.
- Every `step_*()` function checks that `recipes` is installed.

### Documentation

- Examples for the help pages that had none (the `step_*()` functions,
  [`predict()`](https://rdrr.io/r/stats/predict.html) methods,
  [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md),
  [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md),
  [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md),
  [`umad()`](https://christiangoueguel.com/specProc/reference/umad.md),
  the wavelength calibration helpers and the tuning parameters).
- The
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  example runs on a spectral window, and the
  [`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md)
  example that queries the NIST database is run with `\donttest` and
  [`try()`](https://rdrr.io/r/base/try.html).
- The package description covers the whole workflow, with references.
- Non-ASCII mathematical symbols removed from the help pages.

## specProc 0.8.1

### Bug fixes

- The Jackson-Mudholkar limit of the Q residuals
  ([`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md),
  and the Q limits of
  [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md))
  was wrong when its exponent h0 is close to zero or negative, which
  happens for spectra (a few large eigenvalues of the components left
  out, then a long tail of small ones): it fell below the mean of Q, so
  that nearly every sample was flagged. For example, with 3 components
  on the `forageLIBS` spectra, the 99% limit was 1.8e8 instead of 2.0e9,
  and 355 of the 368 spectra were flagged. The sign of h0 is now kept,
  as its derivation requires, and the limit as h0 tends to zero is used
  near zero.

## specProc 0.8.0

### Breaking changes

- [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md)
  has the arguments of the other orthogonalization methods:
  `opls(x, y, ncomp = NULL, center = TRUE, scale = FALSE, crossval = 7, permutation = 0)`.
  `ncomp` (the number of orthogonal components, `NULL` for the automatic
  choice) replaces `ncomp.ortho`, and the logical `center` and `scale`
  replace the character `scale`; for Pareto scaling, apply
  [`pareto_scale()`](https://christiangoueguel.com/specProc/reference/pareto_scale.md)
  to `x` first (or
  [`step_pareto_scale()`](https://christiangoueguel.com/specProc/reference/step_pareto_scale.md)
  before
  [`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md)),
  which gives the same filter. The former arguments still work, with a
  deprecation warning. The permutation test is now skipped by default
  (`permutation = 0`), as it refits the model as many times.
- [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)’s
  `ncomp` is now the number of orthogonal components removed, as in
  [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md),
  [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md) and
  [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md),
  instead of the number of PLS components (one more): replace
  `ncomp = k + 1` with `ncomp = k`. The default is 4 (formerly 5, the
  same filter).
  [`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md)
  is unchanged.
- [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  and
  [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md)
  (and
  [`step_macropca()`](https://christiangoueguel.com/specProc/reference/step_macropca.md),
  [`step_robust_bcyj()`](https://christiangoueguel.com/specProc/reference/step_robust_bcyj.md))
  are implemented natively, after the published algorithms, and no
  longer depend on cellWise, which moves to Suggests.
  - [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
    detects the deviating cells with DDC (Rousseeuw and Van den Bossche,
    2018), whose neighbor search and cell predictions are computed in
    C++ by blocks of variables: on the 368 x 7152 `forageLIBS` spectra,
    DDC takes about 5 s instead of 13 s, and
    [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
    about 12 s instead of 16 s. The results are close to, but not the
    same as, those of
    [`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html)
    (orthogonal distances correlated at 0.99 on `forageLIBS`, nearly the
    same subspace); the score and orthogonal distance cut-offs are those
    of
    [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md).
    In simulations with known rowwise and cellwise outliers and missing
    values, it estimates the PCA subspace more accurately than
    [`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html),
    imputes the missing values as accurately, and flags far fewer
    regular observations as outliers (cellWise compares the distances
    computed with the outlying cells to a cut-off estimated with them
    imputed). Its arguments `scale`, `ndir`, `maxiter` and `tol` replace
    the `...` passed to
    [`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html),
    the result has no `fit` element any more, and `imputed` holds the
    data with the missing values imputed.
  - [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md)
    fits the transformations of Raymaekers and Rousseeuw (2021), with
    nearly the same parameters as
    [`cellWise::transfo()`](https://rdrr.io/pkg/cellWise/man/transfo.html),
    and leaves the variables that cannot be transformed unchanged
    (method `"none"`).
- [`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  and
  [`saha_boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
  are renamed
  [`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)
  and
  [`saha_boltzmann()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann.md),
  so that they are not confused with
  [`plot_boltzmann()`](https://christiangoueguel.com/specProc/reference/plot_boltzmann.md),
  which draws their result. The old names still work, with a deprecation
  warning.
- In
  [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md),
  the nominal degrees of freedom (K - k) are now selected with
  `df = "nominal"`.
- The `soilLIBS` data set is removed, to keep the package data under the
  5 MB CRAN guideline: `data(soilLIBS)` no longer works. The examples
  use `forageLIBS` instead; the example of
  [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md),
  which relied on the soil replicates, is removed. The raw data and the
  script that built it remain in `data-raw/` of the source repository.

### New features

- [`som()`](https://christiangoueguel.com/specProc/reference/som.md)
  fits a self-organizing map of spectra (batch SOM, in C++), which maps
  similar spectra onto the same or neighboring units of a hexagonal or
  rectangular grid and follows nonlinear relations (matrix effects,
  self-absorption, plasma changes) that PCA can miss. It starts from the
  plane of the first two principal components (deterministic training),
  chooses the grid from the number of spectra, and reports the
  quantization and topographic errors. `robust = TRUE` down-weights
  outlying spectra by Huber weights of their quantization errors.
  [`predict()`](https://rdrr.io/r/stats/predict.html) places new spectra
  on their best-matching unit and flags as novel those beyond a robust
  cut-off of the training quantization errors.
  [`plot_som()`](https://christiangoueguel.com/specProc/reference/plot_som.md)
  draws the counts, the U-matrix, the quantization errors, the component
  planes (one map per wavelength or line), the spectra on the map and
  the prototype spectra, and
  [`som_stability()`](https://christiangoueguel.com/specProc/reference/som_stability.md)
  measures how consistently the spectra keep their neighbors over
  resampled fits. On the 368 x 7152 `forageLIBS` spectra, a map takes
  about 2 s.

### Plots

- [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md)
  has an `offset` argument, to shift successive spectra vertically and
  horizontally (`offset = c(horizontal, vertical)`, or a single number
  for a vertical offset), for example to stack them.
- [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  recognizes UMAP maps (axes named `UMAP1`, `UMAP2`, …, as by
  [`embed::step_umap()`](https://embed.tidymodels.org/reference/step_umap.html)):
  it leaves out the lines through the origin, whose position means
  nothing on such a map, and ignores `hotelling` (`"all"` or `"group"`),
  with a warning, since its T-squared limits assume linear scores. On
  other maps, the lines through the origin are light grey instead of
  black.
- [`correlation()`](https://christiangoueguel.com/specProc/reference/correlation.md)
  takes several responses (`var = c(K, Ca)`, or
  `dplyr::all_of(minerals)`), each with its own observations, so that a
  response with many missing values does not reduce the data of the
  others. The result gains an `outcome` column, and `plot = TRUE` draws
  a heatmap of the responses against the wavelengths (or the other
  variables), on a fixed scale from -1 to 1. Pearson and Spearman
  correlations of spectra are computed in one step: the 12 minerals of
  `forageLIBS` against its 7152 channels take under a second. With
  `cluster = TRUE`, the rows are ordered by a hierarchical clustering of
  the responses on their correlation profiles (1 - r, average linkage),
  so that responses whose correlations rise and fall at the same
  wavelengths are adjacent, with the dendrogram on the left.
- Plot titles are bold, and a title longer than 45 characters is split
  at a natural break (“:”, ” (“,” - ” or “,”) into a shorter title and a
  subtitle, for example
  [`plot_calibration()`](https://christiangoueguel.com/specProc/reference/plot_calibration.md)’s
  “Linear calibration” above its R², LOD and LOQ. This applies to every
  plot with a title, including the interactive (plotly) ones and
  [`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md).
  [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md)
  now shows its `title` as a title rather than a subtitle.

### Documentation

- The examples use the `forageLIBS` spectra and mineral contents instead
  of simulated data, wherever the function applies to them (some taken
  from the vignettes). Examples of pure helper functions (line profiles,
  plasma criteria, tuning parameters) keep simple inputs, and the
  examples of
  [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md) and
  [`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md),
  for which `forageLIBS` has no suitable data, are removed.
  [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md)
  gains an example.
- Every example calls dplyr functions with `dplyr::`, so that it runs
  without dplyr attached.
- The vignettes are reorganized into the four stages of a LIBS analysis,
  all on the `forageLIBS` spectra: fitting emission lines
  ([`vignette("peak-fitting")`](https://christiangoueguel.com/specProc/articles/peak-fitting.md),
  which replaces `"line-fitting"`), preprocessing with recipe steps
  ([`vignette("preprocessing")`](https://christiangoueguel.com/specProc/articles/preprocessing.md)),
  calibration curves and figures of merit
  ([`vignette("calibration")`](https://christiangoueguel.com/specProc/articles/calibration.md))
  and plasma diagnostics
  ([`vignette("plasma-diagnostics")`](https://christiangoueguel.com/specProc/articles/plasma-diagnostics.md)).
  The orthogonalization vignette is folded into the preprocessing one.
- The README is shorter, with an overview of the functions by task.

## specProc 0.7.0

### Performance

- Column medians are computed in C++
  ([`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md),
  [`msc()`](https://christiangoueguel.com/specProc/reference/msc.md) and
  [`emsc()`](https://christiangoueguel.com/specProc/reference/emsc.md)
  with `robust = TRUE`, `center(method = "median")`,
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)):
  [`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md)
  is about 20 times faster on 400 spectra of 7152 channels.
- [`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md)
  and
  [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md)
  compute the residuals of the calibration samples from the scores of
  the components left out, without reconstructing the data (about 100
  times faster), and
  [`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)
  measures all the spectra of a line at once (about 3 times faster). The
  results are unchanged.

### Breaking changes

- [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md)
  is implemented natively and no longer needs the Bioconductor package
  ropls. It reproduces
  [`ropls::opls()`](https://rdrr.io/pkg/ropls/man/opls.html) (scores,
  loadings, R2X, R2Y, Q2, RMSEE, VIP and permutation p-values), but
  returns an object of class `specproc_opls` instead of a list holding
  the ropls model: the `model` element is removed, and the model has new
  elements `correction` (the OPLS-filtered data), `fitted`,
  `coefficients`, `vip`, `ortho_vip`, `components` (R2X, R2Y and Q2 of
  each component) and `permutation`. `x` and `y` can be matrices or
  vectors as well as data frames. When `ncomp.ortho = NA` and only the
  predictive component is significant, the model has no orthogonal
  component (with a warning) instead of failing.
  [`predict()`](https://rdrr.io/r/stats/predict.html) filters new data,
  or predicts the response or the scores (`type`), and `crossval = 0`
  skips the cross-validation.
- The data sets are renamed: `specLIBS` is now `soilLIBS`, and
  `fourrage` is now `forageLIBS`. Replace `data(specLIBS)` with
  `data(soilLIBS)` and `data(fourrage)` with `data(forageLIBS)`.
- [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  (and
  [`step_macropca()`](https://christiangoueguel.com/specProc/reference/step_macropca.md))
  now only centers the variables by default, instead of also scaling
  them as
  [`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html)
  does: scaling spectra gives noise and continuum channels as much
  weight as emission lines. Use `macropca(x, scale = TRUE)` for the
  previous behavior.
- [`plot_cell_map()`](https://christiangoueguel.com/specProc/reference/plot_cell_map.md)
  is redrawn with ggplot2 and patchwork instead of
  [`cellWise::cellMap()`](https://rdrr.io/pkg/cellWise/man/cellMap.html),
  for spectra: the map fills the plot, with a wavelength axis, cells
  combined into at most `resolution` blocks (within detector segments),
  flagged cells colored by the sign and size of their residual, a strip
  of the outlier type of each observation, rows optionally sorted by
  orthogonal distance (`order = "od"`), and a top panel of the share of
  flagged cells at each wavelength (`profile`). The arguments
  `nrowsinblock`, `ncolumnsinblock` and `...` are replaced by
  `resolution`. The profile shows the mean spectrum in grey (of the
  imputed data, or of `spectra`) and labels the most flagged regions,
  with the emission lines they match (`lines`), and `order = "cluster"`
  groups the observations with similar flagged cells.
- [`flagged_regions()`](https://christiangoueguel.com/specProc/reference/flagged_regions.md)
  lists the wavelength regions where many observations have cells
  flagged by
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  (at least `threshold` of them), with their extent, share, direction
  and matching emission lines.

### New features

- [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md)
  is drawn like
  [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md):
  points outlined in black and filled by type (in the colors of the
  outlier map), dashed limits at the highest confidence level (dotted, …
  at the others, with a legend only for several levels), a compact
  legend at the bottom, and labels on the `labels` (default 3) most
  outlying samples only, instead of all the flagged ones. It gains the
  options of
  [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md):
  `relative` (distances divided by their limits), `shade` (the outlying
  regions), `log`, `colour_by = "distance"` and point styles in `...`,
  including a size per sample. The arguments after `label` changed
  order: name `log` and `title`. In both plots, labels near the right
  edge are placed on the left of their point, so that they are not cut
  off.

- [`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md)
  removes the orthogonal components of an
  [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md)
  model in a recipe, with `num_comp` tunable. It gives the same filtered
  data as
  [`step_projected_osc()`](https://christiangoueguel.com/specProc/reference/step_projected_osc.md),
  and also offers Pareto scaling.

- [`step_o2pls()`](https://christiangoueguel.com/specProc/reference/step_o2pls.md)
  removes the outcome-orthogonal components of an
  [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md)
  model in a recipe, estimated against one or several outcomes
  (`recipe(K + Ca ~ ., ...)`), with `num_comp` and `joint_comp` tunable
  for a single outcome. Only the predictors are filtered. Its help page
  shows a workflow predicting several outcomes and, since
  [`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
  does not support several outcomes, how to tune one workflow per
  outcome with a workflow set (`workflow_map("tune_grid", ...)`), each
  filter still estimated against all the outcomes.

- [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  is drawn with a grey panel outside the outermost ellipse (of
  T-squared, or of each group) and white inside, with no grid, a fixed
  `aspect_ratio` (0.7) and lines through the origin. The axis titles
  give the explained variance of the components of PCA fits, and the
  T-squared ellipses of the groups (`hotelling = "group"`) are filled
  with their color. `size` can be a variable (such as a concentration)
  that sets the size of the points, and `biplot = TRUE` draws the
  loadings of a PCA over the scores, with the `biplot_top` most
  important variables (emission lines, for spectra) as labeled arrows.
  With `hotelling = "all"`, the T-squared of a PCA model is that of the
  model (its eigenvalues, centered at 0): for a robust fit, the ellipses
  are no longer inflated or tilted by the outlying samples, and the
  flagged samples are the leverage points of
  [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md).
  Without groups, `ellipse = TRUE` with `hotelling = "all"` draws only
  the T-squared ellipse, with a warning, instead of two ellipses of all
  the samples. The samples beyond the T-squared limit are counted in the
  subtitle, circled in red only with `flag = TRUE`, and labeled only
  with `label` (`TRUE`, or a column or vector of labels), independently
  of each other.

- [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
  takes a size per sample (`size`), and `colour_by = "distance"` colors
  its points by their reduced distance from the origin (the larger of SD
  and OD over their cut-offs), on a rainbow from dark red to blue; with
  `relative = TRUE`, where both cut-offs are at 1, yellow marks the
  cut-offs, between the regular observations (dark red to orange) and
  the outlying ones (green to blue).

- The legends of
  [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  and
  [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
  are more compact: smaller text and keys, three sizes, the T-squared
  levels written on their ellipses instead of in a legend, and no key
  for the kind of samples without new samples.

- [`contributions()`](https://christiangoueguel.com/specProc/reference/contributions.md)
  computes the contributions of each variable to the Q residual or to
  Hotelling’s T-squared of samples (whose squares add up to Q and
  T-squared), for a [`prcomp()`](https://rdrr.io/r/stats/prcomp.html)
  fit or a robust fit (where they decompose the orthogonal and score
  distances), and relative contributions against reference samples
  (`reference`, or `"regular"` for all regular samples).
  [`plot_contributions()`](https://christiangoueguel.com/specProc/reference/plot_contributions.md)
  draws them against wavelength with the largest labeled and matched to
  emission lines.

- [`plot_loadings()`](https://christiangoueguel.com/specProc/reference/plot_loadings.md)
  plots the loadings of a PCA
  ([`prcomp()`](https://rdrr.io/r/stats/prcomp.html),
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  or
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md))
  against wavelength, one panel per component, and labels the
  wavelengths that contribute most to each component, with the emission
  lines they match in a line list from
  [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md).
  The mean spectrum can be drawn behind the loadings,
  `type = "contribution"` plots the variance of each wavelength
  explained by the components, and `interactive = TRUE` makes a plotly
  figure.
  [`loading_peaks()`](https://christiangoueguel.com/specProc/reference/loading_peaks.md)
  returns the labeled peaks as a table, with their candidate lines and a
  flag for derivative shapes (line shifts).

- [`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)
  gains `relative`, to plot the reduced distances (divided by their
  cut-offs) so that maps of different models share one scale, `shade`,
  to shade the good leverage, orthogonal outlier and bad leverage
  regions in greys of increasing darkness and name them, and `log`, for
  logarithmic axes. The points are outlined in black, and `...` passes
  styling such as `alpha` or `size` to
  [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md)
  computes the distance to the model in the space of the variables
  (DModX) of a PCA model, normalized or absolute, for the calibration
  samples or new ones. Its limits use the effective number of residual
  dimensions by default (`df = "effective"`), because the nominal
  degrees of freedom (`df = "nominal"`) flag a large share of ordinary
  samples when spectra have far more channels than samples.
  [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md)
  draws DModX or Q against T-squared.

- [`hotelling_t2()`](https://christiangoueguel.com/specProc/reference/hotelling_t2.md),
  [`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md),
  [`dmodx()`](https://christiangoueguel.com/specProc/reference/dmodx.md),
  [`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md)
  and the T-squared ellipses of
  [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md)
  take any confidence level(s) (`conf_level`, 0.975 by default; in
  [`plot_embedding()`](https://christiangoueguel.com/specProc/reference/plot_embedding.md),
  one `conf_level` sets both its confidence and T-squared ellipses), and
  the Beta distribution of T-squared for the samples of the model
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

### Bug fixes

- [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  failed when `k` was not given:
  [`cellWise::MacroPCA()`](https://rdrr.io/pkg/cellWise/man/MacroPCA.html)
  with `k = 0` only reports the explained variance. `k` is now chosen
  from the new arguments `var_explained` (0.8) and `kmax` (10), as in
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  and MacroPCA’s message and scree plot are no longer shown.

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
  [`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md),
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
  - [`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
    and
    [`saha_boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/specProc-deprecated.md)
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
