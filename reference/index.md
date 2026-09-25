# Package index

## Univariate filtering

Functions used to assess individual variables or features independently.

- [`zscore()`](https://christiangoueguel.com/specProc/reference/zscore.md)
  : Classical or Robust Z-Score
- [`iqrMethod()`](https://christiangoueguel.com/specProc/reference/iqrMethod.md)
  : Interquartile Range Method
- [`directOutlyingness()`](https://christiangoueguel.com/specProc/reference/directOutlyingness.md)
  : Directional Outlyingness for Skewed Distribution

## Multivariate filtering

### orthogonalization methods

Functions used to identify and remove unwanted covariance structures and
sources of variance from the multivariate data.

- [`epo()`](https://christiangoueguel.com/specProc/reference/epo.md) :
  External Parameter Orthogonalization
- [`osc()`](https://christiangoueguel.com/specProc/reference/osc.md) :
  Orthogonal Signal Correction
- [`nas()`](https://christiangoueguel.com/specProc/reference/nas.md) :
  Net Analyte Signal
- [`opls()`](https://christiangoueguel.com/specProc/reference/opls.md) :
  Orthogonal Projections to Latent Structures
- [`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md)
  : Modified Orthogonal Projections to Latent Structures
- [`direct_osc()`](https://christiangoueguel.com/specProc/reference/direct_osc.md)
  : Direct Orthogonal Signal Correction
- [`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
  : Projected Orthogonal Signal Correction
- [`yGradientglsw()`](https://christiangoueguel.com/specProc/reference/yGradientglsw.md)
  : y-Gradient Generalized Least Squares Weighting
- [`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md)
  : Direct Orthogonalization

### calibration transfer

Functions used to to transfer a calibration model developed on one
instrument to another instrument.

- [`pds()`](https://christiangoueguel.com/specProc/reference/pds.md) :
  Piecewise Direct Standardization
- [`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md) :
  Generalized Least Squares Weighting

## Peak fitting

Functions used to characterize spectral lines. The choice of the fitting
function depends on the specific broadening mechanisms and physical
conditions of the emitting environment.

- [`peakfit()`](https://christiangoueguel.com/specProc/reference/peakfit.md)
  : Peak Fitting
- [`multipeakfit()`](https://christiangoueguel.com/specProc/reference/multipeakfit.md)
  : Multiple Peaks Fitting

## Data visualization

Functions that help assess data visually.

- [`plotfit()`](https://christiangoueguel.com/specProc/reference/plotfit.md)
  : Plotting of Fitted Spectral Line
- [`plotSpec()`](https://christiangoueguel.com/specProc/reference/plotSpec.md)
  : Plotting of Spectra
- [`outlierplot()`](https://christiangoueguel.com/specProc/reference/outlierplot.md)
  : Univariate Representation of Multivariate Outliers
- [`adjusted_boxplot()`](https://christiangoueguel.com/specProc/reference/adjusted_boxplot.md)
  : Adjusted Boxplot
- [`generalized_boxplot()`](https://christiangoueguel.com/specProc/reference/generalized_boxplot.md)
  : Generalized Boxplot

## Smoothing

Functions used to increase the signal-to-noise ratio.

- [`average()`](https://christiangoueguel.com/specProc/reference/average.md)
  : Fast Average for Large Spectral Dataset

## Baseline correction

Functions used to fit and remove the background emission.

- [`whittaker()`](https://christiangoueguel.com/specProc/reference/whittaker.md)
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
- [`normalize()`](https://christiangoueguel.com/specProc/reference/normalize.md)
  : Spectra Normalization

### data scaling

Functions used to adjust each variable/feature by a scaling factor
computed based on the dispersion of the variable.

- [`snv()`](https://christiangoueguel.com/specProc/reference/snv.md) :
  Standard Normal Variate
- [`pareto()`](https://christiangoueguel.com/specProc/reference/pareto.md)
  : Pareto Scaling
- [`minmax()`](https://christiangoueguel.com/specProc/reference/minmax.md)
  : Min-Max Normalization
- [`center()`](https://christiangoueguel.com/specProc/reference/center.md)
  : Data Centering
- [`poisson_scale()`](https://christiangoueguel.com/specProc/reference/poisson_scale.md)
  : Poisson Scaling

### data transformation

Function used to transform variable(s) toward central normality.

- [`robustBCYJ()`](https://christiangoueguel.com/specProc/reference/robustBCYJ.md)
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
- [`rousseeuwCroux()`](https://christiangoueguel.com/specProc/reference/rousseeuwCroux.md)
  : Rousseeuw-Croux Scale Estimators

## Special functions

Functions used for charcaterizing plasma emission in laser spectroscopy.

- [`tukeyGH()`](https://christiangoueguel.com/specProc/reference/tukeyGH.md)
  : Tukey g-and-h Parametric Distribution
- [`gaussian()`](https://christiangoueguel.com/specProc/reference/gaussian.md)
  : Gaussian Function
- [`lorentzian()`](https://christiangoueguel.com/specProc/reference/lorentzian.md)
  : Lorentzian Function
- [`pseudo_voigt()`](https://christiangoueguel.com/specProc/reference/pseudo_voigt.md)
  : Pseudo-Voigt Function

## Miscellaneous

- [`correlation()`](https://christiangoueguel.com/specProc/reference/correlation.md)
  : Correlation Coefficients: Pearson, Spearman, Kendall, Chatterjee,
  and Biweight Midcorrelation
- [`summaryStats()`](https://christiangoueguel.com/specProc/reference/summaryStats.md)
  : Classical or Robust Descriptive Statistics
