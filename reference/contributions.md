# Q and T-squared Contributions of a PCA Model

Computes how much each variable (wavelength) contributes to the Q
residual or to Hotelling's \\T^2\\ of each sample, keeping the sign of
the deviation, to find the variables that make a sample an outlier.
Relative contributions, computed against reference samples, show what
differs between a sample and the reference ones.

## Usage

``` r
contributions(
  model,
  k = NULL,
  statistic = c("q", "t2"),
  data = NULL,
  samples = NULL,
  reference = NULL
)
```

## Arguments

- model:

  A [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit, or an
  object returned by
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  or
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md).

- k:

  The number of components. Required for a
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit; for a
  robust fit, at most its number of components (the default).

- statistic:

  `"q"` (default) for the Q contributions, or `"t2"` for the \\T^2\\
  contributions.

- data:

  The samples whose contributions are computed: a numeric matrix or data
  frame with the variables of the model (other columns are ignored when
  the columns are named), preprocessed like the data the model was
  fitted on (for example, with
  [`center()`](https://christiangoueguel.com/specProc/reference/center.md)
  if the model was fitted on `center(spectra)`). Default is the
  calibration data of a
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit (which
  must keep all its components), or the imputed data of a
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
  fit; it is required for
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
  and
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  fits, which do not keep their data.

- samples:

  Optional indices or names of the rows of `data` to return. Default is
  all.

- reference:

  Optional indices or names of rows of `data` whose mean contribution is
  subtracted (relative contributions), or `"regular"` for all the
  regular samples.

## Value

A tibble of class `specproc_contributions` with one row per sample:
`sample` (the row name, or number) and one column per variable. The
attribute `total` holds the statistic of each sample (Q or \\T^2\\,
before any reference is subtracted). Draw it with
[`plot_contributions()`](https://christiangoueguel.com/specProc/reference/plot_contributions.md).

## Details

For a sample \\x\\ (centered and scaled as in the model), with scores
\\t = xP\\ on the first \\k\\ components (loadings \\P\\, variances of
the scores \\\lambda\\):

- the **Q contributions** are the residuals \\e = x - tP^T\\, whose sum
  of squares is Q (the squared orthogonal distance of robust fits);

- the **\\T^2\\ contributions** are \\t \Lambda^{-1/2} P^T\\, the scaled
  scores projected back onto the variables, whose sum of squares is
  \\T^2\\ (the squared score distance of robust fits). This is the
  definition of the PLS_Toolbox (Eigenvector Research). For
  [`rospca()`](https://christiangoueguel.com/specProc/reference/rospca.md)
  fits, whose loadings are not exactly orthogonal, the sum of squares is
  close to, but not exactly, \\T^2\\.

Q contributions show which variables the model does not describe for a
sample: large contributions grouped on a few emission lines point to a
systematic deviation (a contamination, a matrix effect, a saturated or
shifted line), small ones spread over all channels to random noise.
\\T^2\\ contributions show which variables place a sample far from the
center in the score space.

**Relative contributions.** Normally, contributions are relative to the
model (its center, for \\T^2\\). With `reference`, the mean contribution
of the reference samples is subtracted from the contribution of each
sample, which shows what differs between them: for example, whether two
samples have large Q residuals for the same reason, or what moves a
sample from the regular samples to where it lies in the score space.
`reference = "regular"` uses all the regular samples: of the robust fit,
or at the highest confidence level of
[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md)
for a [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit.

The contributions are in the units of the preprocessed data of the
model: centered (and scaled, if the model scaled the variables).

## References

- Wise, B.M., Gallagher, N.B., Bro, R., Shaver, J.M., Windig, W., Koch,
  R.S. (2006). PLS_Toolbox 4.0 for use with MATLAB. Eigenvector
  Research, Wenatchee, WA.

- Westerhuis, J.A., Gurden, S.P., Smilde, A.K. (2000). Generalized
  contribution plots in multivariate statistical process monitoring.
  Chemometrics and Intelligent Laboratory Systems, 51(1):95-114.

## See also

[`plot_contributions()`](https://christiangoueguel.com/specProc/reference/plot_contributions.md),
[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md),
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)

## Author

Christian L. Goueguel

## Examples

``` r
minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
spectra <- dplyr::select(forageLIBS, -Measurement, -Sample, -dplyr::all_of(minerals))
set.seed(1)
fit <- robpca(spectra, k = 3)
# Q contributions of two outlying samples, relative to the regular ones
q <- contributions(fit, data = spectra, samples = c(49, 127), reference = "regular")
q[, 1:5]
#> # Q contributions of 2 sample(s) to 4 variables (3 components), relative to reference samples
#> # A tibble: 2 × 5
#>   sample `199.3771616` `199.4644141` `199.5516666` `199.6389192`
#>   <chr>          <dbl>         <dbl>         <dbl>         <dbl>
#> 1 49              22.2          3.58          18.8        -15.3 
#> 2 127             11.1          5.69         -31.7         -3.89
```
