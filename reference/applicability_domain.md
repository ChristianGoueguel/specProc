# Applicability Domain of a Calibration

Describes the space of the calibration spectra by a principal component
analysis (PCA), so that the spectra of new samples can be flagged when
they lie outside it: their predictions, by any model calibrated on these
spectra, are then extrapolations.

## Usage

``` r
applicability_domain(
  x,
  k = NULL,
  variance = 0.99,
  conf_level = 0.99,
  center = TRUE,
  scale = FALSE
)
```

## Arguments

- x:

  The calibration spectra: a numeric matrix or data frame, one spectrum
  per row.

- k:

  The number of principal components. By default, chosen from
  `variance`.

- variance:

  The fraction of the variance explained by the components, when `k` is
  not given. Default is 0.99.

- conf_level:

  The confidence level of the limits. Default is 0.99.

- center, scale:

  Passed to [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html).
  Default is `TRUE` and `FALSE`.

## Value

An object of class `specproc_domain`, a list with the `pca`
([`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) fit), the
number of components `k`, the fraction of the variance they `explained`,
the `conf_level` and the `limits` of \\T^2\\ and Q.
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_domain.md)
flags new spectra.

## Details

A new spectrum is outside the applicability domain when its Hotelling
\\T^2\\, the distance within the plane of the first `k` components, or
its Q residual, the squared distance to that plane, exceeds its limit at
`conf_level` (see
[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md)).
A high \\T^2\\ is a sample more extreme than the calibration samples
(for example, a higher concentration); a high Q, a sample of another
kind (another matrix, a contamination, an instrumental problem). The
limit of \\T^2\\ is that of a new observation, and that of Q the
Jackson-Mudholkar limit.

By default, `k` is the smallest number of components that explain the
fraction `variance` of the variance of the calibration spectra. Prepare
the spectra as for the model (baseline correction, normalization), since
the domain is that of the variables the model uses. The PCA is
classical: remove the outlying calibration spectra first (see
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)).

## References

- Jackson, J.E., Mudholkar, G.S. (1979). Control procedures for
  residuals associated with principal component analysis. Technometrics,
  21(3):341-349.

- Nomikos, P., MacGregor, J.F. (1995). Multivariate SPC charts for
  monitoring batch processes. Technometrics, 37(1):41-59.

## See also

[`predict.specproc_domain()`](https://christiangoueguel.com/specProc/reference/predict.specproc_domain.md),
[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md),
[`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- as.matrix(forageLIBS[which(wl > 380 & wl < 430)])
domain <- applicability_domain(spectra[1:300, ])
domain
#> Applicability domain: PCA of 300 calibration spectra
#> 
#> Components:  11 (99.08% of the variance)
#> Limits:       T2 = 26.37, Q = 34150000 (99%)
flags <- predict(domain, spectra[301:368, ])
table(flags$inside)
#> 
#> FALSE  TRUE 
#>     5    63 
```
