# External Parameter Orthogonalization

This function implements the External Parameter Orthogonalization (EPO)
algorithm as proposed by Roger *et al.* (2003). The EPO algorithm aims
to remove interferences or clutter present in the spectral matrix,
effectively separating the signal of interest from unwanted
perturbations.

## Usage

``` r
epo(x, ncomp = 2, clutter = NULL)
```

## Arguments

- x:

  A numeric matrix, data frame or tibble.

- ncomp:

  An integer specifying the number of singular vectors to orthogonalize
  against. Default is 2.

- clutter:

  An optional numeric matrix or data frame, with the same number of
  columns as `x`, describing the external (clutter) variation. If `NULL`
  (default), the clutter directions are estimated from `x` itself, i.e.
  the `ncomp` dominant directions of `x` are removed.

## Value

An object of class `specproc_epo` (a list), which
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
applies to new spectra, with four components:

- `correction`: The orthogonalized matrix, representing the signal of
  interest.

- `clutter`: The clutter part of `x`, \\\textbf{XVV}^T\\.

- `loadings`: The singular vectors \\\textbf{V}\\ used for the
  orthogonalization.

- `singular_values`: The corresponding singular values of the clutter
  matrix.

## Details

The EPO algorithm works by detecting the dominant directions (right
singular vectors) of a clutter matrix \\\textbf{D}\\ that describe the
external variations, e.g. the differences between spectra of the same
samples measured under different temperatures or moisture levels. It
then projects the spectral matrix onto the subspace orthogonal to these
variations, effectively removing the unwanted perturbations and
extracting the signal of interest.

Let \\\textbf{X}\\ be the spectral matrix. The EPO algorithm aims to
split \\\textbf{X}\\ into:

\$\$\textbf{X} = \textbf{XP} + \textbf{XQ} + \textbf{R}\$\$

where \\\textbf{P}\\ and \\\textbf{Q}\\ are, respectively, the
projection matrices of \\\textbf{X}\\ onto the useful and perturbation
(clutter) subspaces. \\\textbf{R}\\ is the residual matrix. With
\\\textbf{V}\\ the first `ncomp` right singular vectors of
\\\textbf{D}\\, \\\textbf{Q} = \textbf{VV}^T\\ and the corrected matrix
is \\\textbf{X} - \textbf{XVV}^T\\.

The singular value decomposition is computed in C++ (Eigen), and the \\p
\times p\\ projection matrix is never formed explicitly.

## References

- Roger, J.-M., Chauchard, F., Bellon-Maurel, V. (2003). EPO-PLS
  external parameter orthogonalization of PLS application to
  temperature-independent measurement of sugar content of intact fruits.
  Chemometrics and Intelligent Laboratory Systems, 66(2):191-204.

## See also

[`predict.specproc_filter()`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
to correct new spectra with the fitted filter, and
[`step_epo()`](https://christiangoueguel.com/specProc/reference/step_epo.md)
to use it in a tidymodels recipe.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]  # the spectral channels
# the three samples measured twice
twice <- forageLIBS$Sample[duplicated(forageLIBS$Sample)]
first <- match(twice, forageLIBS$Sample)
second <- vapply(twice, function(s) max(which(forageLIBS$Sample == s)), integer(1))
# their differences describe the variation between repeated measurements
clutter <- as.matrix(spectra[second, ]) - as.matrix(spectra[first, ])
res <- epo(spectra, ncomp = 1, clutter = clutter)
dim(res$correction)
#> [1]  368 7152
```
