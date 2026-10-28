# Spectral Space Transformation

Transfers spectra from a secondary instrument to the primary instrument,
on which the calibration model was built, by spectral space
transformation (SST; Du et al., 2011): the secondary spectra are moved,
in the space of the principal components shared by both instruments, by
the difference between the instruments.

## Usage

``` r
sst(x1, x2, ncomp = NULL, variance = 0.99)
```

## Arguments

- x1:

  The spectra of the transfer samples on the primary instrument: a
  numeric matrix or data frame, one spectrum per row.

- x2:

  The spectra of the same samples, in the same order, on the secondary
  instrument.

- ncomp:

  The number of principal components. By default, chosen from
  `variance`.

- variance:

  The fraction of the variance explained, when `ncomp` is not given.
  Default is 0.99.

## Value

An object of class `specproc_transfer`. Use
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_transfer.md)
to transfer secondary spectra.

## Details

The transfer spectra of both instruments, centered by the mean of each
instrument, are joined side by side, \\\[\textbf{X}\_1\\
\textbf{X}\_2\]\\, and decomposed by a principal component analysis,
\\\[\textbf{X}\_1\\ \textbf{X}\_2\] \approx \textbf{T}
\[\textbf{P}\_1^T\\ \textbf{P}\_2^T\]\\: the same scores \\\textbf{T}\\
describe a sample on both instruments, and the loadings
\\\textbf{P}\_1\\ and \\\textbf{P}\_2\\ the spectra of each. A new
secondary spectrum gets the scores \\t = (x_2 - \bar{x}\_2)
(\textbf{P}\_2^T)^{+}\\ and is transferred by adding the difference of
the instruments in that space: \$\$x\_{2 \to 1} = \bar{x}\_1 + (x_2 -
\bar{x}\_2) + t\\(\textbf{P}\_1^T - \textbf{P}\_2^T)\$\$ The part of the
spectrum outside the shared components is kept unchanged, so SST
corrects only the differences the transfer samples show.

By default, `ncomp` is the smallest number of components that explain
the fraction `variance` of the variance of the joined spectra.

## References

- Du, W., Chen, Z.-P., Zhong, L.-J., Wang, S.-X., Yu, R.-Q., Nordon, A.,
  Littlejohn, D., Holden, M. (2011). Maintaining the predictive
  abilities of multivariate calibration models by spectral space
  transformation. Analytica Chimica Acta, 690(1):64-70.

## See also

[`ds()`](https://christiangoueguel.com/specProc/reference/ds.md),
[`pds()`](https://christiangoueguel.com/specProc/reference/pds.md),
[`slope_bias()`](https://christiangoueguel.com/specProc/reference/slope_bias.md),
[`predict.specproc_transfer()`](https://christiangoueguel.com/specProc/reference/predict.specproc_transfer.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- as.matrix(forageLIBS[which(wl > 760 & wl < 780)])  # the K I lines
secondary <- 0.8 * cbind(spectra[, -1], spectra[, ncol(spectra)]) + 50
transfer <- sst(spectra[1:30, ], secondary[1:30, ])
transfer
#> Spectral space transformation (30 transfer samples, 245 wavelengths)
#> Components: 3 (99.12% of the variance)
transferred <- predict(transfer, secondary[31:368, ])
mean(abs(transferred - spectra[31:368, ])) / mean(spectra)
#> [1] 0.02512886
```
