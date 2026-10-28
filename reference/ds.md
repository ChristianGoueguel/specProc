# Direct Standardization

Transfers spectra from one instrument (the slave) to another (the
master) by direct standardization (DS; Wang et al., 1991): a linear map
of the whole slave spectrum onto the whole master spectrum, estimated on
transfer samples measured on both instruments.

## Usage

``` r
ds(x1, x2, ncomp = NULL, tol = 1e-08)
```

## Arguments

- x1:

  The spectra of the transfer samples on the master instrument: a
  numeric matrix or data frame, one spectrum per row.

- x2:

  The spectra of the same samples, in the same order, on the slave
  instrument.

- ncomp:

  The number of singular vectors of the slave spectra kept. By default,
  all those with a singular value above `tol` times the largest.

- tol:

  The relative tolerance on the singular values. Default is 1e-8.

## Value

An object of class `specproc_transfer`. Use
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_transfer.md)
to transfer slave spectra.

## Details

With the transfer spectra centered by the mean of each instrument,
\\\textbf{X}\_1 \approx \textbf{X}\_2 \textbf{F}\\, and the transfer
matrix is \\\textbf{F} = \textbf{X}\_2^{+} \textbf{X}\_1\\, where
\\\textbf{X}\_2^{+}\\ is the pseudo-inverse of the slave spectra,
truncated to their first `ncomp` singular vectors. A new slave spectrum
is transferred as \\\bar{x}\_1 + (x_2 - \bar{x}\_2) \textbf{F}\\.

With as many components as the transfer samples allow (`ncomp = NULL`),
the transfer spectra are reproduced exactly, and the noise of the slave
spectra is amplified: fewer components give a smoother, more robust
transfer. Unlike
[`pds()`](https://christiangoueguel.com/specProc/reference/pds.md), DS
mixes all wavelengths, so it corrects global differences of the response
but needs more transfer samples. The transfer matrix (\\p \times p\\) is
kept in factored form, as `ncomp` columns of each factor.

## References

- Wang, Y., Veltkamp, D.J., Kowalski, B.R. (1991). Multivariate
  instrument standardization. Analytical Chemistry, 63(23):2750-2756.

## See also

[`pds()`](https://christiangoueguel.com/specProc/reference/pds.md),
[`sst()`](https://christiangoueguel.com/specProc/reference/sst.md),
[`slope_bias()`](https://christiangoueguel.com/specProc/reference/slope_bias.md),
[`predict.specproc_transfer()`](https://christiangoueguel.com/specProc/reference/predict.specproc_transfer.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- as.matrix(forageLIBS[which(wl > 760 & wl < 780)])  # the K I lines
# a second instrument, with a shifted and less sensitive response
slave <- 0.8 * cbind(spectra[, -1], spectra[, ncol(spectra)]) + 50
transfer <- ds(spectra[1:30, ], slave[1:30, ], ncomp = 10)
transfer
#> Direct standardization (30 transfer samples, 245 wavelengths)
#> Components: 10
# the other samples, measured on the slave, transferred to the master
transferred <- predict(transfer, slave[31:368, ])
mean(abs(transferred - spectra[31:368, ])) / mean(spectra)
#> [1] 0.02327181
```
