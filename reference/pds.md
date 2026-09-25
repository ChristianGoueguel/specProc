# Piecewise Direct Standardization

The `pds` function performs Piecewise Direct Standardization (PDS), a
method proposed by Wang *et al.* (1991) to transfer spectra from one
instrument to another. Optionally, the local models can be blended with
a global PLSR model (direct standardization), which can lead to better
transfer performance when the instrumental differences are not purely
local.

## Usage

``` r
pds(x1, x2, win = 5, ncomp = 2, alpha = 0)
```

## Arguments

- x1:

  A matrix or data frame containing spectra acquired with the standard
  (master) instrument.

- x2:

  A matrix or data frame containing spectra of the same samples acquired
  with the instrument to be standardized (slave).

- win:

  An integer specifying the half size of the moving window used for
  PLSR. A larger value may improve the transfer but will increase
  computational time. Default is 5.

- ncomp:

  An integer specifying the number of components to be used in PLSR.
  Typically, a small number (e.g., 2-5) is sufficient. Default is 2.

- alpha:

  A numeric value between 0 and 1 specifying the weight for the global
  PLSR model. A value of 0 (default) corresponds to the original PDS
  method, while a value of 1 corresponds to using only the global PLSR
  model.

## Value

A list with two components:

- `transfer_matrix`:

  The \\p \times p\\ transfer matrix \\\textbf{F}\\.

- `intercept`:

  A vector of length \\p\\ containing the intercepts.

## Details

For each wavelength \\j\\ of the standard (master) instrument, a PLSR
model is fitted between the response of the master instrument at \\j\\
and the responses of the slave instrument in the window \\\[j - win, j +
win\]\\ (truncated at the edges of the spectrum). The regression
coefficients are stored in a banded transfer matrix \\\textbf{F}\\, and
the intercepts in a vector \\\textbf{b}\_0\\. Spectra measured on the
slave instrument are then standardized as \$\$\textbf{X}\_{2,std} =
\textbf{X}\_2 \textbf{F} + \textbf{1}\textbf{b}\_0^T\$\$

When `alpha > 0`, a global PLSR model mapping the whole slave spectrum
to the whole master spectrum is also fitted, and the transfer matrix and
intercept are \\(1 - \alpha)\\ times the local ones plus \\\alpha\\
times the global ones.

## References

- Wang, Y., Veltkamp, D.J., Kowalski, B.R., (1991). Multivariate
  instrument standardization. Analytical Chemistry, 63(23):2750-2756.

- Bouveresse, E., Massart, D.L., (1996). Improvement of the piecewise
  direct standardization procedure for the transfer of NIR spectra for
  multivariate calibration. Chemometrics and Intelligent Laboratory
  Systems, (32)2:201-213.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
wl <- seq(0, 1, length.out = 40)
x1 <- t(replicate(15, runif(1) * dnorm(wl, 0.5, 0.1) + runif(1)))
x2 <- 1.1 * x1 + 0.05                 # slave instrument: gain and offset
model <- pds(x1, x2, win = 3, ncomp = 2)
x2_std <- x2 %*% model$transfer_matrix +
  matrix(model$intercept, nrow(x2), ncol(x2), byrow = TRUE)
max(abs(x2_std - x1))
#> [1] 1.662764e-05
```
