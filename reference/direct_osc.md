# Direct Orthogonal Signal Correction

This function implements the Direct Orthogonal Signal Correction (DOSC)
algorithm, as proposed by Westerhuis *et al.* (2001), to remove
systematic variation from predictor variables, \\\textbf{X}\\, that is
orthogonal to the response variable(s), \\\textbf{Y}\\.

## Usage

``` r
direct_osc(x, y, ncomp = 10, center = TRUE, scale = FALSE, tol = 0.001)
```

## Arguments

- x:

  A matrix or data frame of the predictor variables

- y:

  A vector, matrix or data frame of the response variable(s)

- ncomp:

  An integer specifying the number of orthogonal components to remove.
  Default is 10; it is reduced if larger than the rank of the
  orthogonalized matrix.

- center:

  A logical value specifying whether to center the data. Default is
  `TRUE`.

- scale:

  A logical value specifying whether to scale the data. Default is
  `FALSE`.

- tol:

  A numeric value giving the relative tolerance used to compute the
  pseudo-inverse of \\\textbf{X}\\; singular values smaller than `tol`
  times the largest one are discarded, which regularizes the weights.
  Default is 1e-3.

## Value

A list with the following components:

- `correction`: The corrected matrix.

- `loading`: The loadings matrix \\\textbf{P}\\.

- `score`: The scores matrix \\\textbf{T}\\.

- `weight`: The weights matrix \\\textbf{W}\\.

- `center`, `scale`: The column centers and scales applied to `x`.

## Details

Different from the Orthogonal Signal Correction (OSC) algorithm, Wold
*et al.* (1998), the DOSC algorithm is non-iterative:

1.  \\\textbf{Y}\\ is projected onto the column space of \\\textbf{X}\\:
    \\\hat{\textbf{Y}} = \textbf{XX}^{+}\textbf{Y}\\.

2.  \\\textbf{X}\\ is orthogonalized with respect to
    \\\hat{\textbf{Y}}\\: \\\textbf{Z} = \textbf{X} -
    \hat{\textbf{Y}}\hat{\textbf{Y}}^{+}\textbf{X}\\.

3.  PCA of \\\textbf{Z}\\ gives the orthogonal scores \\\textbf{T}\\.

4.  The weights \\\textbf{W} = \textbf{X}^{+}\textbf{T}\\ express the
    scores as a linear combination of \\\textbf{X}\\, the loadings are
    \\\textbf{P} =
    \textbf{X}^T\textbf{T}(\textbf{T}^T\textbf{T})^{-1}\\, and the
    corrected matrix is \\\textbf{X} - \textbf{TP}^T\\.

New data are corrected with \\\textbf{X}\_{new} -
\textbf{X}\_{new}\textbf{WP}^T\\ after applying the returned `center`
and `scale`.

## References

- Westerhuis, J.A., Jong, S.D., Smilde, A.K., (2001). Direct orthogonal
  signal correction. Chemometrics Intell. Lab. Syst., 56(1):13-25

- Wold, S., Antti, H., Lindgren, F., Ohman, J. (1998). Orthogonal signal
  correction of near-infrared spectra. Chemometrics Intell. Lab. Syst.,
  44(1):175-185.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(20 * 50), 20, 50)
y <- x[, 1] + rnorm(20, sd = 0.1)
res <- direct_osc(x, y, ncomp = 2)
dim(res$correction)
#> [1] 20 50
```
