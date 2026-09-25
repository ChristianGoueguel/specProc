# Orthogonal Signal Correction

This function implements three orthogonal signal correction (OSC)
algorithms, which are a class of preprocessing techniques designed to
minimize, in a set of spectral data, the systematic variability or noise
not directly related to or correlated with the response vector or
property of interest.

## Usage

``` r
osc(
  x,
  y,
  method = "sjoblom",
  center = TRUE,
  scale = FALSE,
  ncomp = 10,
  tol = 0.001,
  max.iter = 10,
  pls.ncomp = 5
)
```

## Arguments

- x:

  A matrix or data frame of the predictor variables.

- y:

  A vector (or one-column matrix/data frame) of the response variable.

- method:

  A character string indicating the OSC method to use. Accepted values
  are `"wold"`, `"sjoblom"` and `"fearn"`. Default is `"sjoblom"`.

- center:

  A logical value indicating whether to mean-centered `x` and `y`.
  Default is `TRUE`.

- scale:

  A logical value indicating whether to scale `x` and `y`. Default is
  `FALSE`.

- ncomp:

  An integer representing the number of orthogonal components to remove.
  Default value is 10; it is reduced to `min(n - 1, p)` if larger.

- tol:

  A numeric value representing the tolerance for convergence. The
  default value is 1e-3.

- max.iter:

  An integer representing the maximum number of iterations. The default
  value is 10.

- pls.ncomp:

  An integer giving the number of PLS components used to compute the
  weights in Wold's method. Default is 5.

## Value

A list containing the following components:

- `correction`: The corrected matrix.

- `scores`: The orthogonal scores matrix.

- `loadings`: The orthogonal loadings matrix.

- `weights`: The orthogonal weights matrix.

- `R2`: The percentage of the (preprocessed) variance of `x` remaining
  after correction.

- `angle`: The mean angle (in degrees) between the orthogonal scores and
  `y`; values close to 90 indicate orthogonality.

- `center`, `scale`: The column centers and scales applied to `x`.

## Details

The OSC algorithm identifies and removes the orthogonal variation in the
input spectral matrix, \\\textbf{X}\\, by iteratively deflating
\\\textbf{X}\\ with respect to the response vector \\\textbf{y}\\. The
resulting \\\textbf{X}\\-matrix contains only the variation that is
relevant to the \\\textbf{y}\\-vector, which can then be used for
further modeling or analysis. This function implements three different
methods for OSC:

- `"wold"`: the original method of Wold *et al.* (1998). The first
  principal component score is orthogonalized to \\\textbf{y}\\ and the
  weights are obtained by a PLS regression of the orthogonalized score
  on \\\textbf{X}\\, iterating until convergence.

- `"sjoblom"`: the method of Sjöblom *et al.* (1998), where the weights
  are obtained by a simple least-squares regression instead of PLS.

- `"fearn"`: the non-iterative method of Fearn (2000), which finds the
  directions of maximum variance of \\\textbf{X}\\ subject to the scores
  being exactly orthogonal to \\\textbf{y}\\.

For the iterative methods, \\\textbf{X}\\ is deflated after each
component. New data can be corrected by applying the returned `center`
and `scale` and then, for each component \\i\\, computing
\\\textbf{t}\_i = \textbf{X}\_{new}\textbf{w}\_i\\ and
\\\textbf{X}\_{new} \leftarrow \textbf{X}\_{new} -
\textbf{t}\_i\textbf{p}\_i^T\\.

## References

- Sjöblom, J., Svensson, O., Josefson, M., Kullberg, H., Wold, S.,
  (1998). An evaluation of orthogonal signal correction applied to
  calibration transfer of near infrared spectra. Chemometrics Intell.
  Lab. Syst., 44(1):229-244.

- Fearn, T., (2000). On orthogonal signal correction. Chemometrics
  Intell. Lab. Syst., 50(1):47-52.

- Wold, S., Antti, H., Lindgren, F., Ohman, J. (1998). Orthogonal signal
  correction of near-infrared spectra. Chemometrics Intell. Lab. Syst.,
  44(1):175-185.

- Svensson, O., Kourti, T. and MacGregor, J.F., (2002). An investigation
  of orthogonal correction algorithms and their characteristics. Journal
  of Chemometrics, 16(1):176-188.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(20 * 50), 20, 50)
y <- x[, 1] + rnorm(20, sd = 0.1)
res <- osc(x, y, method = "fearn", ncomp = 2)
res$angle
#> [1] 90
```
