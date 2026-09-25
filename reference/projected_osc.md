# Projected Orthogonal Signal Correction

The projected orthogonal signal correction (POSC) method is a
preprocessing technique used to remove systematic variation from
predictor variables that is orthogonal to the response variable. This
function implements the POSC algorithm for model fitting and prediction.

## Usage

``` r
projected_osc(
  x,
  y,
  ncomp = 5,
  center = TRUE,
  scale = FALSE,
  tol = 1e-10,
  newdata = NULL
)
```

## Arguments

- x:

  A matrix or data frame of the predictor variables.

- y:

  A vector of the response variable.

- ncomp:

  An integer specifying the number of PLS components (at least 2).
  `ncomp - 1` orthogonal components are removed. Default is 5.

- center:

  A logical value indicating whether to mean-center `x` and `y`. Default
  is `TRUE`.

- scale:

  A logical value indicating whether to scale `x` and `y`. Default is
  `FALSE`.

- tol:

  A numeric value; orthogonal components whose singular value is smaller
  than `tol` times the largest one are discarded. The default value is
  1e-10.

- newdata:

  An optional matrix or data frame of new predictor variables to be
  corrected using the POSC model. It is preprocessed with the centers
  and scales of `x`.

## Value

A list containing the following components:

- `correction`: The corrected `x`.

- `scores`: The orthogonal scores matrix \\\textbf{T}\_o\\.

- `loadings`: The orthogonal loadings matrix \\\textbf{P}\_o\\.

- `weights`: The orthogonal weights \\\textbf{W}\_o\\, such that
  \\\textbf{T}\_o = \textbf{XW}\_o\\.

- `center`, `scale`: The column centers and scales applied to `x`.

- `newdata`: If `newdata` is provided, a list with the corrected new
  data (`correction`) and its orthogonal scores (`scores`).

## Details

POSC obtains OPLS-filtered data directly from an ordinary
(non-orthogonalized) PLS1 model (Kemsley and Tapp, 2009):

1.  A PLS1 model with `ncomp` components is fitted, giving the score
    matrix \\\textbf{T}\\ and the fitted response \\\hat{\textbf{y}}\\.

2.  The part of the score space orthogonal to \\\hat{\textbf{y}}\\,
    \\\textbf{T} -
    \hat{\textbf{y}}(\hat{\textbf{y}}^T\hat{\textbf{y}})^{-1}\hat{\textbf{y}}^T\textbf{T}\\,
    spans `ncomp - 1` orthogonal components with scores
    \\\textbf{T}\_o\\.

3.  The orthogonal loadings are \\\textbf{P}\_o =
    \textbf{X}^T\textbf{T}\_o(\textbf{T}\_o^T\textbf{T}\_o)^{-1}\\ and
    the filtered data are \\\textbf{X} - \textbf{T}\_o\textbf{P}\_o^T\\.

The filtered data are identical to those obtained from an OPLS model
with one predictive and `ncomp - 1` orthogonal components.

## References

- Kemsley, E.K., Tapp, H.S., (2009). OPLS filtered data can be obtained
  directly from non-orthogonalized PLS1. Journal of Chemometrics,
  23(5):263-264.

- Trygg, J., Wold, S., (2002). Orthogonal projections to latent
  structures (O-PLS). Journal of Chemometrics, 16(3):119-128.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(30 * 40), 30, 40)
y <- x[, 1] + rnorm(30, sd = 0.1)
res <- projected_osc(x[1:20, ], y[1:20], ncomp = 3, newdata = x[21:30, ])
dim(res$newdata$correction)
#> [1] 10 40
```
