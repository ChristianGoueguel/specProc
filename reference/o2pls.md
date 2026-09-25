# Modified Orthogonal Projections to Latent Structures

This function implements the modified orthogonal projections to latent
structures (O2PLS) algorithm as proposed by Trygg (2002). The OPLS and
O2PLS methods differ as follows: OPLS is unidirectional \\(X \Rightarrow
Y)\\, meaning that only orthogonal variations in the \\X\\-space are
filtered out. Whilst O2PLS is bi-directional \\(X \Leftrightarrow Y)\\,
meaning that orthogonal variations in both the \\X\\- and \\Y\\-space
are filtered out.

## Usage

``` r
o2pls(x, y, ncomp = 1, nx = 1, ny = 0, center = TRUE, scale = FALSE)
```

## Arguments

- x:

  A numeric matrix or data frame representing the predictor variables.

- y:

  A numeric vector, matrix or data frame representing the response
  variables.

- ncomp:

  An integer giving the number of joint (predictive) components. Default
  is 1.

- nx:

  An integer giving the number of \\Y\\-orthogonal components removed
  from `x`. Default is 1.

- ny:

  An integer giving the number of \\X\\-orthogonal components removed
  from `y`. Default is 0.

- center:

  A logical value indicating whether to mean-centered `x` and `y`.
  Default is `TRUE`.

- scale:

  A logical value indicating whether to scale `x` and `y`. Default is
  `FALSE`.

## Value

An object of class `o2pls`, a list containing:

- `correction`: The filtered `x` (\\Y\\-orthogonal variation removed).

- `correction_y`: The filtered `y` (\\X\\-orthogonal variation removed).

- `scores`: A list with the joint scores `x` (\\\textbf{T}\\) and `y`
  (\\\textbf{U}\\), and the orthogonal scores `x_ortho` and `y_ortho`.

- `loadings`: A list with the joint loadings `x` and `y`, and the
  orthogonal loadings `x_ortho` and `y_ortho`.

- `weights`: A list with the joint weights `x` (\\\textbf{W}\\) and `y`
  (\\\textbf{C}\\), and the orthogonal weights `x_ortho` and `y_ortho`.

- `center`, `scale`: The column centers and scales applied to `x`.

## Details

The O2PLS method handles situations where systematic \\X\\-orthogonal
variation in \\Y\\ exists, and it is predictive in both ways, \\(X
\Rightarrow Y)\\ and \\(Y \Rightarrow X)\\. The systematic part in \\X\\
and \\Y\\ is divided into two parts, one which is related to both \\X\\
and \\Y\\ (joint/covarying) and one that is not (orthogonal).

The algorithm follows Trygg and Wold (2003):

1.  The joint weights \\\textbf{W}\\ and \\\textbf{C}\\ are the dominant
    singular vectors of \\\textbf{Y}^T\textbf{X}\\.

2.  For each \\Y\\-orthogonal component of \\X\\, the weight
    \\\textbf{w}\_{o}\\ is the dominant singular vector of
    \\\textbf{E}\_{XY}^T\textbf{T}\\ (with \\\textbf{T} = \textbf{XW}\\
    and \\\textbf{E}\_{XY} = \textbf{X} - \textbf{TW}^T\\), and
    \\\textbf{X}\\ is deflated by \\\textbf{t}\_o\textbf{p}\_o^T\\.

3.  The same procedure is applied to \\Y\\ for the \\X\\-orthogonal
    components.

4.  The joint components are recomputed from the filtered data.

For a single response variable, `ny` must be 0 and the method is
equivalent to OPLS with `nx` orthogonal components.

## References

- Trygg, J., (2002). O2-PLS for qualitative and quantitative analysis in
  multivariate calibration. J. Chemom. 16(1):283–293.

- Trygg, J., Wold, S., (2003). O2-PLS, a two-block (X–Y) latent variable
  regression (LVR) method with an integral OSC filter. J. Chemom.
  17(1):53–64.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(30 * 40), 30, 40)
y <- x[, 1:2] %*% c(1, -1) + rnorm(30, sd = 0.1)
fit <- o2pls(x, y, ncomp = 1, nx = 2)
fit
#> An object of class 'o2pls'
#> 
#> Joint components:        1
#> X-orthogonal components: 2
#> Y-orthogonal components: 0
#> 
#> - correction:  30 x 40
#> - X weights:   40 x 1
#> - Y weights:   1 x 1
#> - X scores:    30 x 1
#> - X ortho:     30 x 2
```
