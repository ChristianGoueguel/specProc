# Orthogonal Projections to Latent Structures

This function fits an Orthogonal Projections to Latent Structures (OPLS)
model to the provided x (predictor) and y (response) data.

## Usage

``` r
opls(x, y, scale = "center", crossval = 7, permutation = 20, ncomp.ortho = NA)
```

## Arguments

- x:

  A numeric matrix or data frame of the predictor variables.

- y:

  A numeric vector, or a matrix or data frame with one column, of the
  response variable.

- scale:

  A character string indicating the scaling method for x and y: "none",
  "center" (default), "pareto" (divided by the square root of the
  standard deviation) or "standard" (divided by the standard deviation).

- crossval:

  An integer giving the number of cross-validation groups (default 7),
  between 2 and the number of observations. With `crossval = 0`, the
  model is not cross-validated (\\Q^2\\ is `NA`), which requires a fixed
  `ncomp.ortho` and no permutation.

- permutation:

  An integer giving the number of permutations for the permutation test.
  Default is 20; 0 skips the test.

- ncomp.ortho:

  The number of orthogonal components. If `NA` (default), it is
  determined automatically by cross-validation.

## Value

An object of class `specproc_opls` (a list), which
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_opls.md)
applies to new data, with the following components:

- x_scores:

  The predictive scores \\\textbf{t}\\ (one column, `p1`).

- x_loadings:

  The predictive loadings \\\textbf{p}\\.

- x_weights:

  The predictive weights \\\textbf{w}\\.

- orthoScores:

  The orthogonal scores \\\textbf{T}\_o\\ (columns `o1`, `o2`, ...).

- orthoLoadings:

  The orthogonal loadings \\\textbf{P}\_o\\.

- orthoWeights:

  The orthogonal weights \\\textbf{W}\_o\\.

- y_weights:

  The y-weight \\c\\ of the predictive component.

- y_scores:

  The y-scores \\\textbf{u} = \textbf{y}/c\\.

- correction:

  The OPLS-filtered x, \\\textbf{X} - \textbf{T}\_o\textbf{P}\_o^T\\
  (preprocessed).

- fitted:

  The fitted response, in the units of y.

- coefficients:

  The regression coefficients of the preprocessed filtered x,
  \\\textbf{w}c\\.

- vip, ortho_vip:

  The predictive and orthogonal variable importance in projection
  (Galindo-Prieto et al., 2014), one value per variable.

- components:

  A tibble with one row per component: `R2X`, `R2Y` and `Q2` (the
  increase due to the component), their cumulative values, and
  `significance` (`"R1"` significant, `"NS"` \\Q^2\\ increase below
  0.01, `"N4"` \\R^2Y\\ increase below 0.01).

- summary:

  A one-row data frame with `R2X(cum)`, `R2Y(cum)`, `Q2(cum)`, `RMSEE`
  (root mean squared error of estimation), the numbers of predictive
  (`pre`) and orthogonal (`ort`) components and, with a permutation
  test, `pR2Y` and `pQ2`.

- permutation:

  With a permutation test, a tibble of `R2Y(cum)`, `Q2(cum)` and `sim`
  (the correlation between the permuted and the original response) of
  the model (first row) and of each permuted model.

- center, scale:

  The column centers and scales applied to x.

- y_center, y_scale:

  The center and scale applied to y.

## Details

OPLS is a supervised modeling technique used to find the
multidimensional direction in the x-space that explains the maximum
multidimensional variance in the y-space. It separates the systematic
variation in x into two parts: one that is linearly related to y
(predictive components) and one that is statistically uncorrelated to
the response variable y (orthogonal components).

The model has one predictive component and `ncomp.ortho` orthogonal
components, fitted with the NIPALS algorithm of Trygg and Wold (2002).
For each orthogonal component:

1.  The weight \\\textbf{w} =
    \textbf{X}^T\textbf{y}/\\\textbf{X}^T\textbf{y}\\\\, the score
    \\\textbf{t} = \textbf{Xw}\\ and the loading \\\textbf{p} =
    \textbf{X}^T\textbf{t}/(\textbf{t}^T\textbf{t})\\ are computed.

2.  The orthogonal weight is the part of \\\textbf{p}\\ orthogonal to
    \\\textbf{w}\\, \\\textbf{w}\_o = \textbf{p} -
    (\textbf{w}^T\textbf{p})\textbf{w}\\, normalized, with score
    \\\textbf{t}\_o = \textbf{Xw}\_o\\ and loading \\\textbf{p}\_o =
    \textbf{X}^T\textbf{t}\_o/(\textbf{t}\_o^T\textbf{t}\_o)\\.

3.  \\\textbf{X}\\ is deflated by \\\textbf{t}\_o\textbf{p}\_o^T\\.

The predictive component is then computed from the filtered
\\\textbf{X}\\. The filtered data are the same as those of
[`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
with `ncomp = ncomp.ortho + 1`, and of
[`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md).

**Cross-validation.** The observations are split into `crossval`
interleaved groups (observation `i` is in group
`(i - 1) %% crossval + 1`). Each group is predicted by a model fitted to
the others, with the same number of orthogonal components, giving the
predictive residual sum of squares (PRESS) and \\Q^2 = 1 -
\text{PRESS}/\text{SS}\_y\\, where \\\text{SS}\_y\\ is the total sum of
squares of the preprocessed y. The data are preprocessed once, with the
centers and scales of all the observations.

**Number of orthogonal components.** If `ncomp.ortho = NA`, components
are added (up to `min(10, n, p) - 1`) while each is significant: a
component is significant if it increases \\R^2Y\\ by at least 0.01 and
\\Q^2\\ by at least 0.01. If the predictive component alone is not
significant, no model is built. If the first orthogonal component is not
significant, the model has no orthogonal component (a one-component PLS
model), with a warning.

**Permutation test.** The response is permuted `permutation` times and
the model refitted with the same number of components. `pR2Y` and `pQ2`
are the proportions of permuted models whose \\R^2Y\\ and \\Q^2\\ are at
least those of the model, \\(1 + \\\\\text{perm} \geq
\text{model}\\)/\text{permutation}\\. Use
[`set.seed()`](https://rdrr.io/r/base/Random.html) for reproducible
p-values.

The results reproduce those of
[`ropls::opls()`](https://rdrr.io/pkg/ropls/man/opls.html) with
`predI = 1` and the same `scaleC`, `crossvalI`, `orthoI` and `permI`
(apart from rounding): specProc no longer depends on ropls. Unlike
ropls, variables with zero variance are kept (their centered values are
zero), and when `ncomp.ortho = NA` a model without orthogonal components
is returned instead of no model when only the predictive component is
significant.

## References

- Trygg, J., and Wold, S., (2002). Orthogonal projections to latent
  structures (O-PLS). Journal of Chemometrics, 16(3):119-128.

- Galindo-Prieto, B., Eriksson, L., and Trygg, J., (2014). Variable
  influence on projection (VIP) for orthogonal projections to latent
  structures (OPLS). Journal of Chemometrics, 28(8):623-632.

- Thévenot, E.A., Roux, A., Xu, Y., Ezan, E., and Junot, C., (2015).
  Analysis of the human adult urinary metabolome variations with age,
  body mass index, and gender by implementing a comprehensive workflow
  for univariate and OPLS statistical analyses. Journal of Proteome
  Research, 14(8):3322-3335.

## See also

[`predict.specproc_opls()`](https://christiangoueguel.com/specProc/reference/predict.specproc_opls.md),
and
[`step_opls()`](https://christiangoueguel.com/specProc/reference/step_opls.md)
to use the OPLS filter in a tidymodels recipe.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(40 * 30), 40, 30)
y <- x[, 1] + rnorm(40, sd = 0.1)
x[, 2:5] <- x[, 2:5] + rnorm(40, sd = 3)  # response-orthogonal variation
fit <- opls(x[1:30, ], y[1:30], permutation = 10)
fit
#> Orthogonal projections to latent structures (OPLS)
#> 
#> Variables:               30
#> Observations:            30
#> Scaling:                 center
#> Predictive components:   1
#> Orthogonal components:   3
#> 
#>       R2X(cum) R2Y(cum) Q2(cum) RMSEE pR2Y pQ2
#> Total    0.677     0.95   0.473 0.224  0.1 0.1
#> 
#> Use predict(<model>, newdata, type = ) to filter new data or predict the response.
predict(fit, x[31:40, ], type = "response")
#>  [1]  0.98911093 -0.21520551 -0.82803563  0.08133964 -0.26928032 -1.03304174
#>  [7] -0.43825878  0.21948811  0.39230090  0.96764347
```
