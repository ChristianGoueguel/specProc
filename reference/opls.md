# Orthogonal Projections to Latent Structures

This function fits an Orthogonal Projections to Latent Structures (OPLS)
model to the provided x (predictor) and y (response) data.

## Usage

``` r
opls(x, y, scale = "center", crossval = 7, permutation = 20, ncomp.ortho = NA)
```

## Arguments

- x:

  A data.frame or tibble containing the x-data (predictors).

- y:

  A data.frame or tibble containing the y-data (responses).

- scale:

  A character string indicating the scaling method for the data: "none",
  "center", "pareto" or "standard".

- crossval:

  An integer representing the number of cross-validation groups.

- permutation:

  An integer representing the number of permutations for the permutation
  test.

- ncomp.ortho:

  The number of orthogonal components. If `NA` (default), it is
  determined automatically by cross-validation.

## Value

A list containing the following components:

- x_scores:

  A matrix of x-scores (the projections of the x-data onto the
  predictive components).

- x_loadings:

  A matrix of x-loadings (the weights of the original x-variables on the
  predictive components).

- x_weights:

  A matrix of x-weights (the weights used to calculate the x-scores).

- orthoScores:

  A matrix of orthogonal scores (the projections of the x-data onto the
  orthogonal components).

- orthoLoadings:

  A matrix of orthogonal loadings (the weights of the original
  x-variables on the orthogonal components).

- orthoWeights:

  A matrix of orthogonal weights (the weights used to calculate the
  orthogonal scores).

- y_weights:

  A matrix of y-weights.

- y_scores:

  A matrix of y-scores (the projections of the y-data onto the
  predictive components).

- summary:

  The model summary (R2X, R2Y, Q2, ...).

- model:

  The fitted `ropls` model object.

## Details

OPLS is a supervised modeling technique used to find the
multidimensional direction in the x-space that explains the maximum
multidimensional variance in the y-space. It separates the systematic
variation in x into two parts: one that is linearly related to y
(predictive components) and one that is statistically uncorrelated to
the response variable y (orthogonal components).

The function is a wrapper around
[`ropls::opls()`](https://rdrr.io/pkg/ropls/man/opls.html) from the
Bioconductor package ropls, which must be installed
(`BiocManager::install("ropls")`). The model is fitted silently (no
printed summary or plots). For a dependency-free alternative that
returns the OPLS-filtered data, see
[`projected_osc()`](https://christiangoueguel.com/specProc/reference/projected_osc.md)
or
[`o2pls()`](https://christiangoueguel.com/specProc/reference/o2pls.md).

## References

- Trygg, J., and Wold, S., (2002). Orthogonal projections to latent
  structures (O-PLS). Journal of Chemometrics, 16(3):119-128.

## Author

Christian L. Goueguel
