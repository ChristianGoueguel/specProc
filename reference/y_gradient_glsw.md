# y-Gradient Generalized Least Squares Weighting

The y-gradient generalized least squares weighting algorithm (GLSW)
removes variance from the data (spectra), which is orthogonal to the
response.

## Usage

``` r
y_gradient_glsw(x, y, alpha = 0.01, window = 5)
```

## Arguments

- x:

  A numeric matrix, data frame or tibble, representing the predictors
  data.

- y:

  A numeric vector representing the response vector.

- alpha:

  A positive numeric value specifying the weighting parameter. Typical
  values range from 1 to 0.0001. Default is 0.01.

- window:

  An odd integer giving the width of the Savitzky-Golay window used to
  compute the gradients. Default is 5.

## Value

A tibble containing the \\p \times p\\ filtering matrix.

## Details

The y-Gradient GLSW is an alternative method to GLSW, where a continuous
\\\textbf{y}\\-variable is used to develop pseudo-groupings of samples
in \\\textbf{X}\\ by comparing the differences in the corresponding
\\\textbf{y}\\ values. This is referred to as the *"gradient method"*
because it utilizes a gradient of the sorted \\\textbf{X}\\- and
\\\textbf{y}\\-blocks to calculate a covariance matrix.

The samples are sorted by increasing \\\textbf{y}\\, and the first
derivatives of \\\textbf{X}\\ and \\\textbf{y}\\ along the sample axis
are computed with a Savitzky-Golay filter. Samples whose neighbours have
similar \\\textbf{y}\\ values receive large weights \\w_i = 2^{-\Delta
y_i / s\_{\Delta y}}\\, so the differences between their spectra,
\\\Delta\textbf{X}\\, describe variation unrelated to \\\textbf{y}\\.
The filter is then built as in
[`glsw()`](https://christiangoueguel.com/specProc/reference/glsw.md)
from \\\textbf{C} = \Delta\textbf{X}^T\textbf{W}^2\Delta\textbf{X}\\.

## References

- Zorzetti, B.M., Shaver, J.M., Harynuk, J.J., (2011). Estimation of the
  age of a weathered mixture of volatile organic compounds. Analytica
  Chimica Acta, 694(1-2):31–37.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(30 * 20), 30, 20)
y <- x[, 1] + rnorm(30, sd = 0.1)
G <- y_gradient_glsw(x, y, alpha = 0.01)
x_filtered <- x %*% as.matrix(G)
```
