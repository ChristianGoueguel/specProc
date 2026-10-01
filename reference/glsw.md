# Generalized Least Squares Weighting

The Generalized Least Squares Weighting (GLSW) algorithm, proposed by
Martens *et al.* (2003), is a technique used to mitigate the effects of
external interferences in datasets. It constructs a filter to remove
these interferences, allowing for more accurate data analysis and
processing.

## Usage

``` r
glsw(x1, x2, alpha = 0.01)
```

## Arguments

- x1:

  A numeric matrix, data frame or tibble representing the first set of
  data.

- x2:

  A numeric matrix, data frame or tibble representing the second set of
  data.

- alpha:

  A positive numeric value specifying the weighting parameter. Typical
  values range from 1 to 0.0001. Default is 0.01.

## Value

A tibble containing the \\p \times p\\ filtering matrix \\\textbf{G}\\.

## Details

The algorithm works by first calculating a covariance matrix from the
differences between two spectral datasets that should ideally be
similar. These differences are considered to be the interferences or
clutter present in the data. For example, if two sets of measurements
have been taken under similar conditions, the differences between them
could be attributed to external factors such as sensor noise,
environmental conditions, or other sources of interference. Once the
covariance matrix is calculated, GLSW applies a filtering matrix to
down-weight the contributions of the identified interferences or
clutter. This filtering matrix is constructed using a regularization
parameter, denoted as alpha (\\\alpha\\).

With \\\textbf{X}\_d\\ the difference between the mean-centered
datasets, \\\textbf{C} = \textbf{X}\_d^T\textbf{X}\_d =
\textbf{V}\textbf{S}^2\textbf{V}^T\\, the filter is \$\$\textbf{G} =
\textbf{V}\textbf{D}^{-1}\textbf{V}^T, \quad \textbf{D} =
\sqrt{\textbf{S}^2/\alpha + \textbf{I}}\$\$ where \\\textbf{S}^2\\ holds
the eigenvalues of \\\textbf{C}\\.

The value of \\\alpha\\ determines how strongly the algorithm
down-weights the clutter components in the data. In cases where the
interferences are well-characterized and distinct from the desired
signal, a small \\\alpha\\ value may be appropriate to achieve effective
clutter removal. However, if the interferences are more subtle or
intertwined with the desired signal, a larger \\\alpha\\ value may be
preferred to avoid over-suppression of the signal itself.

Let \\\textbf{X}\\ be a data matrix. The GLSW filter is applied as
follows:

\$\$\textbf{X}\_{new} = \textbf{X} \cdot \textbf{G}\$\$

where, \\\textbf{X}\_{new}\\ is the filtered matrix and \\\textbf{G}\\
the filtering matrix.

## References

- Martens, H., Hoy, M., Wise, B.M., Bro, R., Brockhoff, P.B., (2003).
  Pre-whitening of data by covariance-weighted preprocessing. Journal of
  Chemometrics, 17(3):153-165

## See also

[`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md)
to use the filter in a tidymodels recipe.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]
wl <- as.numeric(names(spectra))
x <- spectra[wl > 760 & wl < 780]  # the K I resonance lines
# the three samples measured twice
twice <- forageLIBS$Sample[duplicated(forageLIBS$Sample)]
first <- match(twice, forageLIBS$Sample)
second <- vapply(twice, function(s) max(which(forageLIBS$Sample == s)), integer(1))
G <- glsw(x[first, ], x[second, ], alpha = 0.01)
filtered <- as.matrix(x) %*% as.matrix(G)
dim(filtered)
#> [1] 368 245
```
