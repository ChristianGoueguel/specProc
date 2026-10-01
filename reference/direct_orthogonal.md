# Direct Orthogonalization

This function implements the Direct Orthogonalization (DO) algorithm, as
proposed by Andersson (1999), to filter out variation from predictor
variables, \\\textbf{X}\\, that is orthogonal to the response
variable(s), \\\textbf{Y}\\.

## Usage

``` r
direct_orthogonal(x, y, ncomp = 2, center = TRUE, scale = FALSE)
```

## Arguments

- x:

  A matrix or data frame of the predictor variables

- y:

  A vector, matrix or data frame of the response variable(s)

- ncomp:

  An integer specifying the number of principal components to retain for
  orthogonal processing. Default is 2. Values larger than the rank of
  the orthogonalized matrix are reduced accordingly.

- center:

  A logical value specifying whether to center the data. Default is
  `TRUE`.

- scale:

  A logical value specifying whether to scale the data. Default is
  `FALSE`.

## Value

An object of class `specproc_direct_orthogonal` (a list), which
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
applies to new spectra, with the following components:

- `correction`: The corrected matrix.

- `loading`: The loadings matrix \\\textbf{P}\\.

- `score`: The scores matrix \\\textbf{XP}\\.

- `center`, `scale`: The column centers and scales applied to `x`.

## Details

Contrary to the Orthogonal Signal Correction (OSC) algorithm, Wold *et
al.* (1998), which uses inverse Partial Least Squares (PLS) regression
to filter out the orthogonal signal, DO filters out the orthogonal
signal directly by orthogonalization of the \\\textbf{X}\\ matrix:
\$\$\textbf{Z} = \textbf{X} -
\textbf{Y}(\textbf{Y}^T\textbf{Y})^{-1}\textbf{Y}^T\textbf{X}\$\$
Principal Components Analysis (PCA) is performed on \\\textbf{Z}\\ to
obtain the loadings \\\textbf{P}\\, and the corrected matrix is
\\\textbf{X}\_{DO} = \textbf{X} - \textbf{XPP}^T\\. Direct
Orthogonalization is typically simpler and faster than OSC.

To correct new data, preprocess it with the returned `center` and
`scale` vectors and apply \\\textbf{X}\_{new} -
\textbf{X}\_{new}\textbf{PP}^T\\.

## References

- Andersson, C.A., (1999). Direct orthogonalization. Chemometrics
  Intell. Lab. Syst., 47(1):51-63

- Pierna, J.A.F., Massart, D.L., de Noord, O.E., Ricoux, P., (2001).
  Direct orthogonalization: some case studies. Chemometrics Intell. Lab.
  Syst., 55(1-2):101-108

- Wold, S., Antti, H., Lindgren, F., Ohman, J. (1998). Orthogonal signal
  correction of near-infrared spectra. Chemometrics Intell. Lab. Syst.,
  44(1):175-185.

## See also

[`predict.specproc_filter()`](https://christiangoueguel.com/specProc/reference/predict.specproc_filter.md)
to correct new spectra with the fitted filter,
[`step_direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/step_direct_orthogonal.md)
to use it in a tidymodels recipe. Before specProc 0.4.0,
[`nas()`](https://christiangoueguel.com/specProc/reference/nas.md)
returned the same correction; it now computes the net analyte signal and
figures of merit.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]  # the spectral channels
res <- direct_orthogonal(spectra, forageLIBS$K, ncomp = 2)
dim(res$correction)
#> [1]  368 7152
```
