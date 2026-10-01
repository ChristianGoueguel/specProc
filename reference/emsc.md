# Extended Multiplicative Signal Correction

This function implements extended multiplicative signal correction
(EMSC), as proposed by Martens and Stark (1991). EMSC extends
[`msc()`](https://christiangoueguel.com/specProc/reference/msc.md) by
modeling, in addition to the multiplicative and additive effects, a
smooth polynomial baseline and, optionally, known interferent spectra.

## Usage

``` r
emsc(
  x,
  xref = NULL,
  degree = 2,
  interferents = NULL,
  wavelength = NULL,
  robust = TRUE
)
```

## Arguments

- x:

  A numeric matrix or data frame, with one spectrum per row.

- xref:

  An optional numeric vector giving the reference spectrum. If `NULL`
  (default), the median (`robust = TRUE`) or mean spectrum of `x` is
  used.

- degree:

  A non-negative integer giving the degree of the polynomial baseline.
  Default is 2. With 0, only a constant offset is modeled.

- interferents:

  An optional numeric vector (a single spectrum), matrix or data frame
  of interferent spectra, one per row, with the same number of columns
  as `x`.

- wavelength:

  An optional numeric vector of wavelengths used to build the
  polynomials. If `NULL` (default), the column names of `x` are used
  when they are numeric, and the column indices otherwise.

- robust:

  A logical value indicating whether the median (`TRUE`, default) or the
  mean spectrum is used as the reference.

## Value

An object of class `specproc_emsc` (a list), which
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_emsc.md)
applies to new spectra, with the following components:

- `correction`: The corrected spectra.

- `coefficients`: A tibble with the estimated coefficients of each
  spectrum: `slope` (\\b_i\\), `offset` (\\a_i\\), `poly1`, ...
  (\\d\_{ik}\\) and `interferent1`, ... (\\h\_{ij}\\).

- `reference`: The reference spectrum.

- `design`: The design matrix (one column per model term).

## Details

Each spectrum \\\textbf{x}\_i\\ is modeled as a linear combination of a
reference spectrum \\\textbf{m}\\, polynomials of the wavelength
\\\lambda\\ and interferent spectra \\\textbf{k}\_j\\: \$\$\textbf{x}\_i
= b_i\textbf{m} + a_i + d\_{i1}\lambda + d\_{i2}\lambda^2 + \dots +
\sum_j h\_{ij}\textbf{k}\_j + \textbf{e}\_i\$\$ The coefficients are
estimated by least squares, and the corrected spectrum is
\$\$\textbf{x}\_i^{corr} = (\textbf{x}\_i - a_i - d\_{i1}\lambda -
\dots - \sum_j h\_{ij}\textbf{k}\_j) / b_i\$\$ With `degree = 0` and no
interferents, EMSC is identical to
[`msc()`](https://christiangoueguel.com/specProc/reference/msc.md) with
`drop.offset = TRUE`.

The wavelengths are rescaled to \\\[-1, 1\]\\ before the polynomials are
formed, for numerical stability. Interferent spectra describe variation
that should be removed, for example the spectrum of a contaminant or the
dominant directions of repeated measurements of the same samples (see
[`epo()`](https://christiangoueguel.com/specProc/reference/epo.md)).

The reference spectrum is estimated from `x` unless `xref` is given, so
new spectra should be corrected with
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_emsc.md),
which uses the reference and design of the calibration data.

## References

- Martens, H., Stark, E. (1991). Extended multiplicative signal
  correction and spectral interference subtraction: new preprocessing
  methods for near infrared spectroscopy. Journal of Pharmaceutical and
  Biomedical Analysis, 9(8):625-635.

- Afseth, N.K., Kohler, A. (2012). Extended multiplicative signal
  correction in vibrational spectroscopy, a tutorial. Chemometrics and
  Intelligent Laboratory Systems, 117:92-99.

## See also

[`msc()`](https://christiangoueguel.com/specProc/reference/msc.md),
[`step_emsc()`](https://christiangoueguel.com/specProc/reference/step_emsc.md)
to use EMSC in a tidymodels recipe.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]  # the spectral channels
fit <- emsc(spectra[1:300, ], degree = 2)
head(fit$coefficients)
#> # A tibble: 6 × 4
#>   slope offset  poly1  poly2
#>   <dbl>  <dbl>  <dbl>  <dbl>
#> 1 1.06   -89.9 104.    152. 
#> 2 1.02    35.2  88.9   -36.2
#> 3 1.05    58.3   9.46 -106. 
#> 4 1.13    41.2 -77.5  -310. 
#> 5 0.894   99.9  79.5    61.2
#> 6 1.000   54.8  32.3   -35.6
# new spectra are corrected with the calibration reference
corrected <- predict(fit, spectra[301:368, ])
dim(corrected)
#> [1]   68 7152
```
