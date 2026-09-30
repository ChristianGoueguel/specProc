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
set.seed(1)
wl <- seq(390, 400, length.out = 100)
pure <- exp(-(wl - 393.4)^2 / 0.05) + 0.6 * exp(-(wl - 396.8)^2 / 0.05)
# multiplicative effect, offset and a curved baseline for each spectrum
x <- t(sapply(1:10, function(i) {
  runif(1, 0.5, 2) * pure + runif(1, 0, 1) + runif(1, -1, 1) * ((wl - 395) / 5)^2
}))
colnames(x) <- wl

fit <- emsc(x, degree = 2)
head(fit$coefficients)
#> # A tibble: 6 × 4
#>   slope  offset     poly1   poly2
#>   <dbl>   <dbl>     <dbl>   <dbl>
#> 1 0.619  0.0781 -0.000211 -0.0270
#> 2 1.28  -0.408  -0.000437  0.439 
#> 3 1.32   0.0333 -0.000450 -0.110 
#> 4 0.409  0.0120 -0.000139 -0.761 
#> 5 1.06  -0.117  -0.000359  0.245 
#> 6 0.859  0.310  -0.000292  0.744 
# the corrected spectra are nearly identical
range(apply(as.matrix(fit$correction), 2, sd))
#> [1] 4.251830e-16 1.768865e-13

# new spectra are corrected with the calibration reference
corrected <- predict(fit, x[1:2, ])
```
