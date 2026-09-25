# Multiplicative Signal Correction

This function performs multiplicative scatter/signal correction (MSC) on
an input data of spectra. It corrects for multiplicative and additive
effects in the spectral data by regressing against a reference spectrum.

## Usage

``` r
msc(
  x,
  xref = NULL,
  drop.offset = TRUE,
  robust = TRUE,
  window = NULL,
  drop.na = TRUE
)
```

## Arguments

- x:

  A numeric matrix or data frame containing the input spectra. Each row
  represents a sample, and each column represents a spectral variable.

- xref:

  An optional numeric vector representing the reference spectrum. If
  `NULL` (default), the mean or median of `x` is used as the reference.

- drop.offset:

  A logical value indicating whether the additive offset \\a_i\\ should
  be removed from the spectra (default is `TRUE`). If `FALSE`, only the
  multiplicative effect is corrected: \\\textbf{x}\_i / b_i\\.

- robust:

  A logical value indicating whether the median (`TRUE`, default) or the
  mean (`FALSE`) of `x` is used as `xref`.

- window:

  An optional list of numeric vectors specifying the column indices of
  spectral windows. If provided, MSC is performed separately for each
  window.

- drop.na:

  A logical value indicating whether to remove spectra (rows) containing
  missing values. If `TRUE` (the default), such rows are removed.

## Value

A list with the following components:

- `correction`:

  The corrected spectra.

- `offset`:

  The intercepts/offsets \\a_i\\ (a matrix with one column per window
  when `window` is given).

- `slope`:

  The multiplicative scatter factors/slopes \\b_i\\ (a matrix with one
  column per window when `window` is given).

- `reference`:

  The reference spectrum used.

## Details

Each spectrum \\\textbf{x}\_i\\ is regressed against the reference
spectrum, \\\textbf{x}\_i = a_i + b_i \textbf{x}\_{ref} +
\textbf{e}\_i\\, and corrected as \\(\textbf{x}\_i - a_i) / b_i\\. When
`window` is given, the regression and correction are carried out
separately in each spectral window (piecewise MSC); variables outside
all windows are left unchanged.

To correct new spectra consistently, pass the returned `reference` as
`xref`.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
base <- sin(seq(0, pi, length.out = 50))
x <- t(sapply(1:10, function(i) runif(1, 0, 1) + runif(1, 0.5, 2) * base))
res <- msc(x)
range(apply(as.matrix(res$correction), 2, sd))
#> [1] 1.046728e-16 3.051711e-16
```
