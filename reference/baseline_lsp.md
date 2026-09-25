# Least-Squares Polynomial

This function performs baseline correction on the spectral matrix by
estimating and removing the continuous background emission using
least-squares polynomial curve fitting approach.

## Usage

``` r
baseline_lsp(x, degree = 4, tol = 0.001, max.iter = 10)
```

## Arguments

- x:

  A matrix or data frame, with one spectrum per row.

- degree:

  An integer specifying the degree of the polynomial fitting function.
  The default value is 4.

- tol:

  A numeric value representing the tolerance for the difference between
  iterations. The default value is 1e-3.

- max.iter:

  An integer specifying the maximum number of iterations for the
  algorithm. The default value is 10.

## Value

A list with two elements:

- `correction`: The baseline-corrected spectral matrix.

- `background`: The fitted background emission.

## Details

This function implements the algorithm described in Lieber and
Mahadevan-Jansen (2003), which smoothes the spectrum in such a way that
peaks are automatically eliminated, leaving only the baseline to be
subtracted from the raw spectrum. The basis for this method is a
modified least-squares-based polynomial curve-fitting function, such
that all data points in the generated curve that have an intensity value
higher than their respective pixel value in the input spectrum are
automatically reassigned to the original intensity.

## References

- Lieber, C.A., Mahadevan-Jansen, A., (2003). Automated method for
  subtraction of fluorescence from biological Raman spectra. Applied
  Spectroscopy, 57(11):1363-1367

## Author

Christian L. Goueguel

## Examples

``` r
wl <- seq(200, 400, length.out = 500)
spec <- 0.002 * (wl - 200)^2 + 50 * exp(-(wl - 300)^2 / 2) + rnorm(500, sd = 0.5)
res <- baseline_lsp(matrix(spec, nrow = 1), degree = 3, max.iter = 100)
plot(wl, spec, type = "l")
lines(wl, unlist(res$background), col = "red")

```
