# Asymmetric Least Squares

Baseline correction based on asymmetric least squares (ALS) algorithm as
proposed by Eilers *et al.* (2005).

## Usage

``` r
whittaker(x, lambda = 1000, p = 0.001, max.iter = 10)
```

## Arguments

- x:

  A numeric matrix or data frame, with one spectrum per row.

- lambda:

  A numeric value specifying the smoothing parameter, which controls the
  amount of curvature allowed for the baseline. The smaller the lambda,
  the more curvature in the baseline fitting. Default is 1000.

- p:

  A numeric value specifying the extent of asymmetry required of the
  fit. Larger values allow more negative-going regions. Smaller values
  disallow negative-going regions. `p` must be between 0 and 1. Default
  is 0.001.

- max.iter:

  Maximum number of iterations for the algorithm. Default is 10.

## Value

A list containing two tibbles:

- `correction`: The baseline-corrected spectral matrix.

- `background`: The fitted background emission.

## Details

The function applies Eilers' method based on a Whittaker filter. The
algorithm estimates a baseline curve by minimizing the asymmetric least
squares criterion, which allows for different weights for positive and
negative residuals. The resulting baseline curve is subtracted from the
input data, providing a baseline-corrected version.

The penalized system is pentadiagonal and is solved in C++ with a banded
Cholesky decomposition, so the cost grows linearly with the number of
spectral channels. Negative values in the corrected spectra are kept as
they are (they typically reflect noise around the baseline).

## References

- Eilers, P.H.C., Boelens, H.F.M., (2005). Baseline correction with
  asymmetric least squares smoothing. Leiden University Medical Centre
  report.

## Author

Christian L. Goueguel

## Examples

``` r
wl <- seq(200, 400, length.out = 500)
spec <- 0.002 * (wl - 200)^2 + 50 * exp(-(wl - 300)^2 / 2) + rnorm(500, sd = 0.5)
res <- whittaker(matrix(spec, nrow = 1), lambda = 1e5, p = 0.01)
plot(wl, spec, type = "l")
lines(wl, unlist(res$background), col = "red")

```
