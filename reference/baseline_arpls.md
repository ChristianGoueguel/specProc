# Asymmetrically Reweighted Penalized Least Squares

Baseline correction based on asymmetrically reweighted penalized least
squares smoothing algorithm as proposed by Baek *et al.* (2015).

## Usage

``` r
baseline_arpls(x, lambda = 1000, ratio = 0.05, max.iter = 10)
```

## Arguments

- x:

  A numeric matrix or data frame, with one spectrum per row.

- lambda:

  A numeric value specifying the smoothing parameter, which controls the
  amount of curvature allowed for the baseline. The smaller the lambda,
  the more curvature in the baseline fitting. Default is 1000.

- ratio:

  A numeric value specifying the convergence ratio for the iterative
  algorithm. The algorithm stops when the relative change in the weights
  is less than this ratio. Typical values are between 0.01 and 0.1.
  Default is 0.05.

- max.iter:

  Maximum number of iterations for the algorithm. Default is 10.

## Value

A list containing two tibbles:

- `correction`: The baseline-corrected spectral matrix.

- `background`: The fitted background emission.

## Details

The algorithm estimates a baseline curve by iteratively updating weights
based on the residuals and minimizing a penalized least squares
criterion. The resulting baseline curve is subtracted from the input
data, providing a baseline-corrected version.

The penalized system is pentadiagonal and is solved in C++ with a banded
Cholesky decomposition, so the cost grows linearly with the number of
spectral channels.

## References

- Baek, S.-J., Park, A., Ahn, Y.-J., Choo, J., (2015). Baseline
  correction using asymmetrically reweighted penalized least squares
  smoothing. Analyst, 140(1):250–257.

## Author

Christian L. Goueguel

## Examples

``` r
wl <- seq(200, 400, length.out = 500)
spec <- 0.002 * (wl - 200)^2 + 50 * exp(-(wl - 300)^2 / 2) + rnorm(500, sd = 0.5)
res <- baseline_arpls(matrix(spec, nrow = 1), lambda = 1e5)
plot(wl, spec, type = "l")
lines(wl, unlist(res$background), col = "red")

```
