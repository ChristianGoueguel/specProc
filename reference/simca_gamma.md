# Tuning Parameters of the SIMCA Model

[dials](https://dials.tidymodels.org/reference/dials-package.html)
parameters for the classification rule of the
[`simca()`](https://christiangoueguel.com/specProc/reference/simca.md)
parsnip model (and of
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)):

- `simca_gamma()`, for `gamma`, the weight of the orthogonal distances
  against the score distances;

- `simca_squared()`, for `squared`, the combination of the squared
  scaled distances (rule R2) or of the scaled distances (rule R1).

## Usage

``` r
simca_gamma(range = c(0, 1), trans = NULL)

simca_squared(values = c(TRUE, FALSE))
```

## Arguments

- range:

  A two-element vector with the range of `gamma`. Default is `c(0, 1)`.

- trans:

  A transformation object from the scales package, or `NULL` (default)
  for none.

- values:

  The values of `squared`. Default is `c(TRUE, FALSE)`.

## Value

A `quant_param` object (`simca_gamma()`) or a `qual_param` object
(`simca_squared()`).

## See also

[`simca()`](https://christiangoueguel.com/specProc/reference/simca.md),
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)

## Examples

``` r
simca_gamma()
#> Weight of the orthogonal distances (quantitative)
#> Range: [0, 1]
dials::value_seq(simca_gamma(), 5)
#> [1] 0.00 0.25 0.50 0.75 1.00
simca_squared()
#> Squared scaled distances (qualitative)
#> 2 possible values include:
#> TRUE and FALSE
```
