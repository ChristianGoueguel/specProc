# SIMCA Distance Weighting Parameter

A [dials](https://dials.tidymodels.org/reference/dials-package.html)
parameter for the `gamma` argument of the
[`simca()`](https://christiangoueguel.com/specProc/reference/simca.md)
parsnip model (and of
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)):
the weight of the orthogonal distances, against the score distances, in
the classification rule.

## Usage

``` r
simca_gamma(range = c(0, 1), trans = NULL)
```

## Arguments

- range:

  A two-element vector with the range of `gamma`. Default is `c(0, 1)`.

- trans:

  A transformation object from the scales package, or `NULL` (default)
  for none.

## Value

A `quant_param` object.

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
```
