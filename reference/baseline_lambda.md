# Baseline Smoothness Parameter

A [dials](https://dials.tidymodels.org/reference/dials-package.html)
parameter for the `lambda` argument of
[`step_baseline()`](https://christiangoueguel.com/specProc/reference/step_baseline.md),
on a log10 scale.

## Usage

``` r
baseline_lambda(range = c(2, 8), trans = scales::transform_log10())
```

## Arguments

- range:

  A two-element vector with the range of `lambda`, in log10 units.
  Default is `c(2, 8)`, i.e. 100 to 10^8.

- trans:

  A transformation object from the scales package. Default is log10.

## Value

A `quant_param` object.

## Examples

``` r
baseline_lambda()
#> Baseline smoothness (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [2, 8]
```
