# Relative GLSW Weighting Parameter

A [dials](https://dials.tidymodels.org/reference/dials-package.html)
parameter for the `alpha` argument of
[`step_glsw()`](https://christiangoueguel.com/specProc/reference/step_glsw.md)
and
[`step_y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/step_y_gradient_glsw.md),
on a log10 scale.

## Usage

``` r
glsw_alpha(range = c(-4, 0), trans = scales::transform_log10())
```

## Arguments

- range:

  A two-element vector with the range of `alpha`, in log10 units.
  Default is `c(-4, 0)`, i.e. 0.0001 to 1.

- trans:

  A transformation object from the scales package. Default is log10.

## Value

A `quant_param` object.

## Examples

``` r
glsw_alpha()
#> GLSW weighting (relative) (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [-4, 0]
dials::value_seq(glsw_alpha(), 5)
#> [1] 1e-04 1e-03 1e-02 1e-01 1e+00
```
