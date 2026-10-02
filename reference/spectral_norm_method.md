# Norm of a Spectral Normalization

A dials parameter for the `method` of
[`step_spectral_norm()`](https://christiangoueguel.com/specProc/reference/step_spectral_norm.md).

## Usage

``` r
spectral_norm_method(values = c("l1", "area", "l2", "max"))
```

## Arguments

- values:

  The norms to try. Default is all of `"l1"`, `"area"`, `"l2"` and
  `"max"`.

## Value

A dials `qual_param` object.

## See also

[`step_spectral_norm()`](https://christiangoueguel.com/specProc/reference/step_spectral_norm.md)

## Examples

``` r
spectral_norm_method()
#> Spectral norm (qualitative)
#> 4 possible values include:
#> 'l1', 'area', 'l2', and 'max'
spectral_norm_method(c("l1", "max"))
#> Spectral norm (qualitative)
#> 2 possible values include:
#> 'l1' and 'max'
```
