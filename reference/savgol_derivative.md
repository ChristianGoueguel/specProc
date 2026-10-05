# Derivative Order of a Savitzky-Golay Filter

A dials parameter for the `derivative` of
[`step_savgol()`](https://christiangoueguel.com/specProc/reference/step_savgol.md):
0 (smoothing), 1 or 2.
[`step_gap_derivative()`](https://christiangoueguel.com/specProc/reference/step_gap_derivative.md)
uses it with the range 1 to 2.

## Usage

``` r
savgol_derivative(range = c(0L, 2L), trans = NULL)
```

## Arguments

- range:

  The range of derivative orders. Default is 0 to 2.

- trans:

  Not used.

## Value

A dials `quant_param` object.

## See also

[`step_savgol()`](https://christiangoueguel.com/specProc/reference/step_savgol.md),
[`step_gap_derivative()`](https://christiangoueguel.com/specProc/reference/step_gap_derivative.md)

## Examples

``` r
savgol_derivative()
#> Derivative order (quantitative)
#> Range: [0, 2]
dials::value_seq(savgol_derivative(), 3)
#> [1] 0 1 2
```
