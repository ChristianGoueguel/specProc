# Number of Levels of a Wavelet Decomposition

A dials parameter for the `level` of
[`step_wavelet()`](https://christiangoueguel.com/specProc/reference/step_wavelet.md).

## Usage

``` r
wavelet_level(range = c(1L, 6L), trans = NULL)
```

## Arguments

- range:

  The range of levels. Default is 1 to 6.

- trans:

  Not used.

## Value

A dials `quant_param` object.

## See also

[`step_wavelet()`](https://christiangoueguel.com/specProc/reference/step_wavelet.md)

## Examples

``` r
wavelet_level()
#> Wavelet levels (quantitative)
#> Range: [1, 6]
dials::value_seq(wavelet_level(), 3)
#> [1] 1 3 6
```
