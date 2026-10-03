# Number of Intervals

The number of spectral intervals selected by interval PLS, a tuning
parameter of
[`step_select_wavelengths()`](https://christiangoueguel.com/specProc/reference/step_select_wavelengths.md)
with `method = "ipls"`.

## Usage

``` r
num_intervals(range = c(1L, 10L), trans = NULL)
```

## Arguments

- range:

  A two-element vector with the smallest and largest numbers of
  intervals. Default is 1 to 10.

- trans:

  A transformation object from the scales package, or `NULL`.

## Value

A `dials` quantitative parameter.

## Examples

``` r
num_intervals()
#> # Intervals (quantitative)
#> Range: [1, 10]
```
