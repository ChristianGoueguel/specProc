# Slope and Bias Correction of Predictions

Corrects the predictions of a model applied to spectra of another
instrument (or measured under other conditions) by a straight line,
estimated on transfer samples whose reference values are known.

## Usage

``` r
slope_bias(predicted, reference, slope = TRUE)
```

## Arguments

- predicted:

  The predictions of the model for the slave spectra of the transfer
  samples.

- reference:

  Their reference values (or the predictions for their master spectra).

- slope:

  A logical: correct the slope and the bias (`TRUE`, default), or the
  bias only.

## Value

An object of class `specproc_slope_bias`, a list with the `coefficients`
(a tibble with the `bias` and `slope`, their standard errors, the values
they are tested against and the p-values), the number of transfer
samples `n`, and the root mean square errors before and after correction
(`rmse`). Use
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_slope_bias.md)
to correct new predictions.

## Details

The model of the master instrument, applied to the slave spectra of the
transfer samples, gives predictions \\\hat{y}\\; the reference values
\\y\\ are regressed on them, \\y = a + b\\\hat{y}\\, and the predictions
of new slave spectra are corrected likewise. With `slope = FALSE`, only
the bias is corrected (\\b = 1\\). The correction leaves the spectra and
the model unchanged, so it suits differences that affect all samples
alike; it cannot correct differences of spectral shape that change the
ranking of the samples (see
[`ds()`](https://christiangoueguel.com/specProc/reference/ds.md),
[`sst()`](https://christiangoueguel.com/specProc/reference/sst.md) or
[`pds()`](https://christiangoueguel.com/specProc/reference/pds.md)). The
tests of the bias against 0 and of the slope against 1 tell whether the
correction is needed.

## References

- Feudale, R.N., Woody, N.A., Tan, H., Myles, A.J., Brown, S.D.,
  Ferré, J. (2002). Transfer of multivariate calibration models: a
  review. Chemometrics and Intelligent Laboratory Systems,
  64(2):181-192.

## See also

[`predict.specproc_slope_bias()`](https://christiangoueguel.com/specProc/reference/predict.specproc_slope_bias.md),
[`ds()`](https://christiangoueguel.com/specProc/reference/ds.md),
[`sst()`](https://christiangoueguel.com/specProc/reference/sst.md),
[`pds()`](https://christiangoueguel.com/specProc/reference/pds.md)

## Author

Christian L. Goueguel

## Examples

``` r
# predictions of the potassium content of transfer samples on a second
# instrument, 10% too high and offset by 0.2%
set.seed(1)
reference <- runif(20, 0.5, 4)
predicted <- 0.2 + 1.1 * reference + rnorm(20, sd = 0.05)
correction <- slope_bias(predicted, reference)
correction
#> Slope and bias correction (20 transfer samples)
#> 
#>   term estimate std_error null   p_value
#>   bias  -0.2353  0.027947    0 1.177e-07
#>  slope   0.9267  0.009086    1 2.163e-07
#> 
#> RMSE:  0.456 before, 0.0406 after
predict(correction, c(1.3, 2.4, 3.5))
#> [1] 0.9694174 1.9887610 3.0081046
```
