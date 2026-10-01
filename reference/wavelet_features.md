# Wavelet Coefficients of Spectra

Computes the discrete wavelet transform (DWT) of spectra and returns its
coefficients as features: a compressed representation of the spectra for
modeling, with far fewer variables than channels.

## Usage

``` r
wavelet_features(x, wavelet = "d4", level = 3, coefficients = "approximation")
```

## Arguments

- x:

  A numeric matrix or data frame of spectra, one per row, or a numeric
  vector (one spectrum).

- wavelet:

  The wavelet: `"haar"`, `"d4"` (default), `"d6"`, `"d8"` or `"la8"`.

- level:

  The number of levels of the decomposition. Default is 3.

- coefficients:

  The coefficients returned: `"approximation"` (default) or `"all"`.

## Value

A matrix with one row per spectrum and one column per coefficient, named
`A<level>_<i>` for the approximation and `D<j>_<i>` for the details of
level `j`.

## Details

The DWT decomposes a spectrum, level by level, into approximation
coefficients (the smooth part, at half the resolution of the previous
level) and detail coefficients (the fine structure removed at that
level), with the pyramid algorithm of Mallat (1989) and an orthonormal
wavelet:

- `"haar"`: the Haar wavelet (2 coefficients);

- `"d4"`, `"d6"`, `"d8"`: Daubechies' extremal phase wavelets with 4, 6
  and 8 coefficients;

- `"la8"`: Daubechies' least asymmetric wavelet with 8 coefficients
  (symlet 4), whose nearly symmetric shape suits spectral lines.

The coefficients kept are

- `"approximation"` (default): the approximation at `level`, a smoothed
  version of the spectrum with about \\2^{-level}\\ times as many
  values;

- `"all"`: the approximation at `level` and the details of every level,
  the complete transform (as many values as channels, after padding).

The spectrum is extended by reflection at its end to a length divisible
by \\2^{level}\\, and the transform is periodic. The transform is
orthonormal, so the sum of squares of all the coefficients equals that
of the (padded) spectrum. As features for a model, the approximation
keeps the broad shape of the spectrum and drops the noise; to keep only
the most informative coefficients,
[`step_wavelet()`](https://christiangoueguel.com/specProc/reference/step_wavelet.md)
can select those with the largest variance in the training data (Trygg
and Wold, 1998).

## References

- Mallat, S.G. (1989). A theory for multiresolution signal
  decomposition: the wavelet representation. IEEE Transactions on
  Pattern Analysis and Machine Intelligence, 11(7):674-693.

- Daubechies, I. (1992). Ten Lectures on Wavelets. SIAM, Philadelphia.

- Trygg, J., Wold, S. (1998). PLS regression on wavelet compressed NIR
  spectra. Chemometrics and Intelligent Laboratory Systems,
  42(1-2):209-220.

## See also

[`step_wavelet()`](https://christiangoueguel.com/specProc/reference/step_wavelet.md),
[`savitzky_golay()`](https://christiangoueguel.com/specProc/reference/savitzky_golay.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
spectra <- forageLIBS[1:4, -(1:14)]
approx <- wavelet_features(spectra, wavelet = "la8", level = 4)
dim(approx)   # 7152 channels -> 447 coefficients
#> [1]   4 447
```
