# Spectra Normalization

This function implements normalization methods based on background,
total area, and internal standard.

## Usage

``` r
normalize(x, method = "area", bkg = NULL, wlength = NULL, drop.na = TRUE)
```

## Arguments

- x:

  A numeric matrix or data frame containing the spectra.

- method:

  A character vector specifying the normalization method to apply.
  Available methods are: "area", "background", and "internal".

- bkg:

  A numeric matrix or data frame of the same dimension as `x`,
  specifying the intensity of the continuum radiation (background
  emission) used for normalizing `x`. Required for "background" method.

- wlength:

  A character vector of the selected wavelength(s) (column names).
  Required for the "internal" method, where the intensities at these
  wavelengths are summed to give the internal standard. Optional for the
  "background" method, where only these wavelengths are normalized and
  returned.

- drop.na:

  A logical value indicating whether to remove spectra (rows) containing
  missing values before normalizing. Default is `TRUE`.

## Value

A tibble of normalized spectra.

## Details

The three normalization methods:

- **Normalization to the background:** Spectra are divided by the
  intensity of the background emission. Note that it is recommended that
  the detector dark current be subtracted prior to the normalization.

- **Normalization to the total area:** Each spectrum is divided by the
  total area of the spectrum over the whole spectral range. The detector
  dark current must be subtracted prior to this normalization. The total
  area is calculated as the sum of all intensity levels.

- **Normalization to an internal standard:** The peak intensity (or
  area) of the emission line related to the analyte is divided by the
  peak intensity (or area) of a selected emission line related to the
  internal standard. The internal standard concentration is assumed
  constant or known.

## References

- De Giacomo, A., Dell’Aglio, M., De Pascale, O., Gaudiuso, R.,
  Santagata, A., Teghil, R., (2008). Laser-induced breakdown
  spectroscopy methodology for the analysis of copper based alloys used
  in ancient artworks. Spectrochimica Acta Part B, 63(5):585-590

- Body, D., Chadwick, B.L., (2001). Optimization of the spectral data
  processing in a LIBS simultaneous elemental analysis system.
  Spectrochimica Acta Part B, 56(6):725-736.

- Rinnan, A., Van den Berg, F., Balling Engelsen, S., (2009). Review of
  the most common preprocessing techniques for near-infrared spectra,
  Trends in Analytical Chemistry, 28(10):1201-1222.

## Author

Christian L. Goueguel

## Examples

``` r
x <- data.frame(`400` = c(1, 2), `401` = c(3, 6), `402` = c(1, 2), check.names = FALSE)
normalize(x, method = "area")
#> # A tibble: 2 × 3
#>   `400` `401` `402`
#>   <dbl> <dbl> <dbl>
#> 1   0.2   0.6   0.2
#> 2   0.2   0.6   0.2
normalize(x, method = "internal", wlength = "401")
#> # A tibble: 2 × 3
#>   `400` `401` `402`
#>   <dbl> <dbl> <dbl>
#> 1 0.333     1 0.333
#> 2 0.333     1 0.333
```
