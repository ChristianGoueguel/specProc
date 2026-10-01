# Spectra Normalization

This function implements normalization methods based on background,
total area, internal standard, and the L1, L2 and maximum norms of each
spectrum.

## Usage

``` r
normalize(x, method = "area", bkg = NULL, wlength = NULL, drop.na = TRUE)
```

## Arguments

- x:

  A numeric matrix or data frame containing the spectra.

- method:

  A character vector specifying the normalization method to apply.
  Available methods are: "area", "l1", "l2", "max", "background", and
  "internal". Spectra whose L1, L2 or maximum norm is zero are set to
  `NA`, with a warning.

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

The normalization methods:

- **Normalization to the background:** Spectra are divided by the
  intensity of the background emission. Note that it is recommended that
  the detector dark current be subtracted prior to the normalization.

- **Normalization to the total area:** Each spectrum is divided by the
  total area of the spectrum over the whole spectral range. The detector
  dark current must be subtracted prior to this normalization. The total
  area is calculated as the sum of all intensity levels.

- **L1 normalization** (`"l1"`): each spectrum is divided by the sum of
  the absolute values of its intensities. It equals the total area for
  non-negative spectra, and also suits derivative spectra, whose values
  change sign.

- **L2 (vector) normalization** (`"l2"`): each spectrum is divided by
  its Euclidean norm, so that its sum of squares is 1. Unlike SNV, the
  spectrum is not centered.

- **Maximum normalization** (`"max"`): each spectrum is divided by its
  largest absolute intensity, so that its strongest feature is 1 (or
  -1).

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
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]  # the spectral channels
normalize(spectra[1:3, ], method = "area")[, 1:4]
#> # A tibble: 3 × 4
#>   `199.3771616` `199.4644141` `199.5516666` `199.6389192`
#>           <dbl>         <dbl>         <dbl>         <dbl>
#> 1     0.0000444     0.0000452     0.0000484     0.0000451
#> 2     0.0000449     0.0000464     0.0000484     0.0000445
#> 3     0.0000430     0.0000442     0.0000473     0.0000442
normalize(spectra[1:3, ], method = "l2")[, 1:4]
#> # A tibble: 3 × 4
#>   `199.3771616` `199.4644141` `199.5516666` `199.6389192`
#>           <dbl>         <dbl>         <dbl>         <dbl>
#> 1       0.00185       0.00188       0.00201       0.00188
#> 2       0.00191       0.00197       0.00206       0.00189
#> 3       0.00183       0.00188       0.00201       0.00188
# internal standard: the C I 247.86 nm line of the organic matrix
carbon <- names(spectra)[which.min(abs(as.numeric(names(spectra)) - 247.856))]
normalize(spectra[1:3, ], method = "internal", wlength = carbon)[, 1:4]
#> # A tibble: 3 × 4
#>   `199.3771616` `199.4644141` `199.5516666` `199.6389192`
#>           <dbl>         <dbl>         <dbl>         <dbl>
#> 1        0.0207        0.0211        0.0226        0.0210
#> 2        0.0225        0.0232        0.0242        0.0223
#> 3        0.0196        0.0202        0.0216        0.0202
```
