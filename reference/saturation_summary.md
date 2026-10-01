# Detect Saturated Channels in Spectra

Finds the channels at the saturation limit of the detector, for each
spectrum and for each wavelength. Saturated lines are clipped, so their
intensity and width are wrong and they respond nonlinearly to
concentration.

## Usage

``` r
saturation_summary(x, limit = 65535, tolerance = 0)
```

## Arguments

- x:

  A numeric matrix or data frame of spectra, one per row, with the
  wavelengths as column names.

- limit:

  The saturation limit of the detector, in counts. Default is 65535
  (16-bit detectors).

- tolerance:

  Channels within `tolerance` counts of `limit` are counted as
  saturated. Default is 0.

## Value

A list with two tibbles:

- `spectra`: for each spectrum (row), the number `n_saturated` and
  fraction of saturated channels;

- `channels`: for each channel saturated in at least one spectrum, its
  `wavelength` (from the column names; `NA` if they are not numeric),
  column `index`, and the number and fraction of saturated spectra.

## Examples

``` r
data(forageLIBS)
sat <- saturation_summary(forageLIBS[-(1:14)], limit = 65535)
sat$channels
#> # A tibble: 6 × 4
#>   wavelength index n_spectra fraction
#>        <dbl> <int>     <dbl>    <dbl>
#> 1       388.  2229        30   0.0815
#> 2       393.  2297        48   0.130 
#> 3       393.  2298       133   0.361 
#> 4       397.  2339        82   0.223 
#> 5       399.  2368       124   0.337 
#> 6       399.  2369        46   0.125 
```
