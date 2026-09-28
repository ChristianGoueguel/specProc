# Intensities of Emission Lines

Measures the intensity of emission lines in spectra: for each line, the
peak is searched near its tabulated wavelength, and its area or height
is measured. The result feeds
[`boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/boltzmann_plot.md),
[`saha_boltzmann_plot()`](https://christiangoueguel.com/specProc/reference/saha_boltzmann_plot.md),
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md)
and
[`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md).

## Usage

``` r
line_intensities(
  spectra,
  lines,
  half_width = 0.15,
  search = 0.2,
  method = "area",
  baseline = FALSE,
  fit_width = 3 * half_width,
  limit = NULL,
  min_snr = 3
)
```

## Arguments

- spectra:

  The spectra: a numeric vector named by wavelength (one spectrum), or a
  matrix or data frame with one spectrum per row and the wavelengths as
  column names. Other columns of a data frame (such as sample names) are
  kept in the result.

- lines:

  The lines: a numeric vector of wavelengths (nm), whose names are used
  as labels, or a data frame with a `wavelength` column, such as
  returned by
  [`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md),
  whose columns are kept in the result.

- half_width:

  The half-width of the integration window around the peak, in nm.
  Default is 0.15.

- search:

  The half-width of the window in which the peak is searched around the
  tabulated wavelength, in nm. Default is 0.2.

- method:

  `"area"` (default), `"height"` or `"voigt"`.

- baseline:

  A logical: subtract a linear baseline under each line (`FALSE`,
  default).

- fit_width:

  The half-width of the window of the Voigt fit, in nm. Default is 3
  times `half_width`.

- limit:

  The saturation limit of the detector, in counts, or `NULL` (default)
  not to check saturation.

- min_snr:

  The signal-to-noise ratio above which a line is detected. Default is
  3.

## Value

A tibble with one row per spectrum and line: the other columns of
`spectra` (or `spectrum`, the row number), the columns of `lines` (or
`line` and `wavelength`), and `peak_wavelength`, `shift` (nm), `height`,
`intensity`, `snr`, `detected` and `saturated` (which replace the
columns of `lines` of the same name, such as the relative intensity
listed by
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)).
For a single spectrum, it has one row per line, like `lines`, with the
measured `intensity`.

## Details

For each spectrum and line, the peak is the channel of highest intensity
within `search` nm of the tabulated wavelength, which absorbs small
errors of the wavelength calibration. The intensity is then measured on
the channels within `half_width` nm of the peak:

- `method = "area"` (default): the integrated intensity (trapezoidal
  rule), in intensity units times nm;

- `method = "height"`: the peak intensity;

- `method = "voigt"`: the area of a Voigt profile fitted with
  [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)
  to the channels within `fit_width` nm of the peak. It separates the
  line from a flat background and is less sensitive to the window, but
  it is slower and needs a line well sampled by the detector.

With `baseline = TRUE`, a straight line through the intensities at both
ends of the window is subtracted before the area or height is measured
(the Voigt fit always includes a constant background).

Each line is also checked:

- `snr`: the height above the baseline divided by the noise of the
  spectrum, estimated robustly from its channel-to-channel differences;
  `detected` is `snr >= min_snr`.

- `saturated`: with `limit`, whether a channel of the window reaches the
  saturation limit of the detector (see
  [`saturation_summary()`](https://christiangoueguel.com/specProc/reference/saturation_summary.md)).

## See also

[`step_line_intensities()`](https://christiangoueguel.com/specProc/reference/step_line_intensities.md),
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md),
[`cf_libs()`](https://christiangoueguel.com/specProc/reference/cf_libs.md),
[`calibration_curve()`](https://christiangoueguel.com/specProc/reference/calibration_curve.md),
[`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(soilLIBS)
spectra <- soilLIBS[1:8, -(2:8)]
ca <- c(`Ca II 393.37` = 393.37, `Ca II 396.85` = 396.85, `Ca I 422.67` = 422.67)
line_intensities(spectra, ca, baseline = TRUE)
#> # A tibble: 24 × 10
#>    Sample       line   wavelength peak_wavelength   shift height intensity   snr
#>    <chr>        <chr>       <dbl>           <dbl>   <dbl>  <dbl>     <dbl> <dbl>
#>  1 LSG-S18-0001 Ca II…       393.            393. -0.0934  6699.      558. 128. 
#>  2 LSG-S18-0001 Ca II…       397.            397. -0.0731  6595.      550. 126. 
#>  3 LSG-S18-0001 Ca I …       423.            423.  0.0294  2781.      272.  53.1
#>  4 LSG-S18-0001 Ca II…       393.            393. -0.0934  5877.      490. 110. 
#>  5 LSG-S18-0001 Ca II…       397.            397. -0.0731  5759.      480. 108. 
#>  6 LSG-S18-0001 Ca I …       423.            423.  0.0294  2275.      223.  42.6
#>  7 LSG-S18-0001 Ca II…       393.            393. -0.0934  5463.      455. 104. 
#>  8 LSG-S18-0001 Ca II…       397.            397. -0.0731  5824.      485. 111. 
#>  9 LSG-S18-0001 Ca I …       423.            423. -0.0823  1733.      169.  33.1
#> 10 LSG-S18-0001 Ca II…       393.            393. -0.0934  5989.      499. 110. 
#> # ℹ 14 more rows
#> # ℹ 2 more variables: detected <lgl>, saturated <lgl>

# one spectrum and a table of lines, ready for a Boltzmann plot
lines <- data.frame(wavelength = c(428.30, 430.25, 443.50, 445.48),
                    Aki = c(4.34e7, 1.36e8, 6.70e7, 8.70e7), gk = c(5, 5, 5, 7),
                    Ek = c(4.78, 4.78, 4.68, 4.68))
mean_spectrum <- colMeans(soilLIBS[-(1:8)])
line_intensities(mean_spectrum, lines, baseline = TRUE)
#> # A tibble: 4 × 11
#>   wavelength      Aki    gk    Ek peak_wavelength   shift height intensity   snr
#>        <dbl>    <dbl> <dbl> <dbl>           <dbl>   <dbl>  <dbl>     <dbl> <dbl>
#> 1       428.   4.34e7     5  4.78            428. -0.0487   404.      33.9  37.2
#> 2       430.   1.36e8     5  4.78            430. -0.0429   732.      62.2  67.4
#> 3       444.   6.70e7     5  4.68            443. -0.0721   299.      25.1  27.6
#> 4       445.   8.70e7     7  4.68            445. -0.0898   477.      40.3  44.0
#> # ℹ 2 more variables: detected <lgl>, saturated <lgl>
```
