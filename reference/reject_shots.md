# Rejection of Outlying Laser Shots

Flags the outlying shots (spectra) of each sample of a LIBS dataset,
such as shots with a weak or missed plasma, or shots on an inclusion or
a contaminated spot, before the shots are averaged.

## Usage

``` r
reject_shots(
  data,
  sample,
  method = c("intensity", "correlation"),
  cutoff = 3.5,
  wavelength = NULL,
  drop = FALSE,
  shot = NULL,
  scale = c("floor", "sample", "pooled")
)
```

## Arguments

- data:

  A data frame with one shot per row: a sample column and spectral
  columns named by their wavelengths (other columns are kept but not
  used).

- sample:

  The column identifying the sample of each shot, unquoted or as a
  string.

- method:

  The criteria: one or more of `"intensity"`, `"correlation"` (both by
  default) and `"distance"`.

- cutoff:

  The robust z-score above which a shot is rejected. Default is 3.5.

- wavelength:

  An optional wavelength range (nm) in which the criteria are computed,
  for example to avoid saturated lines.

- drop:

  A logical: return only the kept shots, without the added columns
  (`FALSE`, default).

- shot:

  An optional column (unquoted or as a string) giving the order of the
  shots in each sample, such as the shot number. Default is the order of
  the rows.

- scale:

  The robust scale of the z-scores: `"floor"` (default), `"sample"` or
  `"pooled"` (see Details).

## Value

A tibble: `data` with the shot number in its sample `.shot`, a logical
column `.rejected`, the criteria that rejected each shot in `.reason`
(`NA` for kept shots), the raw criteria `.intensity`, `.correlation` and
`.distance`, and the robust z-score of each criterion used
(`.intensity_z`, `.correlation_z`, `.distance_z`). Its attribute
`"reject_shots"` holds the settings. With `drop = TRUE`, the kept rows
of `data`.

## Details

Each shot is compared with the other shots of its sample, by robust
z-scores of one or more criteria:

- `"intensity"`: the total intensity of the spectrum, on a log scale
  (its deviation is relative to the median shot of the sample). Both
  weak and unusually strong shots are flagged.

- `"correlation"`: the Pearson correlation \\r\\ of the spectrum with
  the median spectrum of the sample, which detects changes of the shape
  of the spectrum (other lines, other line ratios) whatever its
  intensity. The z-scores are those of Fisher's \\z = \tanh^{-1}(r)\\:
  correlations near 1 are bounded and skewed, so that small differences
  such as 0.998 and 0.9995 would otherwise look extreme. Only shots with
  a low correlation are flagged.

- `"distance"`: the Euclidean distance of the spectrum to the median
  spectrum of the sample, relative to the norm of the median spectrum,
  on a log scale, which combines both. Only far shots are flagged.

**Scale.** A z-score is the deviation of a shot from the median of its
sample divided by a robust scale (the MAD). With the few shots of a
sample (often 5 to 10), the MAD of the sample is unstable: when its
shots happen to be very similar, a shot that differs by little gets a
large z-score. With `scale = "floor"` (default), the scale of a sample
is its own MAD, but not less than the pooled MAD of the deviations of
all the samples (the typical shot-to-shot variability of the data set);
`"sample"` uses the MAD of each sample alone, and `"pooled"` the pooled
MAD for all samples, which suits samples of similar variability. In
simulations of 8 shots per sample with noise varying between samples,
`"floor"` with Fisher's \\z\\ rejected 0.1% to 0.5% of the regular shots
(against 2.5% to 5% with the MAD of each sample and the raw
correlations), and still detected the shots of a plasma weakened by a
quarter or more, or with an extra line of 10% of the strongest line.
Lower `cutoff` to detect milder deviations.

A shot is rejected when any criterion exceeds `cutoff` (3.5 by default,
following Iglewicz and Hoaglin, 1993). Samples with fewer than 3 shots
are not checked. When a MAD is zero, the mean absolute deviation is
used.

The result keeps the raw criteria (`.intensity`, the total intensity
relative to the median shot of the sample; `.correlation`; `.distance`),
easier to read than the z-scores, and the settings, which
[`plot_shots()`](https://christiangoueguel.com/specProc/reference/plot_shots.md)
uses to show the shots and the rejections. To average the kept shots per
sample, use
[`average()`](https://christiangoueguel.com/specProc/reference/average.md)
on the result with `drop = TRUE`. In a recipe, use
[`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md).

## References

- Iglewicz, B., Hoaglin, D.C. (1993). How to Detect and Handle Outliers.
  ASQC Quality Press, Milwaukee.

- Fisher, R.A. (1915). Frequency distribution of the values of the
  correlation coefficient in samples from an indefinitely large
  population. Biometrika, 10(4):507-521.

## See also

[`plot_shots()`](https://christiangoueguel.com/specProc/reference/plot_shots.md),
[`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md),
[`average()`](https://christiangoueguel.com/specProc/reference/average.md),
[`saturation_summary()`](https://christiangoueguel.com/specProc/reference/saturation_summary.md),
[forageShots](https://christiangoueguel.com/specProc/reference/forageShots.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageShots)
# 8 shots of each of 20 forage samples
res <- reject_shots(forageShots, Measurement, shot = shot)
res[res$.rejected, c("Measurement", ".shot", ".reason", ".intensity", ".correlation")]
#> # A tibble: 8 × 5
#>   Measurement .shot .reason                .intensity .correlation
#>         <int> <int> <chr>                       <dbl>        <dbl>
#> 1      121138     4 intensity, correlation      1.28         0.960
#> 2      121144     7 intensity, correlation      0.729        0.961
#> 3      121238     5 intensity                   0.704        0.956
#> 4      121238     6 intensity                   0.747        0.969
#> 5      121306     6 intensity                   0.697        0.936
#> 6      121382     5 intensity                   1.34         0.958
#> 7      121618     7 correlation                 0.793        0.970
#> 8      121645     4 intensity                   0.734        0.956
# the kept shots only
nrow(reject_shots(forageShots, Measurement, drop = TRUE))
#> [1] 152
# the former behavior, with the MAD of each sample alone
sum(reject_shots(forageShots, Measurement, scale = "sample")$.rejected)
#> [1] 8
```
