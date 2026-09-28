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
  drop = FALSE
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

## Value

A tibble: `data` with a logical column `.rejected`, the criteria that
rejected each shot in `.reason` (`NA` for kept shots) and the robust
z-score of each criterion (`.intensity_z`, `.correlation_z`,
`.distance_z`). With `drop = TRUE`, the kept rows of `data`.

## Details

Each shot is compared with the other shots of its sample, by robust
z-scores (deviation from the median of the sample divided by the MAD) of
one or more criteria:

- `"intensity"`: the total intensity of the spectrum. Both weak and
  unusually strong shots are flagged.

- `"correlation"`: the Pearson correlation of the spectrum with the
  median spectrum of the sample, which detects changes of the shape of
  the spectrum (other lines, other line ratios) whatever its intensity.
  Only shots with a low correlation are flagged.

- `"distance"`: the Euclidean distance of the spectrum to the median
  spectrum of the sample, which combines both. Only far shots are
  flagged.

A shot is rejected when any criterion exceeds `cutoff` (3.5 by default,
following Iglewicz and Hoaglin, 1993). Samples with fewer than 3 shots
are not checked. When all shots of a sample vary by the same amount, the
MAD can be zero; the mean absolute deviation is then used.

To average the kept shots per sample, use
[`average()`](https://christiangoueguel.com/specProc/reference/average.md)
on the result with `drop = TRUE`. In a recipe, use
[`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md).

## References

- Iglewicz, B., Hoaglin, D.C. (1993). How to Detect and Handle Outliers.
  ASQC Quality Press, Milwaukee.

## See also

[`step_reject_shots()`](https://christiangoueguel.com/specProc/reference/step_reject_shots.md),
[`average()`](https://christiangoueguel.com/specProc/reference/average.md),
[`saturation_summary()`](https://christiangoueguel.com/specProc/reference/saturation_summary.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(specLIBS)
shots <- reject_shots(specLIBS[-(2:8)], Sample)
table(shots$.rejected)
#> 
#> FALSE  TRUE 
#>   363    37 
head(shots[shots$.rejected, c("Sample", ".reason", ".intensity_z", ".correlation_z")])
#> # A tibble: 6 × 4
#>   Sample       .reason                .intensity_z .correlation_z
#>   <chr>        <chr>                         <dbl>          <dbl>
#> 1 LSG-S18-0003 intensity                      5.50          0.414
#> 2 LSG-S18-0007 correlation                   -1.03          6.49 
#> 3 LSG-S18-0007 correlation                    1.01         10.6  
#> 4 LSG-S18-0009 intensity                     -5.98          0.786
#> 5 LSG-S18-0009 intensity                     -6.70          0.708
#> 6 LSG-S18-0010 intensity, correlation         7.29         18.2  

# mean spectrum of each sample, without the rejected shots
means <- average(reject_shots(specLIBS[-(2:8)], Sample, drop = TRUE), Sample)
dim(means)
#> [1]   50 7153
```
