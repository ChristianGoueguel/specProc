# LIBS Laser Shots of Forage Samples

The single laser shots of 20 of the measurements of
[forageLIBS](https://christiangoueguel.com/specProc/reference/forageLIBS.md):
8 shots per measurement, in two wavelength windows, to study the
shot-to-shot variability and the rejection of outlying shots.

## Usage

``` r
forageShots
```

## Format

A tibble with 160 rows (shots) and 844 columns:

- Measurement:

  Measurement identifier, as in
  [forageLIBS](https://christiangoueguel.com/specProc/reference/forageLIBS.md).

- Sample:

  Sample identifier, as in
  [forageLIBS](https://christiangoueguel.com/specProc/reference/forageLIBS.md).

- shot:

  Shot number in the measurement (1 to 8), in firing order.

- Ca, K:

  Calcium and potassium contents, in percent.

- 380.011254, ..., 779.977295:

  Emission intensity (counts) at each wavelength, in nm.

## Source

Laboratory LIBS measurements provided by Christian L. Goueguel. The
script `data-raw/forageShots.R` in the package source builds the data
set from the shot-level export.

## Details

[forageLIBS](https://christiangoueguel.com/specProc/reference/forageLIBS.md)
holds the mean of the 8 shots of each measurement; these are the shots
themselves, raw detector counts, in the order in which they were fired.
To keep the package small, only two windows are kept: 380 to 430 nm (Ca
II 393.37 and 396.85 nm, Ca I 422.67 nm, Al I 394.40 and 396.15 nm, the
CN band at 388 nm) and 760 to 780 nm (K I 766.49 and 769.90 nm, O I 777
nm). The strongest lines reach the saturation of the detector (65535
counts) in most shots (132 of the 160); the `wavelength` argument of
[`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md)
can leave them out.

The 20 measurements are those of the 368 with a shot rejected by
[`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md)
in these windows (7), and 13 others drawn at random: their outlying
shots are weak or strong plasmas (total intensity 0.7 or 1.3 times that
of the other shots) with spectra of a different shape.

## See also

[`reject_shots()`](https://christiangoueguel.com/specProc/reference/reject_shots.md),
[`plot_shots()`](https://christiangoueguel.com/specProc/reference/plot_shots.md),
[forageLIBS](https://christiangoueguel.com/specProc/reference/forageLIBS.md)

## Examples

``` r
data(forageShots)
dim(forageShots)
#> [1] 160 844
# the shot-to-shot variability of the total intensity, per measurement
total <- rowSums(forageShots[-(1:5)])
round(tapply(total, forageShots$Measurement, function(v) 100 * sd(v) / mean(v)), 1)
#> 121022 121041 121080 121089 121117 121138 121140 121144 121163 121238 121306 
#>    4.7    6.1    9.3    7.7    7.2   10.9    5.0   11.3   12.3   15.7   12.7 
#> 121318 121319 121322 121323 121367 121382 121440 121618 121645 
#>    4.8    6.6    5.7    7.7    9.3   11.8    6.6    8.6   12.1 
```
