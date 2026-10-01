
<!-- README.md is generated from README.Rmd. Please edit that file -->

# specProc <img src="man/figures/logo.png" align="right" height="160"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ChristianGoueguel/specProc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ChristianGoueguel/specProc/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ChristianGoueguel/specProc/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ChristianGoueguel/specProc?branch=main)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

**specProc** processes and analyzes emission spectra, from raw spectra
to calibrated concentrations. It was developed for laser-induced
breakdown spectroscopy (LIBS), and works with other techniques such as
Raman, infrared and ICP-OES. Spectra are stored one per row, with the
wavelengths as column names, and the heavy computations run in C++.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("ChristianGoueguel/specProc", build_vignettes = TRUE)
```

A C++ compiler is needed to build the package from source (Rtools on
Windows, Xcode Command Line Tools on macOS).

## A LIBS workflow

The vignettes follow a typical LIBS analysis, on the `forageLIBS`
spectra of forage samples:

1.  **Fitting emission lines** (`vignette("peak-fitting")`): line
    identification, wavelength calibration, line profiles and
    overlapping lines.
2.  **Preprocessing with recipe steps** (`vignette("preprocessing")`):
    baseline correction, normalization and other `step_*()` functions,
    in a tidymodels workflow.
3.  **Calibration curves and figures of merit**
    (`vignette("calibration")`): univariate curves, limits of detection
    and quantification, and the net analyte signal of multivariate
    models.
4.  **Plasma diagnostics** (`vignette("plasma-diagnostics")`): electron
    density, temperature, LTE, self-absorption and calibration-free
    LIBS.

They are also available as [articles on the package
website](https://christiangoueguel.com/specProc/articles/).

## Example

A calibration curve for potassium, from the intensity of the K I 769.90
nm line normalized to the carbon line C I 247.86 nm:

``` r
library(specProc)
library(dplyr)

data("forageLIBS")
spectra_id <- forageLIBS |> select(1:2) |> names()
minerals <- forageLIBS |> select(3:14) |> names()
spectra <- forageLIBS |> select(-all_of(c(spectra_id, minerals)))

corrected <- baseline_arpls(spectra, lambda = 1e5, max.iter = 20)$correction
intensities <- line_intensities(corrected, c(C = 247.856, K = 769.896))

data <- tibble(
  K = forageLIBS$K,
  signal = intensities$intensity[intensities$line == "K"] /
    intensities$intensity[intensities$line == "C"]
)
curve <- calibration_curve(data, signal, K)
curve$figures_of_merit
#> # A tibble: 1 × 7
#>       n sensitivity r_squared sigma sigma_blank   lod   loq
#>   <int>       <dbl>     <dbl> <dbl>       <dbl> <dbl> <dbl>
#> 1   368       0.302     0.575 0.139       0.139  1.52  4.60
```

``` r
plot_calibration(curve)
```

<img src="man/figures/README-example-plot-1.png" alt="" width="80%" />

## Function overview

| Task | Functions |
|----|----|
| Line identification | `libs_lines()`, `plot_lines()`, `line_finder()` (Shiny app), `wavelength_calibration()`, `apply_calibration()` |
| Line fitting | `peak_fit()`, `multipeak_fit()`, `plot_fit()`, `line_intensities()`, `voigt_profile()`, `pseudo_voigt_profile()`, `gaussian_profile()`, `lorentzian_profile()`, `voigt_fwhm()` |
| Preprocessing | `baseline_arpls()`, `baseline_als()`, `baseline_lsp()`, `snv()`, `msc()`, `emsc()`, `normalize()`, `savitzky_golay()`, `wavelet_features()`, `reject_shots()`, `average()` |
| Recipe steps | `step_baseline()`, `step_snv()`, `step_msc()`, `step_emsc()`, `step_spectral_norm()`, `step_line_ratio()`, `step_savgol()`, `step_wavelet()`, `step_line_intensities()`, `step_reject_shots()`, `step_opls()`, `step_o2pls()`, `step_epo()`, `step_glsw()`, and more |
| Orthogonalization | `osc()`, `direct_osc()`, `projected_osc()`, `opls()`, `o2pls()`, `epo()`, `glsw()`, `y_gradient_glsw()` |
| Calibration | `calibration_curve()`, `plot_calibration()`, `nas()`, `pds()` |
| Plasma diagnostics | `saturation_summary()`, `electron_density()`, `boltzmann_plot()`, `saha_boltzmann_plot()`, `plot_boltzmann()`, `mcwhirter_criterion()`, `self_absorption()`, `correct_self_absorption()`, `cf_libs()`, `nist_lines()`, `starkb_lines()` |
| Outliers and robust PCA | `robpca()`, `rospca()`, `macropca()`, `plot_outlier_map()`, `plot_cell_map()`, `q_residuals()`, `dmodx()`, `plot_influence()`, `hotelling_t2()` |
| Robust statistics | `summary_stats()`, `biweight_location()`, `biweight_scale()`, `rousseeuw_croux()`, `umad()`, `adjusted_boxplot()`, `robust_bcyj()` |
| Visualization | `plot_spectra()`, `plot_embedding()`, `plot_loadings()`, `plot_contributions()` |

See the [reference](https://christiangoueguel.com/specProc/reference/)
for the full list.

## License

MIT © Christian L. Goueguel
