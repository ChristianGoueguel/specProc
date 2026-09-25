# Deprecated functions in specProc

These functions were renamed in specProc 0.2.0 to follow a consistent
snake_case naming scheme. The old names still work but issue a
deprecation warning and will be removed in a future release.

## Usage

``` r
whittaker(...)

lorentzian(...)

pseudo_voigt(...)

peakfit(...)

multipeakfit(...)

plotfit(...)

plotSpec(...)

outlierplot(...)

directOutlyingness(...)

iqrMethod(...)

robustBCYJ(...)

rousseeuwCroux(...)

summaryStats(...)

tukeyGH(...)

yGradientglsw(...)

pareto(...)
```

## Arguments

- ...:

  Arguments passed to the new function.

## Value

The result of the new function.

## Details

[`gaussian()`](https://rdrr.io/r/stats/family.html) was renamed to
[`gaussian_profile()`](https://christiangoueguel.com/specProc/reference/gaussian_profile.md)
without an alias, because an exported
[`gaussian()`](https://rdrr.io/r/stats/family.html) masks
[`stats::gaussian()`](https://rdrr.io/r/stats/family.html), the family
used by [`glm()`](https://rdrr.io/r/stats/glm.html).

|  |  |
|----|----|
| Old name | New name |
| `whittaker()` | [`baseline_als()`](https://christiangoueguel.com/specProc/reference/baseline_als.md) |
| `lorentzian()` | [`lorentzian_profile()`](https://christiangoueguel.com/specProc/reference/lorentzian_profile.md) |
| `pseudo_voigt()` | [`pseudo_voigt_profile()`](https://christiangoueguel.com/specProc/reference/pseudo_voigt_profile.md) |
| `peakfit()` | [`peak_fit()`](https://christiangoueguel.com/specProc/reference/peak_fit.md) |
| `multipeakfit()` | [`multipeak_fit()`](https://christiangoueguel.com/specProc/reference/multipeak_fit.md) |
| `plotfit()` | [`plot_fit()`](https://christiangoueguel.com/specProc/reference/plot_fit.md) |
| `plotSpec()` | [`plot_spectra()`](https://christiangoueguel.com/specProc/reference/plot_spectra.md) |
| `outlierplot()` | [`plot_outliers()`](https://christiangoueguel.com/specProc/reference/plot_outliers.md) |
| `directOutlyingness()` | [`directional_outlyingness()`](https://christiangoueguel.com/specProc/reference/directional_outlyingness.md) |
| `iqrMethod()` | [`iqr_outliers()`](https://christiangoueguel.com/specProc/reference/iqr_outliers.md) |
| `robustBCYJ()` | [`robust_bcyj()`](https://christiangoueguel.com/specProc/reference/robust_bcyj.md) |
| `rousseeuwCroux()` | [`rousseeuw_croux()`](https://christiangoueguel.com/specProc/reference/rousseeuw_croux.md) |
| `summaryStats()` | [`summary_stats()`](https://christiangoueguel.com/specProc/reference/summary_stats.md) |
| `tukeyGH()` | [`tukey_gh()`](https://christiangoueguel.com/specProc/reference/tukey_gh.md) |
| `yGradientglsw()` | [`y_gradient_glsw()`](https://christiangoueguel.com/specProc/reference/y_gradient_glsw.md) |
| `pareto()` | [`pareto_scale()`](https://christiangoueguel.com/specProc/reference/pareto_scale.md) |
