#' Deprecated functions in specProc
#'
#' These functions were renamed in specProc 0.2.0 to follow a consistent
#' snake_case naming scheme. The old names still work but issue a deprecation
#' warning and will be removed in a future release.
#'
#' `gaussian()` was renamed to [gaussian_profile()] without an alias, because
#' an exported `gaussian()` masks `stats::gaussian()`, the family used by
#' `glm()`.
#'
#' | Old name | New name |
#' |---|---|
#' | `whittaker()` | [baseline_als()] |
#' | `lorentzian()` | [lorentzian_profile()] |
#' | `pseudo_voigt()` | [pseudo_voigt_profile()] |
#' | `peakfit()` | [peak_fit()] |
#' | `multipeakfit()` | [multipeak_fit()] |
#' | `plotfit()` | [plot_fit()] |
#' | `plotSpec()` | [plot_spectra()] |
#' | `outlierplot()` | [plot_outliers()] |
#' | `directOutlyingness()` | [directional_outlyingness()] |
#' | `iqrMethod()` | [iqr_outliers()] |
#' | `robustBCYJ()` | [robust_bcyj()] |
#' | `rousseeuwCroux()` | [rousseeuw_croux()] |
#' | `summaryStats()` | [summary_stats()] |
#' | `tukeyGH()` | [tukey_gh()] |
#' | `yGradientglsw()` | [y_gradient_glsw()] |
#' | `pareto()` | [pareto_scale()] |
#'
#' @param ... Arguments passed to the new function.
#' @return The result of the new function.
#' @name specProc-deprecated
#' @keywords internal
NULL

deprecate <- function(old, new) {
  force(new)
  function(...) {
    .Deprecated(new, package = "specProc",
                msg = sprintf("'%s()' is deprecated; use '%s()' instead.", old, new))
    get(new, envir = asNamespace("specProc"))(...)
  }
}

#' @rdname specProc-deprecated
#' @export
whittaker <- deprecate("whittaker", "baseline_als")

#' @rdname specProc-deprecated
#' @export
lorentzian <- deprecate("lorentzian", "lorentzian_profile")

#' @rdname specProc-deprecated
#' @export
pseudo_voigt <- deprecate("pseudo_voigt", "pseudo_voigt_profile")

#' @rdname specProc-deprecated
#' @export
peakfit <- deprecate("peakfit", "peak_fit")

#' @rdname specProc-deprecated
#' @export
multipeakfit <- deprecate("multipeakfit", "multipeak_fit")

#' @rdname specProc-deprecated
#' @export
plotfit <- deprecate("plotfit", "plot_fit")

#' @rdname specProc-deprecated
#' @export
plotSpec <- deprecate("plotSpec", "plot_spectra")

#' @rdname specProc-deprecated
#' @export
outlierplot <- deprecate("outlierplot", "plot_outliers")

#' @rdname specProc-deprecated
#' @export
directOutlyingness <- deprecate("directOutlyingness", "directional_outlyingness")

#' @rdname specProc-deprecated
#' @export
iqrMethod <- deprecate("iqrMethod", "iqr_outliers")

#' @rdname specProc-deprecated
#' @export
robustBCYJ <- deprecate("robustBCYJ", "robust_bcyj")

#' @rdname specProc-deprecated
#' @export
rousseeuwCroux <- deprecate("rousseeuwCroux", "rousseeuw_croux")

#' @rdname specProc-deprecated
#' @export
summaryStats <- deprecate("summaryStats", "summary_stats")

#' @rdname specProc-deprecated
#' @export
tukeyGH <- deprecate("tukeyGH", "tukey_gh")

#' @rdname specProc-deprecated
#' @export
yGradientglsw <- deprecate("yGradientglsw", "y_gradient_glsw")

#' @rdname specProc-deprecated
#' @export
pareto <- deprecate("pareto", "pareto_scale")
