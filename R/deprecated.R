#' Defunct functions in specProc
#'
#' These functions were renamed, most of them in specProc 0.2.0 to follow a
#' consistent snake_case naming scheme. They were deprecated until specProc
#' 0.8.3 and are defunct since 0.9.0: they stop with an error that names the
#' new function, and will be removed in a future release.
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
#' | `boltzmann_plot()` | [boltzmann()] |
#' | `saha_boltzmann_plot()` | [saha_boltzmann()] |
#'
#' `boltzmann_plot()` and `saha_boltzmann_plot()` were renamed in specProc
#' 0.8.0, so that they are not confused with [plot_boltzmann()], which
#' draws their result.
#'
#' @param ... Not used.
#' @return None: they stop with an error.
#' @name specProc-defunct
#' @keywords internal
NULL

defunct <- function(old, new) {
  force(new)
  function(...) {
    .Defunct(new, package = "specProc",
             msg = sprintf("'%s()' is defunct; use '%s()' instead.", old, new))
  }
}

# The value of a renamed argument: the former one, with a deprecation
# warning, when the caller used it, and otherwise the new one.
renamed_arg <- function(old_value, new_value, fn, old, new, details = NULL,
                        user_env = rlang::caller_env(2)) {
  if (!lifecycle::is_present(old_value)) {
    return(new_value)
  }
  lifecycle::deprecate_warn("0.9.0", sprintf("%s(%s)", fn, old), sprintf("%s(%s)", fn, new),
                            details = details, user_env = user_env)
  old_value
}

#' @rdname specProc-defunct
#' @export
whittaker <- defunct("whittaker", "baseline_als")

#' @rdname specProc-defunct
#' @export
lorentzian <- defunct("lorentzian", "lorentzian_profile")

#' @rdname specProc-defunct
#' @export
pseudo_voigt <- defunct("pseudo_voigt", "pseudo_voigt_profile")

#' @rdname specProc-defunct
#' @export
peakfit <- defunct("peakfit", "peak_fit")

#' @rdname specProc-defunct
#' @export
multipeakfit <- defunct("multipeakfit", "multipeak_fit")

#' @rdname specProc-defunct
#' @export
plotfit <- defunct("plotfit", "plot_fit")

#' @rdname specProc-defunct
#' @export
plotSpec <- defunct("plotSpec", "plot_spectra")

#' @rdname specProc-defunct
#' @export
outlierplot <- defunct("outlierplot", "plot_outliers")

#' @rdname specProc-defunct
#' @export
directOutlyingness <- defunct("directOutlyingness", "directional_outlyingness")

#' @rdname specProc-defunct
#' @export
iqrMethod <- defunct("iqrMethod", "iqr_outliers")

#' @rdname specProc-defunct
#' @export
robustBCYJ <- defunct("robustBCYJ", "robust_bcyj")

#' @rdname specProc-defunct
#' @export
rousseeuwCroux <- defunct("rousseeuwCroux", "rousseeuw_croux")

#' @rdname specProc-defunct
#' @export
summaryStats <- defunct("summaryStats", "summary_stats")

#' @rdname specProc-defunct
#' @export
tukeyGH <- defunct("tukeyGH", "tukey_gh")

#' @rdname specProc-defunct
#' @export
yGradientglsw <- defunct("yGradientglsw", "y_gradient_glsw")

#' @rdname specProc-defunct
#' @export
pareto <- defunct("pareto", "pareto_scale")

#' @rdname specProc-defunct
#' @export
boltzmann_plot <- defunct("boltzmann_plot", "boltzmann")

#' @rdname specProc-defunct
#' @export
saha_boltzmann_plot <- defunct("saha_boltzmann_plot", "saha_boltzmann")
