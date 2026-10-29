#' @title Tidy and Glance Methods for Fits, Calibrations and Plasma Diagnostics
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `tidy()` summarizes the estimates of a fit, a calibration or a plasma
#' diagnostic in a tibble, one row per estimate, and `glance()` summarizes
#' the fit in a one-row tibble, following the conventions of the broom
#' package. The tibbles of several spectra or samples can then be bound
#' together.
#'
#' @details
#' `tidy()` returns the columns `term`, `estimate` and `std.error`, with the
#' `statistic` and `p.value` of a t test where one applies, and, with
#' `conf.int = TRUE`, the bounds `conf.low` and `conf.high` of the
#' confidence interval at `conf.level`:
#'  - a peak fit (an element of the `fit` column of [peak_fit()] or
#'    [multipeak_fit()]): the parameters of each line and the common shifts,
#'    as in `tidied`; the test is that of a zero value;
#'  - [calibration_curve()]: the `intercept`, `slope` and `quadratic` term,
#'    with the test of a zero value;
#'  - [wavelength_calibration()]: the coefficients of the polynomial
#'    correction of each `segment`, in powers of the wavelength minus the
#'    `center` of the segment (nm), without standard errors;
#'  - [slope_bias()]: the `bias` and `slope`, with the test of no
#'    correction (a bias of 0, a slope of 1);
#'  - [boltzmann()] and [saha_boltzmann()]: the `temperature` (K) and the
#'    `intercept` of the plot; the interval of the temperature is that of
#'    the slope, transformed, so it is not symmetric (its upper bound is
#'    infinite when the interval of the slope reaches zero);
#'  - [cf_libs()]: the `temperature` (with an interval as above), and the
#'    `atomic_fraction` and `mass_fraction` of each `element`, without
#'    standard errors (see [plasma_uncertainty()]);
#'  - [plasma_uncertainty()]: the same quantities with their Monte Carlo
#'    standard uncertainties, and always their intervals, at the `level`
#'    of the object;
#'  - [curve_of_growth()]: the column density of each species (`term`),
#'    with its number of lines (`n.lines`) and its largest optical depth
#'    (`max.optical.depth`).
#'
#' `glance()` returns:
#'  - a peak fit: `sigma`, the root mean square error `rmse`, `isConv`,
#'    `finTol`, `logLik`, `AIC`, `BIC`, `deviance`, `df.residual` and
#'    `nobs`, as for `nls` fits in broom (with `rmse`);
#'  - [calibration_curve()]: `nobs`, `r.squared`, `sigma`, `df.residual`,
#'    the `sensitivity`, the `critical.value` (ISO 11843 only), `lod`,
#'    `loq`, the reduced `chi.squared` of a Deming or York fit, the p-values
#'    of Mandel's test (`mandel.p.value`) and of the lack-of-fit test
#'    (`lack.of.fit.p.value`), the `regression` and the `model`;
#'  - [wavelength_calibration()]: the number of reference lines `n.lines`,
#'    of lines used `nobs`, of `n.segments`, the `degree`, and the `rmse`
#'    and largest absolute residual `max.residual` of the lines used (nm);
#'  - [slope_bias()]: `nobs`, the root mean square differences to the
#'    reference before (`rmse.before`) and after (`rmse.after`) the
#'    correction, and whether the slope was fitted (`slope.fitted`);
#'  - [boltzmann()] and [saha_boltzmann()]: `r.squared`, `sigma`, `nobs`,
#'    `df.residual`, the range of the abscissa `energy.range` (eV), the
#'    `electron.density` and number of `iterations` (Saha-Boltzmann only),
#'    the `method` and the `units`;
#'  - [cf_libs()]: `r.squared` (about the mean), `sigma`, `nobs`,
#'    `df.residual` (`NA` when the temperature is given), the numbers of
#'    `n.species` and `n.elements`, the `electron.density`, the number of
#'    `iterations`, the `method` and the `units`;
#'  - [plasma_uncertainty()]: the number of draws `n.draws`, of `failed`
#'    draws, the reduced `chi.squared` of the fit, the `level` and the
#'    `method`;
#'  - [curve_of_growth()]: the `temperature`, the `rmse` of the fit (on
#'    the log scale), `nobs`, the `scale` and the `units`.
#'
#' @param x A peak fit, or an object returned by [calibration_curve()],
#'   [wavelength_calibration()], [slope_bias()], [boltzmann()],
#'   [saha_boltzmann()], [cf_libs()], [plasma_uncertainty()] or
#'   [curve_of_growth()].
#' @param conf.int A logical: add the confidence interval (`FALSE`,
#'   default).
#' @param conf.level The confidence level of the interval. Default is 0.95.
#' @param ... Not used.
#'
#' @return A tibble.
#'
#' @seealso [peak_fit()], [calibration_curve()], [boltzmann()], [cf_libs()],
#'   [plasma_uncertainty()]
#' @name specproc-tidy
#'
#' @examples
#' data(forageLIBS)
#' mean_spectrum <- colMeans(forageLIBS[-(1:14)])
#' atomic <- data.frame(
#'   species = c(rep("Ca I", 6), rep("Ca II", 4)),
#'   stage = c(rep(1, 6), rep(2, 4)),
#'   wavelength = c(428.301, 430.253, 431.865, 445.478, 612.222, 616.217,
#'                  315.887, 317.933, 370.603, 373.690),
#'   Aki = c(4.34e7, 1.36e8, 7.40e7, 8.70e7, 2.87e7, 4.77e7, 3.10e8, 3.60e8, 8.80e7, 1.70e8),
#'   gk = c(5, 5, 3, 7, 3, 3, 4, 6, 2, 2),
#'   Ek = c(4.780, 4.780, 4.769, 4.681, 3.910, 3.910, 7.047, 7.050, 6.468, 6.468)
#' )
#' lines <- line_intensities(mean_spectrum, atomic, baseline = TRUE)
#' fit <- saha_boltzmann(lines, ionization_energy = 6.113, electron_density = 1.9e17)
#' tidy(fit, conf.int = TRUE)
#' glance(fit)
#'
#' # the K I 769.90 nm line, normalized to the C I 247.86 nm line of the matrix
#' k <- line_intensities(forageLIBS[-(1:14)], c(C = 247.856, K = 769.896), baseline = TRUE)
#' standards <- data.frame(K = forageLIBS$K,
#'                         signal = k$intensity[k$line == "K"] / k$intensity[k$line == "C"])
#' cal <- calibration_curve(standards, signal, K)
#' tidy(cal)
#' glance(cal)
NULL

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

# ---- peak fits ---------------------------------------------------------------

#' @rdname specproc-tidy
#' @export
tidy.specproc_peak_fit <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  out <- fit_parameters(x)
  if (check_conf_int(conf.int, conf.level)) {
    out <- add_conf_int(out, conf.level, stats::df.residual(x))
  }
  out
}

#' @rdname specproc-tidy
#' @export
glance.specproc_peak_fit <- function(x, ...) {
  conv <- x$convInfo
  tibble::tibble(
    sigma = stats::sigma(x), rmse = sqrt(mean(stats::residuals(x)^2)),
    isConv = isTRUE(conv$isConv), finTol = conv$finTol %||% NA_real_,
    logLik = as.numeric(stats::logLik(x)), AIC = stats::AIC(x), BIC = stats::BIC(x),
    deviance = stats::deviance(x), df.residual = stats::df.residual(x), nobs = stats::nobs(x)
  )
}

# ---- calibrations ------------------------------------------------------------

#' @rdname specproc-tidy
#' @export
tidy.specproc_calibration <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  co <- x$coefficients
  df <- calibration_df(x)
  statistic <- co$estimate / co$std_error
  out <- tibble::tibble(term = co$term, estimate = co$estimate, std.error = co$std_error,
                        statistic = statistic, p.value = 2 * stats::pt(-abs(statistic), df))
  if (check_conf_int(conf.int, conf.level)) out <- add_conf_int(out, conf.level, df)
  out
}

#' @rdname specproc-tidy
#' @export
glance.specproc_calibration <- function(x, ...) {
  fom <- x$figures_of_merit
  lin <- x$linearity
  p_value <- function(test) {
    p <- lin$p_value[lin$test == test]
    if (length(p) == 0) NA_real_ else p[[1]]
  }
  tibble::tibble(
    nobs = fom$n, r.squared = fom$r_squared, sigma = fom$sigma, df.residual = calibration_df(x),
    sensitivity = fom$sensitivity, critical.value = fom$critical_value, lod = fom$lod,
    loq = fom$loq, chi.squared = x$eiv$chi_squared %||% NA_real_,
    mandel.p.value = p_value("Mandel"), lack.of.fit.p.value = p_value("Lack of fit"),
    regression = x$regression, model = x$model
  )
}

#' @rdname specproc-tidy
#' @export
tidy.specproc_wavelength_calibration <- function(x, ...) {
  rows <- lapply(names(x$fits), function(s) {
    model <- x$fits[[s]]
    tibble::tibble(segment = as.integer(s), term = power_terms(length(model$coef)),
                   estimate = model$coef, center = model$center)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- tibble::tibble(segment = integer(), term = character(), estimate = numeric(),
                          center = numeric())
  }
  out
}

#' @rdname specproc-tidy
#' @export
glance.specproc_wavelength_calibration <- function(x, ...) {
  used <- x$lines$used
  residual <- x$lines$residual[used]
  tibble::tibble(
    n.lines = nrow(x$lines), nobs = sum(used), n.segments = nrow(x$segments), degree = x$degree,
    rmse = if (any(used)) sqrt(mean(residual^2)) else NA_real_,
    max.residual = if (any(used)) max(abs(residual)) else NA_real_
  )
}

#' @rdname specproc-tidy
#' @export
tidy.specproc_slope_bias <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  co <- x$coefficients
  out <- tibble::tibble(term = co$term, estimate = co$estimate, std.error = co$std_error,
                        statistic = (co$estimate - co$null) / co$std_error, p.value = co$p_value)
  if (check_conf_int(conf.int, conf.level)) {
    out <- add_conf_int(out, conf.level, x$n - if (x$slope) 2 else 1)
  }
  out
}

#' @rdname specproc-tidy
#' @export
glance.specproc_slope_bias <- function(x, ...) {
  tibble::tibble(nobs = x$n, rmse.before = unname(x$rmse[["before"]]),
                 rmse.after = unname(x$rmse[["after"]]), slope.fitted = x$slope)
}

# ---- plasma diagnostics ------------------------------------------------------

#' @rdname specproc-tidy
#' @export
tidy.specproc_boltzmann <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  est <- suppressWarnings(summary(x$fit))$coefficients
  out <- tibble::tibble(term = c("temperature", "intercept"),
                        estimate = c(x$temperature, est[1, "Estimate"]),
                        std.error = c(x$temperature_se, if (nrow(x$points) > 2) est[1, "Std. Error"] else NA_real_))
  if (check_conf_int(conf.int, conf.level)) {
    df <- stats::df.residual(x$fit)
    out <- add_conf_int(out, conf.level, df)
    bounds <- temperature_interval(est["x", "Estimate"], out$std.error[1], x$temperature, conf.level, df)
    out$conf.low[1] <- bounds[1]
    out$conf.high[1] <- bounds[2]
  }
  out
}

#' @rdname specproc-tidy
#' @export
glance.specproc_boltzmann <- function(x, ...) {
  tibble::tibble(
    r.squared = x$r_squared, sigma = suppressWarnings(summary(x$fit))$sigma,
    nobs = nrow(x$points), df.residual = stats::df.residual(x$fit),
    energy.range = diff(range(x$points$x)),
    electron.density = x$electron_density %||% NA_real_,
    iterations = x$iterations %||% NA_integer_, method = x$method, units = x$units
  )
}

#' @rdname specproc-tidy
#' @export
tidy.specproc_cflibs <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  comp <- x$composition
  n <- nrow(comp)
  out <- tibble::tibble(
    term = c("temperature", rep(c("atomic_fraction", "mass_fraction"), each = n)),
    element = c(NA_character_, comp$element, comp$element),
    estimate = c(x$temperature, comp$atomic_fraction, comp$mass_fraction),
    std.error = c(x$temperature_se, rep(NA_real_, 2 * n))
  )
  if (check_conf_int(conf.int, conf.level)) {
    out$conf.low <- NA_real_
    out$conf.high <- NA_real_
    if (!is.null(x$fit)) {
      slope <- stats::coef(x$fit)[["x"]]
      bounds <- temperature_interval(slope, x$temperature_se, x$temperature, conf.level,
                                     stats::df.residual(x$fit))
      out$conf.low[1] <- bounds[1]
      out$conf.high[1] <- bounds[2]
    }
  }
  out
}

#' @rdname specproc-tidy
#' @export
glance.specproc_cflibs <- function(x, ...) {
  fit <- x$fit
  # the group intercepts span the constant: R-squared about the mean
  y <- x$points$y
  r_squared <- if (is.null(fit)) NA_real_ else
    1 - sum(stats::residuals(fit)^2) / sum((y - mean(y))^2)
  tibble::tibble(
    r.squared = r_squared, sigma = if (is.null(fit)) NA_real_ else stats::sigma(fit),
    nobs = nrow(x$points), df.residual = if (is.null(fit)) NA_integer_ else stats::df.residual(fit),
    n.species = nrow(x$species), n.elements = nrow(x$composition),
    electron.density = x$electron_density %||% NA_real_, iterations = x$iterations,
    method = x$method, units = x$units
  )
}

#' @rdname specproc-tidy
#' @export
tidy.specproc_uncertainty <- function(x, ...) {
  e <- x$estimates
  out <- tibble::tibble(term = e$quantity, element = e$element, estimate = e$estimate,
                        std.error = e$se, conf.low = e$lower, conf.high = e$upper)
  if (all(is.na(out$element))) out$element <- NULL
  out
}

#' @rdname specproc-tidy
#' @export
glance.specproc_uncertainty <- function(x, ...) {
  tibble::tibble(n.draws = x$n, failed = x$failed, chi.squared = x$chi_squared %||% NA_real_,
                 level = x$level, method = x$method)
}

#' @rdname specproc-tidy
#' @export
tidy.specproc_curve_of_growth <- function(x, ...) {
  cd <- curve_of_growth_attr(x, "column_density")
  tibble::tibble(term = cd$species, estimate = cd$column_density, n.lines = cd$n_lines,
                 max.optical.depth = cd$max_optical_depth)
}

#' @rdname specproc-tidy
#' @export
glance.specproc_curve_of_growth <- function(x, ...) {
  tibble::tibble(temperature = curve_of_growth_attr(x, "temperature"),
                 rmse = curve_of_growth_attr(x, "rmse"), nobs = curve_of_growth_attr(x, "n"),
                 scale = curve_of_growth_attr(x, "scale"), units = curve_of_growth_attr(x, "units"))
}

# ---- internals ---------------------------------------------------------------

check_conf_int <- function(conf.int, conf.level) {
  check_flag(conf.int, "conf.int")
  if (conf.int) {
    check_number(conf.level, "conf.level", lower = 0, upper = 1, lower_open = TRUE,
                 upper_open = TRUE)
  }
  conf.int
}

# Wald intervals with the t distribution.
add_conf_int <- function(out, conf.level, df) {
  q <- if (is.finite(df) && df > 0) stats::qt(1 - (1 - conf.level) / 2, df) else NA_real_
  out$conf.low <- out$estimate - q * out$std.error
  out$conf.high <- out$estimate + q * out$std.error
  out
}

# The interval of the temperature, from that of the (negative) slope: the
# slope's standard error is recovered from that of the temperature.
temperature_interval <- function(slope, temperature_se, temperature, conf.level, df) {
  if (!is.finite(temperature_se) || !is.finite(df) || df <= 0) return(c(NA_real_, NA_real_))
  slope_se <- temperature_se * k_boltzmann_ev * slope^2
  q <- stats::qt(1 - (1 - conf.level) / 2, df)
  steep <- slope - q * slope_se
  flat <- slope + q * slope_se
  c(-1 / (k_boltzmann_ev * steep), if (flat < 0) -1 / (k_boltzmann_ev * flat) else Inf)
}

power_terms <- function(n) {
  names <- c("intercept", "slope", "quadratic", "cubic")
  out <- names[seq_len(min(n, 4))]
  if (n > 4) out <- c(out, paste0("power_", 4:(n - 1)))
  out
}

curve_of_growth_attr <- function(x, name) {
  value <- attr(x, name)
  if (is.null(value)) {
    stop("'x' has lost the attributes of curve_of_growth(); use the object it returns.",
         call. = FALSE)
  }
  value
}
