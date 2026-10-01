#' @title Plasma Temperature from a Boltzmann Plot
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Estimates the excitation temperature of a plasma in local thermodynamic
#' equilibrium (LTE) from the intensities of several lines of the same
#' species, by a Boltzmann plot.
#'
#' @details
#' In LTE, the integrated intensity of a line emitted from an upper level of
#' energy \eqn{E_k} and degeneracy \eqn{g_k}, with transition probability
#' \eqn{A_{ki}} and wavelength \eqn{\lambda}, satisfies
#' \deqn{\ln\frac{I\lambda}{g_k A_{ki}} = -\frac{E_k}{k_B T} + C}
#' for intensities in energy units, or
#' \eqn{\ln(I / g_k A_{ki})} for photon counts (`units = "photons"`). A
#' least-squares line through the points gives the temperature from its
#' slope, \eqn{T = -1/(k_B\,\mathrm{slope})}, and its standard error from
#' that of the slope.
#'
#' The intensities must be corrected for the spectral response of the
#' instrument, and the lines must be optically thin (see
#' [self_absorption()]). A wide range of upper-level energies gives a more
#' precise temperature. Atomic data (\eqn{A_{ki}}, \eqn{g_k}, \eqn{E_k}) can
#' be taken from the NIST Atomic Spectra Database
#' (\url{https://physics.nist.gov/PhysRefData/ASD/lines_form.html}).
#'
#' @param lines A data frame with one row per line and columns `intensity`
#'   (integrated line intensity), `wavelength` (nm), `Aki` (transition
#'   probability, s\eqn{^{-1}}), `gk` (upper-level degeneracy) and `Ek`
#'   (upper-level energy, eV).
#' @param units The units of `intensity`: `"energy"` (default; radiance or
#'   energy-calibrated counts) or `"photons"` (photon counts).
#'
#' @return An object of class `specproc_boltzmann`, a list with
#'  - `temperature` and `temperature_se`: the temperature and its standard
#'    error, in K;
#'  - `r_squared`: the coefficient of determination of the fit;
#'  - `points`: a tibble with the abscissa `x` (eV) and ordinate `y` of each
#'    line;
#'  - `fit`: the `lm` fit of `y` on `x`.
#'
#' Use [plot_boltzmann()] to draw the plot.
#'
#' @references
#'  - Cristoforetti, G., De Giacomo, A., Dell'Aglio, M., Legnaioli, S.,
#'    Tognoni, E., Palleschi, V., Omenetto, N. (2010). Local thermodynamic
#'    equilibrium in laser-induced breakdown spectroscopy: beyond the
#'    McWhirter criterion. Spectrochimica Acta Part B, 65(1):86-95.
#'
#' @seealso [saha_boltzmann_plot()], [plot_boltzmann()],
#'   [mcwhirter_criterion()]
#' @export boltzmann_plot
#'
#' @examples
#' # Lines of a hypothetical species emitted by a plasma at 10000 K
#' kB <- 8.617333262e-5
#' lines <- data.frame(
#'   wavelength = c(400, 420, 450, 480, 500),
#'   Aki = c(1e8, 5e7, 2e7, 8e7, 3e7),
#'   gk = c(3, 5, 7, 5, 9),
#'   Ek = c(3.1, 3.9, 4.6, 5.3, 6.0)
#' )
#' lines$intensity <- with(lines, gk * Aki / wavelength * exp(-Ek / (kB * 10000)))
#' fit <- boltzmann_plot(lines)
#' fit
#' plot_boltzmann(fit)
boltzmann_plot <- function(lines, units = "energy") {
  units <- match.arg(units, c("energy", "photons"))
  lines <- check_lines(lines, c("intensity", "wavelength", "Aki", "gk", "Ek"))
  if (nrow(lines) < 3) {
    stop("A Boltzmann plot needs at least 3 lines.")
  }
  points <- tibble::tibble(
    x = lines$Ek,
    y = boltzmann_ordinate(lines, units),
    stage = if ("stage" %in% names(lines)) as.integer(lines$stage) else rep(1L, nrow(lines))
  )
  boltzmann_result(points, method = "Boltzmann", units = units)
}

#' @title Plasma Temperature from a Saha-Boltzmann Plot
#'
#' @description
#' Estimates the temperature of a plasma in LTE from lines of the neutral
#' atom and of the singly charged ion of the same element, by a
#' Saha-Boltzmann plot.
#'
#' @details
#' Lines of the ion are placed on the Boltzmann plot of the neutral atom
#' with the Saha equation. For ionic lines, the abscissa becomes
#' \eqn{E_k + E_{ion}} and the ordinate
#' \deqn{y^* = \ln\frac{I\lambda}{g_k A_{ki}} - \ln\left(\frac{2}{N_e}\left(\frac{2\pi m_e k_B T}{h^2}\right)^{3/2}\right)}
#' where \eqn{E_{ion}} is the ionization energy of the neutral atom and
#' \eqn{N_e} the electron density (Aguilera and Aragón, 2004). Because the
#' correction depends on \eqn{T}, the fit is iterated until the temperature
#' converges. The lowering of the ionization energy in the plasma is
#' neglected. The much wider energy range than in a Boltzmann plot of a
#' single species gives a more precise temperature.
#'
#' @param lines A data frame as for [boltzmann_plot()], with an additional
#'   column `stage`: 1 for lines of the neutral atom, 2 for lines of the
#'   singly charged ion.
#' @param ionization_energy The ionization energy of the neutral atom, in eV.
#' @param electron_density The electron density, in cm\eqn{^{-3}}, for
#'   example from [electron_density()].
#' @param units The units of `intensity`: `"energy"` (default) or
#'   `"photons"`.
#' @param max_iter The maximum number of iterations. Default is 100.
#' @param tol The relative tolerance on the temperature. Default is 1e-8.
#'
#' @return An object of class `specproc_boltzmann`, as for
#'   [boltzmann_plot()], with the number of iterations in `iterations`.
#'
#' @references
#'  - Aguilera, J.A., Aragón, C. (2004). Characterization of a laser-induced
#'    plasma by spatially resolved spectroscopy of neutral atom and ion
#'    emissions: comparison of local and spatially integrated measurements.
#'    Spectrochimica Acta Part B, 59(12):1861-1876.
#'
#' @seealso [boltzmann_plot()], [electron_density()], [plot_boltzmann()]
#' @export saha_boltzmann_plot
#'
#' @examples
#' kB <- 8.617333262e-5
#' T <- 12000; ne <- 1e17; E_ion <- 6.11
#' lines <- data.frame(
#'   stage = c(1, 1, 1, 2, 2, 2),
#'   wavelength = c(420, 445, 560, 390, 395, 850),
#'   Aki = c(2e8, 8e7, 5e7, 1.5e8, 1.4e8, 1e7),
#'   gk = c(3, 5, 7, 4, 2, 6),
#'   Ek = c(2.9, 4.7, 5.0, 3.1, 3.2, 3.2)
#' )
#' saha <- 2 * (2 * pi * 9.1093837015e-31 * 1.380649e-23 * T / 6.62607015e-34^2)^1.5 * 1e-6 / ne
#' lines$intensity <- with(lines, gk * Aki / wavelength *
#'   exp(-(Ek + (stage == 2) * E_ion) / (kB * T)) * ifelse(stage == 2, saha, 1))
#' saha_boltzmann_plot(lines, ionization_energy = E_ion, electron_density = ne)
saha_boltzmann_plot <- function(lines, ionization_energy, electron_density, units = "energy",
                                max_iter = 100, tol = 1e-8) {
  units <- match.arg(units, c("energy", "photons"))
  lines <- check_lines(lines, c("intensity", "wavelength", "Aki", "gk", "Ek", "stage"))
  if (!all(lines$stage %in% c(1, 2)) || !all(c(1, 2) %in% lines$stage)) {
    stop("'stage' must be 1 (neutral) or 2 (ion), with lines of both stages.")
  }
  check_number(ionization_energy, "ionization_energy", lower = 0, lower_open = TRUE)
  check_number(electron_density, "electron_density", lower = 0, lower_open = TRUE)
  check_count(max_iter, "max_iter")
  if (nrow(lines) < 3) {
    stop("A Saha-Boltzmann plot needs at least 3 lines.")
  }
  y0 <- boltzmann_ordinate(lines, units)
  ion <- lines$stage == 2
  x <- lines$Ek + ion * ionization_energy
  temperature <- 1e4
  for (iter in seq_len(max_iter)) {
    y <- y0 - ion * log(saha_factor(temperature, electron_density))
    slope <- stats::coef(stats::lm(y ~ x))[[2]]
    if (!is.finite(slope) || slope >= 0) {
      stop("The Saha-Boltzmann plot has a non-negative slope; check the line data.")
    }
    new_temperature <- -1 / (k_boltzmann_ev * slope)
    converged <- abs(new_temperature - temperature) <= tol * new_temperature
    temperature <- new_temperature
    if (converged) break
  }
  if (!converged) {
    warning("The temperature did not converge in ", max_iter, " iterations.", call. = FALSE)
  }
  points <- tibble::tibble(x = x, y = y0 - ion * log(saha_factor(temperature, electron_density)),
                          stage = as.integer(lines$stage))
  res <- boltzmann_result(points, method = "Saha-Boltzmann", units = units)
  res$iterations <- iter
  res$electron_density <- electron_density
  res
}

#' @title Draw a Boltzmann or Saha-Boltzmann Plot
#'
#' @description
#' Plots the points and the fitted line of [boltzmann_plot()] or
#' [saha_boltzmann_plot()], with the estimated temperature, or the parallel
#' Boltzmann plots of the species of [cf_libs()].
#'
#' @param object An object returned by [boltzmann_plot()],
#'   [saha_boltzmann_plot()] or [cf_libs()].
#' @param title The plot title. By default, the method and temperature.
#'
#' @return A ggplot object.
#' @seealso [boltzmann_plot()], [saha_boltzmann_plot()]
#' @export
#'
#' @examples
#' kB <- 8.617333262e-5
#' lines <- data.frame(wavelength = c(400, 450, 500), Aki = c(1e8, 2e7, 3e7),
#'                     gk = c(3, 7, 9), Ek = c(3.1, 4.6, 6.0))
#' lines$intensity <- with(lines, gk * Aki / wavelength * exp(-Ek / (kB * 9000)))
#' plot_boltzmann(boltzmann_plot(lines))
plot_boltzmann <- function(object, title = NULL) {
  if (inherits(object, "specproc_cflibs")) {
    return(plot_cf_libs(object, title))
  }
  if (!inherits(object, "specproc_boltzmann")) {
    stop("'object' must be returned by boltzmann_plot(), saha_boltzmann_plot() or cf_libs().",
         call. = FALSE)
  }
  if (is.null(title)) {
    title <- sprintf("%s plot: T = %.0f +/- %.0f K", object$method, object$temperature,
                     object$temperature_se)
  }
  df <- object$points
  df$stage <- factor(ifelse(df$stage == 2, "ion", "neutral"), levels = c("neutral", "ion"))
  coefs <- stats::coef(object$fit)
  # plotmath expressions draw the symbols on every graphics device, whatever
  # the font encoding (a literal lambda fails on the pdf device in some locales)
  ylab <- if (object$units == "photons") {
    quote(ln(I / (g[k] * A[ki])))
  } else {
    quote(ln(I * lambda / (g[k] * A[ki])))
  }
  if (object$method == "Saha-Boltzmann") {
    ylab <- bquote(.(ylab) ~ "(Saha-corrected for ions)")
  }
  ylab <- as.expression(ylab)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_abline(intercept = coefs[[1]], slope = coefs[[2]], colour = "grey40") +
    ggplot2::geom_point(ggplot2::aes(colour = .data$stage), size = 2.5) +
    ggplot2::scale_colour_manual(values = c(neutral = "#1b9e77", ion = "#d95f02"), drop = TRUE,
                                 name = NULL) +
    ggplot2::labs(x = if (object$method == "Saha-Boltzmann") "Upper-level energy (+ ionization energy for ions), eV"
                  else "Upper-level energy, eV",
                  y = ylab, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = if (length(unique(df$stage)) > 1) "bottom" else "none")
  finish_title(p)
}

# Parallel Boltzmann (or Saha-Boltzmann) plots of a CF-LIBS fit.
plot_cf_libs <- function(object, title) {
  saha_plane <- object$method == "saha-boltzmann"
  if (is.null(title)) {
    title <- sprintf("CF-LIBS %s plots: T = %.0f%s K",
                     if (saha_plane) "Saha-Boltzmann" else "Boltzmann", object$temperature,
                     if (is.finite(object$temperature_se)) sprintf(" +/- %.0f", object$temperature_se) else "")
  }
  slope <- -1 / (k_boltzmann_ev * object$temperature)
  fitted <- data.frame(group = factor(names(object$intercept), levels = levels(object$points$group)),
                       intercept = as.vector(object$intercept), slope = slope)
  ylab <- if (object$units == "photons") {
    quote(ln(I / (g[k] * A[ki])))
  } else {
    quote(ln(I * lambda / (g[k] * A[ki])))
  }
  if (saha_plane) {
    ylab <- bquote(.(ylab) ~ "(Saha-corrected for ions)")
  }
  df <- object$points
  df$stage <- factor(ifelse(df$stage == 2, "ion", "neutral"), levels = c("neutral", "ion"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$group)) +
    ggplot2::geom_abline(data = fitted, ggplot2::aes(intercept = .data$intercept, slope = .data$slope,
                                                     colour = .data$group), alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$stage), size = 2.5) +
    ggplot2::scale_shape_manual(values = c(neutral = 16, ion = 17), drop = TRUE) +
    ggplot2::labs(x = if (saha_plane) "Upper-level energy (+ ionization energy for ions), eV"
                  else "Upper-level energy, eV",
                  y = as.expression(ylab), colour = NULL, shape = NULL, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  finish_title(p)
}

#' @export
print.specproc_boltzmann <- function(x, ...) {
  cat(x$method, " plot (", nrow(x$points), " lines)\n\n", sep = "")
  cat("Temperature:  ", format(round(x$temperature)), " +/- ", format(round(x$temperature_se)),
      " K\n", sep = "")
  cat("R-squared:    ", format(x$r_squared, digits = 4), "\n", sep = "")
  if (!is.null(x$electron_density)) {
    cat("Ne:           ", format(x$electron_density, digits = 3), " cm-3\n", sep = "")
  }
  invisible(x)
}

#' @title McWhirter Criterion for Local Thermodynamic Equilibrium
#'
#' @description
#' Computes the minimum electron density required for local thermodynamic
#' equilibrium (LTE) by the McWhirter criterion, and checks a measured
#' density against it.
#'
#' @details
#' \deqn{N_e \geq 1.6 \times 10^{12}\, T^{1/2} (\Delta E)^3\ \mathrm{cm^{-3}}}
#' with \eqn{T} in K and \eqn{\Delta E} (eV) the largest energy gap between
#' adjacent levels of interest. The criterion is necessary but not
#' sufficient: in transient and inhomogeneous plasmas such as those of LIBS,
#' LTE also requires that equilibration be faster than the plasma evolution
#' and diffusion (Cristoforetti et al., 2010).
#'
#' @param temperature The plasma temperature, in K.
#' @param delta_e The largest energy gap between adjacent levels, in eV.
#' @param electron_density An optional measured electron density, in
#'   cm\eqn{^{-3}}.
#'
#' @return A tibble with the `temperature`, `delta_e`, `minimum_density`
#'   (cm\eqn{^{-3}}) and, if `electron_density` is given, the density and
#'   whether the criterion is `satisfied`. The inputs are recycled to a
#'   common length.
#'
#' @references
#'  - McWhirter, R.W.P. (1965). Spectral intensities. In Huddlestone, R.H.,
#'    Leonard, S.L. (eds.), Plasma Diagnostic Techniques, Academic Press,
#'    New York, pp. 201-264.
#'  - Cristoforetti, G., et al. (2010). Local thermodynamic equilibrium in
#'    laser-induced breakdown spectroscopy: beyond the McWhirter criterion.
#'    Spectrochimica Acta Part B, 65(1):86-95.
#'
#' @export mcwhirter_criterion
#'
#' @examples
#' mcwhirter_criterion(temperature = 10000, delta_e = 3.12, electron_density = 1e17)
mcwhirter_criterion <- function(temperature, delta_e, electron_density = NULL) {
  if (!is.numeric(temperature) || any(temperature <= 0) || anyNA(temperature)) {
    stop("'temperature' must contain positive values (K).")
  }
  if (!is.numeric(delta_e) || any(delta_e <= 0) || anyNA(delta_e)) {
    stop("'delta_e' must contain positive values (eV).")
  }
  n <- max(length(temperature), length(delta_e), length(electron_density))
  out <- tibble::tibble(
    temperature = rep_len(temperature, n),
    delta_e = rep_len(delta_e, n),
    minimum_density = 1.6e12 * sqrt(rep_len(temperature, n)) * rep_len(delta_e, n)^3
  )
  if (!is.null(electron_density)) {
    out$electron_density <- rep_len(electron_density, n)
    out$satisfied <- out$electron_density >= out$minimum_density
  }
  out
}

#' @title Self-Absorption Coefficient from Line Widths
#'
#' @description
#' Estimates the self-absorption coefficient of an emission line from the
#' ratio of its measured Stark width to the width it would have if it were
#' optically thin (El Sherbini et al., 2005).
#'
#' @details
#' Self-absorption flattens and broadens a line. The self-absorption
#' coefficient, the ratio of the measured peak intensity to the intensity
#' without absorption, is
#' \deqn{SA = \left(\frac{\Delta\lambda}{\Delta\lambda_0}\right)^{1/\alpha},
#'   \quad \alpha = -0.54}
#' where \eqn{\Delta\lambda} is the measured Lorentzian (Stark) width and
#' \eqn{\Delta\lambda_0} the width of the optically thin line. The latter
#' follows from the electron density measured on an optically thin line
#' (such as H\eqn{\alpha}, see [electron_density()]) and the Stark width
#' of the line at that density ([stark_width()]). \eqn{SA = 1} means no
#' self-absorption; the measured intensity divided by \eqn{SA} corrects it.
#'
#' @param width The measured Lorentzian full width at half maximum of the
#'   line, in nm.
#' @param thin_width The width of the line if it were optically thin, in nm.
#' @param alpha The exponent of the relation. Default is -0.54.
#'
#' @return A tibble with the self-absorption coefficient `SA` and the
#'   `intensity_correction` factor \eqn{1/SA}.
#'
#' @references
#'  - El Sherbini, A.M., El Sherbini, Th.M., Hegazy, H., Cristoforetti, G.,
#'    Legnaioli, S., Palleschi, V., Pardini, L., Salvetti, A., Tognoni, E.
#'    (2005). Evaluation of self-absorption coefficients of aluminum emission
#'    lines in laser-induced breakdown spectroscopy measurements.
#'    Spectrochimica Acta Part B, 60(12):1573-1579.
#'
#' @seealso [electron_density()], [stark_width()]
#' @export self_absorption
#'
#' @examples
#' # a line twice as wide as expected from its Stark width
#' self_absorption(width = 0.04, thin_width = 0.02)
self_absorption <- function(width, thin_width, alpha = -0.54) {
  if (!is.numeric(width) || anyNA(width) || any(width <= 0)) {
    stop("'width' must contain positive values (nm).")
  }
  if (!is.numeric(thin_width) || anyNA(thin_width) || any(thin_width <= 0)) {
    stop("'thin_width' must contain positive values (nm).")
  }
  check_number(alpha, "alpha", upper = 0, upper_open = TRUE)
  sa <- (width / thin_width)^(1 / alpha)
  if (any(sa > 1 + 1e-8)) {
    warning("Some lines are narrower than their optically thin width (SA > 1); ",
            "check the widths or the electron density.", call. = FALSE)
  }
  tibble::tibble(width = width, thin_width = thin_width, SA = sa, intensity_correction = 1 / sa)
}

#' @title Detect Saturated Channels in Spectra
#'
#' @description
#' Finds the channels at the saturation limit of the detector, for each
#' spectrum and for each wavelength. Saturated lines are clipped, so their
#' intensity and width are wrong and they respond nonlinearly to
#' concentration.
#'
#' @param x A numeric matrix or data frame of spectra, one per row, with the
#'   wavelengths as column names.
#' @param limit The saturation limit of the detector, in counts. Default is
#'   65535 (16-bit detectors).
#' @param tolerance Channels within `tolerance` counts of `limit` are
#'   counted as saturated. Default is 0.
#'
#' @return A list with two tibbles:
#'  - `spectra`: for each spectrum (row), the number `n_saturated` and
#'    fraction of saturated channels;
#'  - `channels`: for each channel saturated in at least one spectrum, its
#'    `wavelength` (from the column names; `NA` if they are not numeric),
#'    column `index`, and the number and fraction of saturated spectra.
#'
#' @export saturation_summary
#'
#' @examples
#' data(forageLIBS)
#' sat <- saturation_summary(forageLIBS[-(1:14)], limit = 65535)
#' sat$channels
saturation_summary <- function(x, limit = 65535, tolerance = 0) {
  x <- as_numeric_matrix(x, "x")
  check_number(limit, "limit")
  check_number(tolerance, "tolerance", lower = 0)
  saturated <- !is.na(x) & x >= limit - tolerance
  per_channel <- colSums(saturated)
  hit <- which(per_channel > 0)
  wl <- parse_wavelength(colnames(x) %||% character(ncol(x)))
  list(
    spectra = tibble::tibble(spectrum = seq_len(nrow(x)), n_saturated = rowSums(saturated),
                             fraction = rowSums(saturated) / ncol(x)),
    channels = tibble::tibble(wavelength = if (length(wl) == ncol(x)) wl[hit] else rep(NA_real_, length(hit)),
                              index = hit, n_spectra = unname(per_channel[hit]),
                              fraction = unname(per_channel[hit]) / nrow(x))
  )
}

# ---- internals ---------------------------------------------------------------

k_boltzmann_ev <- 8.617333262e-5  # eV / K (CODATA 2018, exact)

# 2 (2 pi m_e k T / h^2)^(3/2) / Ne, with Ne in cm^-3
saha_factor <- function(temperature, electron_density) {
  m_e <- 9.1093837015e-31
  k_b <- 1.380649e-23
  h <- 6.62607015e-34
  2 * (2 * pi * m_e * k_b * temperature / h^2)^1.5 * 1e-6 / electron_density
}

boltzmann_ordinate <- function(lines, units) {
  if (units == "photons") {
    log(lines$intensity / (lines$gk * lines$Aki))
  } else {
    log(lines$intensity * lines$wavelength / (lines$gk * lines$Aki))
  }
}

boltzmann_result <- function(points, method, units) {
  fit <- stats::lm(y ~ x, data = points)
  # exactly collinear points (simulated data) trigger a harmless warning
  fit_summary <- suppressWarnings(summary(fit))
  est <- fit_summary$coefficients
  slope <- est["x", "Estimate"]
  if (!is.finite(slope) || slope >= 0) {
    stop("The ", method, " plot has a non-negative slope; check the line data.", call. = FALSE)
  }
  temperature <- -1 / (k_boltzmann_ev * slope)
  se <- if (nrow(points) > 2) est["x", "Std. Error"] / (k_boltzmann_ev * slope^2) else NA_real_
  structure(
    list(temperature = temperature, temperature_se = se, r_squared = fit_summary$r.squared,
         points = points, fit = fit, method = method, units = units),
    class = "specproc_boltzmann"
  )
}

check_lines <- function(lines, needed) {
  if (!is.data.frame(lines)) {
    stop("'lines' must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(needed, names(lines))
  if (length(missing_cols) > 0) {
    stop("'lines' needs the column(s) ", paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }
  num <- intersect(c("intensity", "wavelength", "Aki", "gk", "Ek"), needed)
  vals <- as.matrix(lines[num])
  if (!is.numeric(vals) || anyNA(vals) || any(vals[, setdiff(num, "Ek"), drop = FALSE] <= 0)) {
    stop("'intensity', 'wavelength', 'Aki' and 'gk' must be positive numbers, ",
         "and 'Ek' must not be missing.", call. = FALSE)
  }
  lines
}
