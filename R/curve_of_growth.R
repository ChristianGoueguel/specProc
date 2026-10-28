#' @title Self-Absorption by the Curve of Growth
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Fits the curve of growth of a homogeneous plasma in local thermodynamic
#' equilibrium (LTE) to the intensities of emission lines, and returns the
#' optical depth and the self-absorption coefficient of each line, with the
#' intensity it would have if the plasma were optically thin.
#'
#' @details
#' In a homogeneous plasma of length \eqn{l} in LTE, the integrated intensity
#' of a line is
#' \deqn{I = F\, B_\lambda(T) \int \left(1 - e^{-\tau_0 V(\lambda) / V(\lambda_0)}\right) d\lambda}
#' where \eqn{B_\lambda(T)} is the Planck function, \eqn{F} a factor of the
#' instrument and of the geometry, common to all lines, \eqn{V} the line
#' profile and \eqn{\tau_0} the optical depth at the line center,
#' \deqn{\tau_0 = S\, \lambda^4 g_k A_{ki}\, e^{-E_i / k_B T}
#'   \left(1 - e^{-hc / \lambda k_B T}\right) V(\lambda_0)}
#' The column density \eqn{S}, proportional to the number density of the
#' species times \eqn{l} over its partition function, is the same for all
#' lines of a species. When the plasma is optically thin, the intensity
#' grows as \eqn{\tau_0} (it is the intensity of a Boltzmann plot); as
#' \eqn{\tau_0} grows, the line center saturates and the intensity grows
#' only as \eqn{\sqrt{\tau_0}} (Lorentzian wings) or slower. This is the
#' curve of growth of Gornushkin et al. (1999), and the model of the
#' C-sigma graphs of Aragón and Aguilera (2014), with one column density per
#' species, since their concentrations are unknown.
#'
#' The profile is a Voigt profile of the Stark (Lorentzian) width of each
#' line, `stark_width`, and of its Doppler (Gaussian) width at the
#' temperature (see [doppler_width()]). The instrumental broadening, which
#' does not change the optical depth, must not be included: take the
#' Lorentzian width of a Voigt fit with the instrumental width held fixed
#' (see [peak_fit()]), or the Stark width at the electron density of the
#' plasma (see [stark_width()]). Self-absorption broadens lines, so the
#' widths of strongly absorbed lines should come from the Stark parameters.
#'
#' The factor \eqn{F} and the column densities are fitted by least squares
#' on the logarithms of the intensities (weighted by their relative standard
#' errors when the lines have a column `intensity_se`). The optical depths
#' are determined by the curvature of the intensities: lines of different
#' optical depths, such as resonance and non-resonance lines, are needed,
#' and a species whose lines are all thin keeps a column density near the
#' lower bound, with coefficients close to 1. The self-absorption
#' coefficient is \eqn{SA = I / I_{thin}}, the ratio of the intensity to
#' that of an optically thin plasma (as in [self_absorption()]), and the
#' corrected intensities, \eqn{I / SA}, go to [boltzmann()] or
#' [cf_libs()].
#'
#' @param lines A data frame with one row per line and columns `species`
#'   (such as `"K I"`), `intensity` (integrated intensity, corrected for the
#'   spectral response), `wavelength` (nm), `Aki` (s\eqn{^{-1}}), `gk`, `Ek`
#'   and `Ei` (upper and lower level energies, eV) and `stark_width` (the
#'   Stark full width at half maximum, nm).
#' @param temperature The plasma temperature, in K.
#' @param units The units of `intensity`: `"energy"` (default) or
#'   `"photons"`, as in [boltzmann()].
#'
#' @return `lines`, as a tibble of class `specproc_curve_of_growth`, with
#'   the `measured_intensity`, the Doppler width (`doppler_width`, nm), the
#'   `optical_depth` at the line center, the self-absorption coefficient
#'   `SA`, the corrected `intensity` (\eqn{I / SA}) and the `residual` of the
#'   fit (on the logarithm of the intensity). The attributes hold the
#'   `temperature`, the `units`, the fitted `scale` (\eqn{F}), the
#'   `column_density` of each species (a tibble), and the root mean square
#'   residual `rmse`. Draw it with [plot_curve_of_growth()].
#'
#' @references
#'  - Aragón, C., Aguilera, J.A. (2014). CSigma graphs: a new approach for
#'    plasma characterization in laser-induced breakdown spectroscopy.
#'    Journal of Quantitative Spectroscopy and Radiative Transfer,
#'    149:90-102.
#'  - Gornushkin, I.B., Anzano, J.M., King, L.A., Smith, B.W., Omenetto, N.,
#'    Winefordner, J.D. (1999). Curve of growth methodology applied to
#'    laser-induced plasma emission spectroscopy. Spectrochimica Acta Part B,
#'    54(3-4):491-503.
#'
#' @seealso [plot_curve_of_growth()], [correct_self_absorption()],
#'   [self_absorption()], [stark_width()], [cf_libs()]
#' @export curve_of_growth
#'
#' @examples
#' # simulated lines of a species: two resonance lines (Ei = 0), strongly
#' # absorbed, and four lines from excited levels
#' lines <- data.frame(
#'   species = "Ca I", wavelength = c(400, 405, 520, 560, 610, 650),
#'   Aki = c(1.5e8, 1.2e8, 3e7, 5e7, 4e7, 2e7), gk = c(4, 2, 6, 4, 3, 5),
#'   Ei = c(0, 0, 2.0, 2.1, 2.2, 2.4), stark_width = 0.02
#' )
#' lines$Ek <- lines$Ei + 1239.842 / lines$wavelength
#' # their intensities in a plasma where the strongest line has an optical
#' # depth of 5 at its center
#' truth <- curve_of_growth_model(lines, temperature = 9000, scale = 1e20, optical_depth = 5)
#' truth
#' lines$intensity <- truth$intensity
#'
#' cog <- curve_of_growth(lines, temperature = 9000)
#' cog[c("wavelength", "optical_depth", "SA")]
#' plot_curve_of_growth(cog)
#' # the corrected intensities lie on the Boltzmann plot
#' boltzmann(cog)
curve_of_growth <- function(lines, temperature, units = "energy") {
  units <- match.arg(units, c("energy", "photons"))
  lines <- check_lines(lines, c("species", "intensity", "wavelength", "Aki", "gk", "Ek"))
  for (col in c("Ei", "stark_width")) {
    if (!col %in% names(lines) || !is.numeric(lines[[col]]) || anyNA(lines[[col]])) {
      stop("'lines' needs a numeric column `", col, "` without missing values.", call. = FALSE)
    }
  }
  if (any(lines$stark_width < 0)) {
    stop("The Stark widths must not be negative.", call. = FALSE)
  }
  check_number(temperature, "temperature", lower = 0, lower_open = TRUE)
  species <- vapply(as.character(lines$species), function(s) parse_species(s)$label, character(1),
                    USE.NAMES = FALSE)
  groups <- unique(species)
  n <- nrow(lines)
  if (n <= length(groups) + 1) {
    stop("The curve of growth needs more lines than fitted parameters (", length(groups) + 1,
         ").", call. = FALSE)
  }
  shape <- cog_shapes(lines, species, temperature, units)
  weight <- if ("intensity_se" %in% names(lines)) 1 / sdlog(line_intensity_rsd(lines))^2 else
    rep(1, n)
  weight[!is.finite(weight)] <- max(weight[is.finite(weight)], 1)
  log_i <- log(lines$intensity)
  g <- match(species, groups)

  # bounds of log S: the largest optical depth of each species within the tables
  top <- vapply(groups, function(s) max(log(shape$sigma[species == s])), numeric(1),
                USE.NAMES = FALSE)
  theta <- cog_fit(log_i, weight, shape, g, top)
  tau <- exp(theta[-1][g]) * shape$sigma
  width <- vapply(seq_len(n), function(j) cog_width(shape$tables[[j]], tau[j]), numeric(1))
  sa <- pmin(width * shape$peak / tau, 1)
  residual <- log_i - cog_log_intensity(theta, shape, g)

  # per species (computed here: inside tibble(), `species` would be the new column)
  n_lines <- as.integer(table(factor(species, groups)))
  deepest <- vapply(groups, function(s) max(tau[species == s]), numeric(1), USE.NAMES = FALSE)
  out <- tibble::as_tibble(lines)
  out$measured_intensity <- lines$intensity
  out$doppler_width <- shape$doppler
  out$optical_depth <- tau
  out$SA <- sa
  out$intensity <- lines$intensity / sa
  out$residual <- residual
  if ("intensity_se" %in% names(lines)) out$intensity_se <- lines$intensity_se / sa
  structure(
    out, class = c("specproc_curve_of_growth", class(out)), temperature = temperature,
    units = units, scale = exp(theta[1]), rmse = sqrt(mean(residual^2)), n = n,
    column_density = tibble::tibble(species = groups, column_density = exp(theta[-1]),
                                    n_lines = n_lines, max_optical_depth = deepest)
  )
}

#' @title Intensities of Emission Lines on a Curve of Growth
#'
#' @description
#' Computes the integrated intensities and optical depths of emission lines
#' of a homogeneous plasma in LTE, given the column density of their
#' species, by the model of [curve_of_growth()], for simulations and for
#' checking a fit.
#'
#' @inheritParams curve_of_growth
#' @param scale The factor \eqn{F} of the instrument and geometry.
#' @param optical_depth The largest optical depth at the line center of the
#'   lines of each species, which sets its column density: one value, or
#'   one per species named by species.
#'
#' @return A tibble with, for each line, its `wavelength`, `optical_depth`,
#'   self-absorption coefficient `SA`, `intensity` and the intensity of an
#'   optically thin plasma (`thin_intensity`).
#'
#' @seealso [curve_of_growth()]
#' @export curve_of_growth_model
#'
#' @examples
#' lines <- data.frame(species = "Ca I", wavelength = c(400, 520), Aki = c(1.5e8, 3e7),
#'                     gk = c(4, 6), Ei = c(0, 2), stark_width = 0.02)
#' lines$Ek <- lines$Ei + 1239.842 / lines$wavelength
#' curve_of_growth_model(lines, temperature = 9000, scale = 1, optical_depth = 5)
curve_of_growth_model <- function(lines, temperature, scale, optical_depth, units = "energy") {
  units <- match.arg(units, c("energy", "photons"))
  check_number(temperature, "temperature", lower = 0, lower_open = TRUE)
  check_number(scale, "scale", lower = 0, lower_open = TRUE)
  needed <- c("species", "wavelength", "Aki", "gk", "Ek", "Ei", "stark_width")
  absent <- setdiff(needed, names(lines))
  if (length(absent) > 0) {
    stop("'lines' needs the column(s) ", paste(absent, collapse = ", "), ".", call. = FALSE)
  }
  species <- vapply(as.character(lines$species), function(s) parse_species(s)$label, character(1),
                    USE.NAMES = FALSE)
  if (length(optical_depth) == 1 && is.null(names(optical_depth))) {
    optical_depth <- stats::setNames(rep(optical_depth, length(unique(species))), unique(species))
  }
  if (!is.numeric(optical_depth) || any(optical_depth <= 0) ||
      !all(species %in% names(optical_depth))) {
    stop("'optical_depth' must be positive, one value or one per species named by species.",
         call. = FALSE)
  }
  shape <- cog_shapes(lines, species, temperature, units)
  strongest <- as.vector(tapply(shape$sigma, species, max)[species])
  tau <- as.vector(optical_depth[species]) / strongest * shape$sigma
  width <- vapply(seq_along(tau), function(j) cog_width(shape$tables[[j]], tau[j]), numeric(1))
  intensity <- scale * shape$planck * width
  thin <- scale * shape$planck * tau / shape$peak
  tibble::tibble(wavelength = lines$wavelength, optical_depth = tau, SA = intensity / thin,
                 intensity = intensity, thin_intensity = thin)
}

#' @title Plot a Curve of Growth
#'
#' @description
#' Draws the curve of growth fitted by [curve_of_growth()]: the reduced
#' intensity of each line against its optical depth at the line center, on
#' logarithmic axes, with the optically thin limit and the curve of growth
#' of the median line profile.
#'
#' @details
#' The reduced intensity, \eqn{I\,V(\lambda_0) / (F B_\lambda(T))}, equals
#' the optical depth for an optically thin line, and bends below it as the
#' line saturates. Points of all species lie on the same curve when the
#' model holds (up to the differences of their line profiles), as in the
#' C-sigma graphs of Aragón and Aguilera (2014).
#'
#' @param object The result of [curve_of_growth()].
#' @param title The plot title. By default, the temperature and the largest
#'   optical depth.
#'
#' @return A ggplot object.
#' @seealso [curve_of_growth()]
#' @export plot_curve_of_growth
#'
#' @examples
#' lines <- data.frame(
#'   species = "Ca I", wavelength = c(400, 405, 520, 560, 610, 650),
#'   Aki = c(1.5e8, 1.2e8, 3e7, 5e7, 4e7, 2e7), gk = c(4, 2, 6, 4, 3, 5),
#'   Ei = c(0, 0, 2.0, 2.1, 2.2, 2.4), stark_width = 0.02
#' )
#' lines$Ek <- lines$Ei + 1239.842 / lines$wavelength
#' lines$intensity <- curve_of_growth_model(lines, 9000, scale = 1e20, optical_depth = 5)$intensity
#' plot_curve_of_growth(curve_of_growth(lines, temperature = 9000))
plot_curve_of_growth <- function(object, title = NULL) {
  if (!inherits(object, "specproc_curve_of_growth") || is.null(attr(object, "scale"))) {
    stop("'object' must be returned by curve_of_growth().", call. = FALSE)
  }
  temperature <- attr(object, "temperature")
  units <- attr(object, "units")
  d <- as.data.frame(unclass_cog(object))
  shape <- cog_shapes(d, d$species, temperature, units)
  d$reduced <- d$measured_intensity * shape$peak / (attr(object, "scale") * shape$planck)
  # the curve of the median profile, as a reduced width (W V0) of the optical depth
  j <- which.min(abs(shape$damping - stats::median(shape$damping)))
  tau <- 10^seq(log10(max(min(d$optical_depth) / 10, cog_tau[1])),
                log10(min(max(d$optical_depth) * 10, cog_tau[length(cog_tau)])), length.out = 200)
  curve <- data.frame(tau = tau, reduced = vapply(tau, function(t) cog_width(shape$tables[[j]], t),
                                                  numeric(1)) * shape$peak[j])
  if (is.null(title)) {
    title <- sprintf("Curve of growth: T = %.0f K, largest optical depth %s", temperature,
                     format(signif(max(d$optical_depth), 3)))
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = curve, ggplot2::aes(.data$tau, .data$tau), colour = "grey50",
                       linetype = "dashed") +
    ggplot2::geom_line(data = curve, ggplot2::aes(.data$tau, .data$reduced), colour = "grey30") +
    ggplot2::geom_point(data = d, ggplot2::aes(.data$optical_depth, .data$reduced,
                                               colour = .data$species), size = 2.5) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(x = "Optical depth at the line center",
                  y = "Reduced intensity", colour = NULL, title = title,
                  subtitle = "Dashed: optically thin limit; solid: curve of the median line profile") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  finish_title(p)
}

#' @export
print.specproc_curve_of_growth <- function(x, ...) {
  if (is.null(attr(x, "scale")) || !identical(attr(x, "n"), nrow(x))) {   # a subset
    print(tibble::as_tibble(unclass_cog(x)), ...)
    return(invisible(x))
  }
  cat("Curve of growth (", nrow(x), " lines, T = ", format(round(attr(x, "temperature"))),
      " K; RMS residual ", format(signif(attr(x, "rmse"), 3)), " in log intensity)\n\n", sep = "")
  cd <- as.data.frame(attr(x, "column_density"))
  cd$column_density <- signif(cd$column_density, 4)
  cd$max_optical_depth <- signif(cd$max_optical_depth, 3)
  print(cd, row.names = FALSE)
  cat("\n")
  cols <- intersect(c("species", "wavelength", "measured_intensity", "optical_depth", "SA",
                      "intensity"), names(x))
  print(tibble::as_tibble(unclass_cog(x))[cols], ...)
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

# Optical depths of the tables of the curve of growth.
cog_tau <- 10^seq(-4, 5, length.out = 181)

unclass_cog <- function(x) {
  class(x) <- setdiff(class(x), "specproc_curve_of_growth")
  for (a in c("temperature", "units", "scale", "rmse", "n", "column_density")) attr(x, a) <- NULL
  x
}

# The line shapes of the model: the line strength sigma (tau0 = S sigma),
# the Planck function, the peak of the normalized Voigt profile, the Doppler
# widths and the tables of the integrated absorption of each line.
cog_shapes <- function(lines, species, temperature, units) {
  c2 <- 1.4387769e7   # hc / k, nm K
  lambda <- lines$wavelength
  element <- vapply(as.character(species), function(s) parse_species(s)$element, character(1),
                    USE.NAMES = FALSE)
  doppler <- doppler_width(lambda, temperature, element)
  stark <- lines$stark_width
  peak <- vapply(seq_along(lambda), function(j) profile_voigt(0, 0, doppler[j], stark[j]),
                 numeric(1))
  x <- c2 / (lambda * temperature)
  planck <- if (units == "energy") 1 / (lambda^5 * expm1(x)) else 1 / (lambda^4 * expm1(x))
  sigma <- lambda^4 * lines$gk * lines$Aki * exp(-lines$Ei / (k_boltzmann_ev * temperature)) *
    (-expm1(-x)) * peak
  tables <- lapply(seq_along(lambda), function(j) cog_table(doppler[j], stark[j], peak[j]))
  list(sigma = sigma, planck = planck, peak = peak, doppler = doppler,
       damping = stark / pmax(doppler, .Machine$double.eps), tables = tables)
}

# The integrated absorption W(tau0) = int (1 - exp(-tau0 V / V0)) dlambda of
# a Voigt profile, on the grid cog_tau, with its Lorentzian tail beyond the
# integration range added analytically.
cog_table <- function(w_g, w_l, peak) {
  fwhm <- voigt_fwhm(w_g, w_l)
  u <- unique(c(seq(0, 5, by = 0.01), 10^seq(log10(5.01), log10(2000), length.out = 800)))
  x <- u * fwhm
  ratio <- profile_voigt(x, 0, w_g, w_l) / peak
  dx <- diff(x)
  # beyond max(x), int V / V0 of the Lorentzian wings (the absorption is linear there)
  tail <- (w_l / 2) / (pi * max(x)) / peak
  width <- vapply(cog_tau, function(t) {
    f <- -expm1(-t * ratio)
    2 * (sum((f[-1] + f[-length(f)]) * dx) / 2 + t * tail)
  }, numeric(1))
  stats::splinefun(log(cog_tau), log(width), method = "monoH.FC")
}

# W(tau0) from a table, extrapolated as optically thin below the grid.
cog_width <- function(table, tau) {
  if (tau < cog_tau[1]) {
    return(exp(table(log(cog_tau[1]))) * tau / cog_tau[1])
  }
  exp(table(log(min(tau, cog_tau[length(cog_tau)]))))
}

# Least-squares fit of theta = (log F, log S by species). For given column
# densities, log F is the weighted mean residual; each column density is
# fitted in turn on a grid of optical depths, refined by optimize(), until
# they converge, then all parameters are polished together.
cog_fit <- function(log_i, weight, shape, g, top) {
  m <- length(top)
  grid <- seq(log(cog_tau[1]), log(cog_tau[length(cog_tau)]), length.out = 46)
  profile <- function(log_s) {
    model <- cog_log_intensity(c(0, log_s), shape, g)
    log_f <- sum(weight * (log_i - model)) / sum(weight)
    list(value = sum(weight * (log_i - model - log_f)^2), log_f = log_f)
  }
  log_s <- log(0.01) - top
  for (cycle in seq_len(50)) {
    previous <- log_s
    for (s in seq_len(m)) {
      one <- function(v) {
        ls <- log_s
        ls[s] <- v
        profile(ls)$value
      }
      values <- vapply(grid - top[s], one, numeric(1))
      i <- which.min(values)
      range <- grid[c(max(1, i - 1), min(length(grid), i + 1))] - top[s]
      log_s[s] <- if (range[1] < range[2]) stats::optimize(one, range, tol = 1e-10)$minimum else
        range[1]
    }
    if (max(abs(log_s - previous)) < 1e-8) break
  }
  start <- c(profile(log_s)$log_f, log_s)
  objective <- function(theta) {
    sum(weight * (log_i - cog_log_intensity(theta, shape, g))^2)
  }
  polished <- stats::optim(start, objective, method = "L-BFGS-B",
                           lower = c(-Inf, grid[1] - top), upper = c(Inf, grid[length(grid)] - top),
                           control = list(maxit = 500, factr = 10))
  if (polished$value <= objective(start)) polished$par else start
}

# log of the model intensities for parameters theta = (log F, log S by species).
cog_log_intensity <- function(theta, shape, g) {
  tau <- exp(theta[-1][g]) * shape$sigma
  width <- vapply(seq_along(tau), function(j) cog_width(shape$tables[[j]], tau[j]), numeric(1))
  theta[1] + log(shape$planck) + log(width)
}
