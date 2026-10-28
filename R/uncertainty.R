#' @title Monte Carlo Uncertainty of Plasma Diagnostics
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Propagates the uncertainties of the line intensities, of the transition
#' probabilities and of the electron density to the temperature of a
#' Boltzmann or Saha-Boltzmann plot, or to the temperature and the
#' composition of a calibration-free (CF-LIBS) analysis, by Monte Carlo
#' simulation.
#'
#' @details
#' Each of the `n` draws perturbs the inputs of the fit within their
#' uncertainties and repeats the fit. The standard deviation of the results
#' is their standard uncertainty, and their quantiles give the interval at
#' `level`. The perturbations are lognormal (multiplicative), with the
#' relative standard uncertainties of the inputs, so that intensities,
#' transition probabilities and densities stay positive. The sources are:
#'  - the intensities: the column `intensity_se` of the lines (standard
#'    errors, in the units of `intensity`), as returned by
#'    [line_intensities()];
#'  - the transition probabilities: `aki_rsd`, or else the accuracy grades
#'    of the NIST Atomic Spectra Database in the column `accuracy` (see
#'    [aki_uncertainty()]);
#'  - the electron density, for Saha-Boltzmann plots and CF-LIBS:
#'    `electron_density_se`, for example from
#'    [electron_density_uncertainty()];
#'  - the temperature, for a [cf_libs()] fit given the temperature:
#'    `temperature_se`.
#'
#' A source that is not given is taken as exact. The errors of different
#' lines are taken as independent, and the partition functions and the
#' ionization energies as exact. Lines corrected by
#' [correct_self_absorption()] are not independent: their corrected
#' intensities are computed from their reference line.
#'
#' The interval reflects the stated uncertainties of the inputs, whereas
#' the standard error of the fit (`temperature_se` of the fit) reflects the
#' scatter of the points about the fitted line. The reduced chi-squared
#' compares the two: \eqn{\chi^2_\nu = \sum_i (r_i / \sigma_i)^2 / \nu},
#' where \eqn{r_i} are the residuals of the fit, \eqn{\sigma_i} the standard
#' uncertainties of the ordinates of the points (from those of the
#' intensities and of the transition probabilities) and \eqn{\nu} the
#' residual degrees of freedom. Near 1, the stated uncertainties explain the
#' scatter. Well above 1, other errors scatter the points (self-absorption,
#' spectral response, departures from LTE, or understated uncertainties) and
#' the interval is too narrow. Well below 1, the uncertainties are
#' overstated; the NIST grades are upper bounds.
#'
#' A draw whose Boltzmann plot has a non-negative slope has no finite
#' temperature: it counts as an infinite temperature in the interval, whose
#' upper bound can then be infinite, and is left out of the standard
#' deviation. A CF-LIBS draw that fails is left out, with a warning.
#'
#' Use [set.seed()] for reproducible results.
#'
#' @param object A fit returned by [boltzmann()], [saha_boltzmann()] or
#'   [cf_libs()].
#' @param n The number of draws. Default is 1000.
#' @param level The confidence level of the intervals. Default is 0.95.
#' @param aki_rsd The relative standard uncertainties of the transition
#'   probabilities, such as 0.1 for 10%: one value, or one per line. By
#'   default, they are taken from the column `accuracy` of the lines.
#' @param electron_density_se The standard uncertainty of the electron
#'   density of the fit, in cm\eqn{^{-3}}. Default is 0.
#' @param temperature_se For a [cf_libs()] fit given the temperature: the
#'   standard uncertainty of the temperature, in K. Default is 0.
#'
#' @return An object of class `specproc_uncertainty`, a list with
#'  - `estimates`: a tibble with one row per quantity: the `quantity`
#'    (`"temperature"`, `"atomic_fraction"` or `"mass_fraction"`), the
#'    `element` (CF-LIBS), the `estimate` of the fit, the standard
#'    uncertainty `se` (the standard deviation of the draws), and the
#'    `lower` and `upper` bounds of the interval;
#'  - `draws`: a tibble of the draws, with the `draw` number, the
#'    `temperature` and, for CF-LIBS, the `element`, `atomic_fraction` and
#'    `mass_fraction`;
#'  - `inputs`: a tibble with the relative standard uncertainties of the
#'    intensity (`intensity_rsd`) and of the transition probability
#'    (`aki_rsd`) of each line;
#'  - `electron_density_rsd` and `temperature_rsd`: the relative standard
#'    uncertainties of the electron density and of a given temperature
#'    (`NULL` when not used by the fit);
#'  - `chi_squared`: the reduced chi-squared of the fit (`NA` when the
#'    temperature is given, or when a point has no stated uncertainty);
#'  - `temperature_se`: the standard error of the temperature of the fit;
#'  - `n`, `level`, the number of `failed` draws and the `method`.
#'
#' @references
#'  - JCGM 101:2008. Evaluation of measurement data - Supplement 1 to the
#'    "Guide to the expression of uncertainty in measurement" - Propagation
#'    of distributions using a Monte Carlo method. Joint Committee for
#'    Guides in Metrology.
#'
#' @seealso [aki_uncertainty()], [electron_density_uncertainty()],
#'   [boltzmann()], [saha_boltzmann()], [cf_libs()], [line_intensities()]
#' @export plasma_uncertainty
#'
#' @examples
#' data(forageLIBS)
#' mean_spectrum <- colMeans(forageLIBS[-(1:14)])
#' # calcium lines, with their atomic data and accuracy grades from the NIST
#' # database
#' atomic <- data.frame(
#'   species = c(rep("Ca I", 6), rep("Ca II", 4)),
#'   stage = c(rep(1, 6), rep(2, 4)),
#'   wavelength = c(428.301, 430.253, 431.865, 445.478, 612.222, 616.217,
#'                  315.887, 317.933, 370.603, 373.690),
#'   Aki = c(4.34e7, 1.36e8, 7.40e7, 8.70e7, 2.87e7, 4.77e7, 3.10e8, 3.60e8, 8.80e7, 1.70e8),
#'   accuracy = c("C+", "C+", "C+", "C", "C", "C", "C", "C", "C", "C"),
#'   gk = c(5, 5, 3, 7, 3, 3, 4, 6, 2, 2),
#'   Ek = c(4.780, 4.780, 4.769, 4.681, 3.910, 3.910, 7.047, 7.050, 6.468, 6.468)
#' )
#' lines <- line_intensities(mean_spectrum, atomic, baseline = TRUE)
#' fit <- saha_boltzmann(lines, ionization_energy = 6.113, electron_density = 1.9e17)
#'
#' # with a 15% standard uncertainty of the electron density
#' set.seed(1)
#' u <- plasma_uncertainty(fit, electron_density_se = 0.15 * 1.9e17)
#' u
#' u$estimates
plasma_uncertainty <- function(object, n = 1000, level = 0.95, aki_rsd = NULL,
                               electron_density_se = 0, temperature_se = 0) {
  if (!inherits(object, c("specproc_boltzmann", "specproc_cflibs"))) {
    stop("'object' must be returned by boltzmann(), saha_boltzmann() or cf_libs().",
         call. = FALSE)
  }
  if (is.null(object$lines)) {
    stop("'object' does not keep its lines: fit it again with this version of specProc.",
         call. = FALSE)
  }
  n <- check_count(n, "n", lower = 2)
  check_number(level, "level", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  check_number(electron_density_se, "electron_density_se", lower = 0)
  check_number(temperature_se, "temperature_se", lower = 0)
  cflibs <- inherits(object, "specproc_cflibs")
  fixed_temperature <- cflibs && is.null(object$fit)
  lines <- object$lines
  nl <- nrow(lines)

  intensity_rsd <- line_intensity_rsd(lines)
  aki <- line_aki_rsd(lines, aki_rsd)
  ne <- object$electron_density
  if (electron_density_se > 0 && is.null(ne)) {
    stop("'electron_density_se' needs a fit that uses the electron density ",
         "(saha_boltzmann(), or cf_libs() with `electron_density`).", call. = FALSE)
  }
  if (temperature_se > 0 && !fixed_temperature) {
    stop("'temperature_se' applies to cf_libs() fits given the temperature; the temperature ",
         "of this fit is estimated from the lines.", call. = FALSE)
  }
  ne_rsd <- if (!is.null(ne)) electron_density_se / ne
  t_rsd <- if (fixed_temperature) temperature_se / object$temperature

  # lognormal perturbations of the inputs
  e_int <- matrix(stats::rnorm(nl * n), nl) * sdlog(intensity_rsd)
  e_aki <- matrix(stats::rnorm(nl * n), nl) * sdlog(aki)
  ne_draws <- if (!is.null(ne)) ne * exp(stats::rnorm(n) * sdlog(ne_rsd))
  t_draws <- if (fixed_temperature) object$temperature * exp(stats::rnorm(n) * sdlog(t_rsd))

  probs <- c((1 - level) / 2, (1 + level) / 2)
  failed <- 0L
  if (cflibs) {
    out <- cf_libs_draws(object, e_int, e_aki, ne_draws, t_draws)
    failed <- sum(is.na(out$temperature))
    if (failed > 0) {
      warning(failed, " of the ", n, " draws failed (no CF-LIBS fit) and are left out.",
              call. = FALSE)
    }
    comp <- object$composition
    elements <- comp$element
    estimates <- rbind(
      draws_summary("temperature", NA_character_, object$temperature, out$temperature, probs),
      do.call(rbind, lapply(seq_along(elements), function(j) {
        draws_summary("atomic_fraction", elements[j], comp$atomic_fraction[j], out$atomic[j, ], probs)
      })),
      do.call(rbind, lapply(seq_along(elements), function(j) {
        draws_summary("mass_fraction", elements[j], comp$mass_fraction[j], out$mass[j, ], probs)
      }))
    )
    draws <- tibble::tibble(
      draw = rep(seq_len(n), each = length(elements)),
      element = rep(elements, n),
      temperature = rep(out$temperature, each = length(elements)),
      atomic_fraction = as.vector(out$atomic),
      mass_fraction = as.vector(out$mass)
    )
    method <- paste0("CF-LIBS fit (", if (object$method == "boltzmann") "Boltzmann" else
      "Saha-Boltzmann", " plots)")
  } else {
    dy <- e_int - e_aki   # change of the ordinates ln(I lambda / g A)
    temperature <- if (object$method == "Saha-Boltzmann") {
      s <- object$settings
      t <- saha_temperatures(object$points$x, boltzmann_ordinate(lines, object$units) + dy,
                             object$points$stage == 2, ne_draws, s$max_iter, s$tol)$temperature
      ifelse(is.na(t), Inf, t)
    } else {
      boltzmann_temperatures(object$points$x, object$points$y + dy)
    }
    failed <- sum(!is.finite(temperature))
    estimates <- draws_summary("temperature", NA_character_, object$temperature, temperature, probs)
    draws <- tibble::tibble(draw = seq_len(n), temperature = temperature)
    method <- paste(object$method, "plot")
  }

  sigma <- sqrt(sdlog(intensity_rsd)^2 + sdlog(aki)^2)
  chi_squared <- NA_real_
  if (!is.null(object$fit) && all(sigma > 0) && stats::df.residual(object$fit) > 0) {
    chi_squared <- sum((stats::residuals(object$fit) / sigma)^2) / stats::df.residual(object$fit)
  }
  inputs <- tibble::tibble(wavelength = lines$wavelength, intensity_rsd = intensity_rsd,
                           aki_rsd = aki)
  if ("species" %in% names(lines)) {
    inputs <- tibble::add_column(inputs, species = as.character(lines$species), .before = 1)
  }
  structure(
    list(estimates = estimates, draws = draws, inputs = inputs,
         electron_density_rsd = ne_rsd, temperature_rsd = t_rsd, chi_squared = chi_squared,
         temperature_se = object$temperature_se, n = n, level = level, failed = failed,
         method = method),
    class = "specproc_uncertainty"
  )
}

#' @export
print.specproc_uncertainty <- function(x, ...) {
  cat("Monte Carlo uncertainty of a ", x$method, "\n", x$n, " draws, ",
      format(100 * x$level), "% intervals\n\n", sep = "")
  percent <- function(v) {
    v <- v[v > 0]
    if (length(v) == 0) return("not included")
    r <- signif(100 * range(v), 2)
    if (r[1] == r[2]) paste0(r[1], "%") else paste0(r[1], "% to ", r[2], "%")
  }
  cat("Intensities:              ", percent(x$inputs$intensity_rsd), "\n", sep = "")
  cat("Transition probabilities: ", percent(x$inputs$aki_rsd), "\n", sep = "")
  if (!is.null(x$electron_density_rsd)) {
    cat("Electron density:         ", percent(x$electron_density_rsd), "\n", sep = "")
  }
  if (!is.null(x$temperature_rsd)) {
    cat("Temperature:              ", percent(x$temperature_rsd), "\n", sep = "")
  }
  if (is.finite(x$chi_squared)) {
    cat("Reduced chi-squared:      ", format(x$chi_squared, digits = 3), "\n", sep = "")
  }
  cat("\n")
  est <- as.data.frame(x$estimates)
  temp <- est$quantity == "temperature"
  for (col in c("estimate", "se", "lower", "upper")) {
    est[[col]] <- ifelse(temp, formatC(est[[col]], format = "f", digits = 0),
                         formatC(est[[col]], format = "f", digits = 4))
  }
  if (all(is.na(est$element))) est$element <- NULL else est$element[is.na(est$element)] <- ""
  print(est, row.names = FALSE, right = FALSE)
  if (is.finite(x$temperature_se)) {
    cat("\nStandard error of the fit (scatter of the points): ",
        format(round(x$temperature_se)), " K\n", sep = "")
  }
  if (x$failed > 0) {
    cat("\n", x$failed, " draw(s) without a finite temperature",
        if (grepl("^CF-LIBS", x$method)) " (left out)" else
          " (counted as infinite temperatures in the interval)", ".\n", sep = "")
  }
  invisible(x)
}

#' @title Uncertainty of Transition Probabilities from NIST Accuracy Grades
#'
#' @description
#' Converts the accuracy grades of the transition probabilities of the NIST
#' Atomic Spectra Database into relative standard uncertainties.
#'
#' @details
#' The grades bound the uncertainty of the transition probability (and of
#' the oscillator strength and line strength) at the level of one standard
#' deviation: AAA \eqn{\le} 0.3%, AA \eqn{\le} 1%, A+ \eqn{\le} 2%, A
#' \eqn{\le} 3%, B+ \eqn{\le} 7%, B \eqn{\le} 10%, C+ \eqn{\le} 18%, C
#' \eqn{\le} 25%, D+ \eqn{\le} 40%, D \eqn{\le} 50%, and E > 50%. The bound
#' of each grade is returned, so the uncertainties are upper bounds. Grade
#' E, which has no bound, and unknown grades give `NA`: such lines are best
#' left out of plasma diagnostics.
#'
#' @param accuracy A character vector of accuracy grades, such as the
#'   column `accuracy` returned by [nist_lines()].
#'
#' @return A numeric vector of relative standard uncertainties (0.25 for
#'   25%).
#'
#' @references
#'  - Kramida, A., Ralchenko, Yu., Reader, J., and NIST ASD Team. NIST
#'    Atomic Spectra Database, \url{https://physics.nist.gov/asd}. National
#'    Institute of Standards and Technology, Gaithersburg, MD.
#'
#' @seealso [nist_lines()], [plasma_uncertainty()]
#' @export aki_uncertainty
#'
#' @examples
#' aki_uncertainty(c("AA", "B+", "C", "D", "E"))
aki_uncertainty <- function(accuracy) {
  grades <- c(AAA = 0.003, AA = 0.01, `A+` = 0.02, A = 0.03, `B+` = 0.07, B = 0.10,
              `C+` = 0.18, C = 0.25, `D+` = 0.40, D = 0.50)
  unname(grades[toupper(trimws(as.character(accuracy)))])
}

#' @title Uncertainty of the Electron Density from Stark Broadening
#'
#' @description
#' Computes the electron density from the Stark width of a line, as
#' [electron_density()], with its standard uncertainty and confidence
#' interval, from the standard error of the width and the uncertainty of
#' the Stark parameter of the line.
#'
#' @details
#' The electron density is proportional to \eqn{(w / w_{ref})^p}, where
#' \eqn{w} is the measured width, \eqn{w_{ref}} the reference width of the
#' line (its Stark parameter, or the width law of H\eqn{\alpha}), \eqn{p =
#' 1} for `method = "stark"` and \eqn{p = 1 / 0.67823} for `method =
#' "halpha"`. With lognormal errors of \eqn{w} and \eqn{w_{ref}}, the
#' density is lognormal, with
#' \deqn{\sigma_{\ln N_e} = p \sqrt{\ln(1 + u_w^2) + \ln(1 + u_{ref}^2)}}
#' where \eqn{u_w} and \eqn{u_{ref}} are the relative standard
#' uncertainties of the width and of the reference width. The interval is
#' \eqn{N_e e^{\pm z \sigma_{\ln N_e}}}, and the relative standard
#' uncertainty is \eqn{\sqrt{e^{\sigma^2_{\ln N_e}} - 1}}.
#'
#' The width of H\eqn{\alpha} grows less than linearly with the density, so
#' its relative uncertainty is amplified about 1.5 times in the density.
#' The standard error of the width is that of the fit of the line (the
#' `std.error` of the width in [peak_fit()]), or that of replicate
#' measurements. The uncertainty of the reference width depends on its
#' source: give it with `reference_rsd`. The weak dependence of the Stark
#' width on the temperature is not propagated.
#'
#' @inheritParams electron_density
#' @param width_se The standard error of `width`, in nm: one value, or one
#'   per width. Default is 0.
#' @param reference_rsd The relative standard uncertainty of the reference
#'   width: of the Stark parameter of the line (`method = "stark"`), or of
#'   the width law of H\eqn{\alpha} (`method = "halpha"`). Default is 0.
#' @param level The confidence level of the interval. Default is 0.95.
#'
#' @return A tibble with one row per width: the `width` and `width_se`, the
#'   `electron_density` (cm\eqn{^{-3}}), its standard uncertainty `se`, its
#'   relative standard uncertainty `rsd`, and the `lower` and `upper` bounds
#'   of the interval. Pass `se` to [plasma_uncertainty()] as
#'   `electron_density_se`.
#'
#' @seealso [electron_density()], [plasma_uncertainty()], [peak_fit()]
#' @export electron_density_uncertainty
#'
#' @examples
#' # H-alpha width of 0.50 +/- 0.02 nm, with a 10% uncertainty of the width law
#' electron_density_uncertainty(0.5, width_se = 0.02, method = "halpha", reference_rsd = 0.1)
#'
#' # a line with a Stark width of 0.012 nm at 1e17 cm-3, known within 20%
#' electron_density_uncertainty(0.024, width_se = 0.002, reference_width = 0.012,
#'                              reference_rsd = 0.2)
electron_density_uncertainty <- function(width, width_se = 0, method = "stark", stark = NULL,
                                         wavelength = NULL, temperature = NULL,
                                         reference_width = NULL, reference_density = 1e17,
                                         reference_rsd = 0, tolerance = 0.5, level = 0.95) {
  ne <- electron_density(width, method = method, stark = stark, wavelength = wavelength,
                         temperature = temperature, reference_width = reference_width,
                         reference_density = reference_density, tolerance = tolerance)
  method <- match.arg(method, c("stark", "halpha"))
  if (!is.numeric(width_se) || !length(width_se) %in% c(1, length(width)) || anyNA(width_se) ||
      any(width_se < 0)) {
    stop("'width_se' must contain one non-negative value, or one per width (nm).", call. = FALSE)
  }
  check_number(reference_rsd, "reference_rsd", lower = 0)
  check_number(level, "level", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  power <- if (method == "halpha") 1 / halpha_exponent else 1
  sigma <- power * sqrt(sdlog(width_se / width)^2 + sdlog(reference_rsd)^2)
  rsd <- sqrt(expm1(sigma^2))
  z <- stats::qnorm((1 + level) / 2)
  tibble::tibble(width = width, width_se = rep_len(width_se, length(width)),
                 electron_density = ne, se = ne * rsd, rsd = rsd,
                 lower = ne * exp(-z * sigma), upper = ne * exp(z * sigma))
}

# ---- internals ---------------------------------------------------------------

# Standard deviation of the logarithm of a lognormal variable with relative
# standard uncertainty (coefficient of variation) rsd.
sdlog <- function(rsd) sqrt(log1p(rsd^2))

# Relative standard uncertainties of the intensities of the lines.
line_intensity_rsd <- function(lines) {
  if (!"intensity_se" %in% names(lines)) return(rep(0, nrow(lines)))
  se <- lines$intensity_se
  if (!is.numeric(se) || anyNA(se) || any(se < 0)) {
    stop("The column `intensity_se` of the lines must contain non-negative numbers.",
         call. = FALSE)
  }
  se / lines$intensity
}

# Relative standard uncertainties of the transition probabilities of the lines.
line_aki_rsd <- function(lines, aki_rsd) {
  nl <- nrow(lines)
  if (!is.null(aki_rsd)) {
    if (!is.numeric(aki_rsd) || !length(aki_rsd) %in% c(1, nl) || anyNA(aki_rsd) ||
        any(aki_rsd < 0)) {
      stop("'aki_rsd' must contain one non-negative value, or one per line.", call. = FALSE)
    }
    return(rep_len(aki_rsd, nl))
  }
  if (!"accuracy" %in% names(lines)) return(rep(0, nl))
  u <- aki_uncertainty(lines$accuracy)
  if (anyNA(u)) {
    stop("The accuracy grade of the line(s) at ",
         paste(format(lines$wavelength[is.na(u)]), collapse = ", "),
         " nm gives no uncertainty (grade E or unknown): leave them out, or give 'aki_rsd'.",
         call. = FALSE)
  }
  u
}

# Repeats a CF-LIBS fit with perturbed inputs: temperatures (vector) and
# atomic and mass fractions (elements x draws), NA for the failed draws.
cf_libs_draws <- function(object, e_int, e_aki, ne_draws, t_draws) {
  lines <- object$lines
  s <- object$settings
  n <- ncol(e_int)
  ne_el <- nrow(object$composition)
  out <- list(temperature = rep(NA_real_, n), atomic = matrix(NA_real_, ne_el, n),
              mass = matrix(NA_real_, ne_el, n))
  for (d in seq_len(n)) {
    l <- lines
    l$intensity <- l$intensity * exp(e_int[, d])
    l$Aki <- l$Aki * exp(e_aki[, d])
    fit <- tryCatch(
      suppressWarnings(cf_libs(l, electron_density = ne_draws[d], temperature = t_draws[d],
                               method = object$method, partition = s$partition,
                               ionization_energy = s$ionization_energy,
                               reference = object$reference, units = object$units,
                               timeout = s$timeout)),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    out$temperature[d] <- fit$temperature
    out$atomic[, d] <- fit$composition$atomic_fraction
    out$mass[, d] <- fit$composition$mass_fraction
  }
  out
}

# One row of estimates: the value of the fit and the spread of its draws.
draws_summary <- function(quantity, element, estimate, draws, probs) {
  finite <- draws[is.finite(draws)]
  bounds <- if (all(is.na(draws))) c(NA_real_, NA_real_) else
    stats::quantile(draws, probs, names = FALSE, na.rm = TRUE)
  tibble::tibble(quantity = quantity, element = element, estimate = estimate,
                 se = if (length(finite) > 1) stats::sd(finite) else NA_real_,
                 lower = bounds[1], upper = bounds[2])
}
