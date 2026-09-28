#' @title Self-Absorption Correction with an Internal Reference Line
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Corrects the intensities of emission lines for self-absorption by the
#' internal reference method of Sun and Yu (2009): for each species, the
#' self-absorption coefficient of every line is measured against a line of
#' the same species that is little self-absorbed.
#'
#' @details
#' In an optically thin plasma in LTE, the intensity ratio of two lines of
#' the same species depends only on their atomic data and on the
#' temperature. With a reference line \eqn{r} assumed optically thin, the
#' self-absorption coefficient of a line \eqn{j} is
#' \deqn{SA_j = \frac{I_j}{I_r} \frac{g_r A_r \lambda_j}{g_j A_j \lambda_r}
#'   \exp\left(\frac{E_j - E_r}{k_B T}\right)}
#' (for intensities in energy units; without the wavelengths for photon
#' counts), where \eqn{g}, \eqn{A} and \eqn{E} are the statistical weight,
#' transition probability and energy of the upper level. The corrected
#' intensity is \eqn{I_j / SA_j}.
#'
#' Self-absorption grows with the optical depth of the line, roughly
#' proportional to \eqn{g_k A_{ki} \lambda^4 \exp(-E_i / k_B T)}, where
#' \eqn{E_i} is the energy of the lower level: resonance lines (\eqn{E_i =
#' 0}) and strong lines are the most absorbed. By default, the reference of
#' each species is the line of smallest optical depth, which needs the
#' lower-level energies (column `Ei`, as returned by [nist_lines()]);
#' otherwise, give the reference lines with `reference`. The reference line
#' must be well measured: its noise propagates to every coefficient.
#'
#' The corrected lines of a species lie, by construction, on the Boltzmann
#' plot of its reference line at the temperature used, so they cannot give
#' the temperature by themselves. Give `temperature`, measured on optically
#' thin lines (for example with [saha_boltzmann_plot()] or [cf_libs()]), or
#' give `electron_density`: the temperature is then the one at which the
#' corrected lines of the neutral atom and of the ion of each element lie on
#' a common Saha-Boltzmann plot (as in [cf_libs()]), which needs lines of
#' both stages for at least one element. This is the iterative scheme of Sun
#' and Yu (2009), solved directly for the temperature.
#'
#' Coefficients above 1 mean that the line is stronger than expected from
#' the reference, because of noise, errors of the atomic data, or a
#' self-absorbed reference; they are kept, with a warning.
#'
#' @param lines A data frame with one row per line and columns `species`,
#'   `intensity`, `wavelength` (nm), `Aki` (s\eqn{^{-1}}), `gk`, `Ek`
#'   (upper-level energy, eV) and, for the default reference, `Ei`
#'   (lower-level energy, eV).
#' @param temperature The plasma temperature, in K. Without it, it is
#'   estimated from the corrected lines, which needs `electron_density`.
#' @param electron_density The electron density, in cm\eqn{^{-3}}, to
#'   estimate the temperature (see details).
#' @param ionization_energy The ionization energies of the neutral atoms,
#'   in eV, named by element, to estimate the temperature. By default, they
#'   are taken from the NIST database (see [nist_ionization_energy()]).
#' @param timeout The timeout of the NIST downloads, in seconds.
#' @param reference The reference lines: row numbers of `lines`, one per
#'   species, or `NULL` (default) to choose them by optical depth.
#' @param units The units of `intensity`: `"energy"` (default) or
#'   `"photons"`, as in [boltzmann_plot()].
#' @param tol The relative tolerance on the estimated temperature.
#'   Default is 1e-8.
#'
#' @return `lines`, as a tibble, with the corrected `intensity`, the
#'   `measured_intensity`, the self-absorption coefficient `SA` and a
#'   logical `reference`, ready for [boltzmann_plot()] or [cf_libs()]. The
#'   temperature used is in the attribute `temperature`.
#'
#' @references
#'  - Sun, L., Yu, H. (2009). Correction of self-absorption effect in
#'    calibration-free laser-induced breakdown spectroscopy by an internal
#'    reference method. Talanta, 79(2):388-395.
#'
#' @seealso [self_absorption()] for the coefficient from line widths,
#'   [cf_libs()], [line_intensities()]
#' @export correct_self_absorption
#'
#' @examples
#' # Lines of one species at 9000 K; the resonance line at 400 nm keeps only
#' # 40% of its intensity
#' kB <- 8.617333262e-5
#' lines <- data.frame(
#'   species = "X I", wavelength = c(400, 420, 450, 480, 500),
#'   Aki = c(2e8, 5e7, 2e7, 8e7, 3e7), gk = c(3, 5, 7, 5, 9),
#'   Ek = c(3.1, 3.9, 4.6, 5.3, 6.0), Ei = c(0, 1.0, 1.9, 2.7, 3.5)
#' )
#' lines$intensity <- with(lines, gk * Aki / wavelength * exp(-Ek / (kB * 9000)))
#' lines$intensity[1] <- 0.4 * lines$intensity[1]
#' corrected <- correct_self_absorption(lines, temperature = 9000)
#' corrected[c("wavelength", "measured_intensity", "SA", "reference")]
correct_self_absorption <- function(lines, temperature = NULL, electron_density = NULL,
                                    ionization_energy = NULL, reference = NULL,
                                    units = "energy", tol = 1e-8, timeout = 120) {
  units <- match.arg(units, c("energy", "photons"))
  lines <- check_lines(lines, c("species", "intensity", "wavelength", "Aki", "gk", "Ek"))
  check_number(tol, "tol", lower = 0, lower_open = TRUE)
  if (!is.null(temperature)) {
    check_number(temperature, "temperature", lower = 0, lower_open = TRUE)
  } else if (is.null(electron_density)) {
    stop("Give `temperature`, or `electron_density` to estimate it (see ?correct_self_absorption).",
         call. = FALSE)
  } else {
    check_number(electron_density, "electron_density", lower = 0, lower_open = TRUE)
  }
  species <- as.character(lines$species)
  groups <- unique(species)
  measured <- lines$intensity

  # the reference line of each species
  if (is.null(reference)) {
    if (!"Ei" %in% names(lines) || anyNA(lines$Ei)) {
      stop("Choosing the reference lines needs the lower-level energies (column `Ei`); ",
           "otherwise give `reference`.", call. = FALSE)
    }
  } else {
    if (!is.numeric(reference) || anyNA(reference) || any(reference < 1) ||
        any(reference > nrow(lines)) || any(reference != round(reference))) {
      stop("'reference' must give row numbers of 'lines'.", call. = FALSE)
    }
    ref_species <- species[reference]
    if (anyDuplicated(ref_species) || !setequal(ref_species, groups)) {
      stop("'reference' must give exactly one line per species.", call. = FALSE)
    }
  }
  choose_reference <- function(t) {
    if (!is.null(reference)) return(stats::setNames(reference, species[reference]))
    depth <- lines$gk * lines$Aki * lines$wavelength^4 * exp(-lines$Ei / (k_boltzmann_ev * t))
    stats::setNames(vapply(groups, function(g) {
      rows <- which(species == g)
      rows[which.min(depth[rows])]
    }, integer(1)), groups)
  }
  sa_coefficients <- function(t, ref) {
    r <- ref[species]
    wl_ratio <- if (units == "energy") lines$wavelength / lines$wavelength[r] else 1
    measured / measured[r] * (lines$gk[r] * lines$Aki[r]) / (lines$gk * lines$Aki) * wl_ratio *
      exp((lines$Ek - lines$Ek[r]) / (k_boltzmann_ev * t))
  }

  if (is.null(temperature)) {
    parsed <- lapply(species, parse_species)
    element <- vapply(parsed, `[[`, character(1), "element")
    stage <- vapply(parsed, `[[`, integer(1), "charge") + 1L
    both <- intersect(element[stage == 1], element[stage == 2])
    if (length(both) == 0 || any(stage > 2)) {
      stop("Estimating the temperature needs lines of the neutral atom and of the singly ",
           "charged ion of at least one element (stages I and II only).", call. = FALSE)
    }
    e_ion <- cf_ionization_energy(ionization_energy, unique(element[stage == 2]), timeout)
    ion <- stage == 2
    x <- lines$Ek
    x[ion] <- x[ion] + unname(e_ion[element[ion]])
    group <- factor(element)
    # temperature of the Saha-Boltzmann fit of the lines corrected at t
    fitted_temperature <- function(t) {
      fit_lines <- lines
      fit_lines$intensity <- measured / sa_coefficients(t, choose_reference(t))
      y <- boltzmann_ordinate(fit_lines, units) - ion * log(saha_factor(t, electron_density))
      slope <- stats::coef(stats::lm(if (nlevels(group) > 1) y ~ 0 + group + x else y ~ x))[["x"]]
      if (!is.finite(slope) || slope >= 0) return(Inf)
      -1 / (k_boltzmann_ev * slope)
    }
    root <- tryCatch(
      stats::uniroot(function(t) fitted_temperature(t) - t, c(2000, 50000), tol = tol * 1e4),
      error = function(e) NULL
    )
    if (is.null(root)) {
      stop("No temperature between 2000 and 50000 K makes the corrected lines consistent; ",
           "check the lines, the reference lines and the electron density.", call. = FALSE)
    }
    temperature <- root$root
  }
  ref <- choose_reference(temperature)
  sa <- sa_coefficients(temperature, ref)
  if (any(sa > 1.05)) {
    warning(sum(sa > 1.05), " line(s) have SA > 1: check the reference lines, the atomic ",
            "data or the intensities.", call. = FALSE)
  }
  out <- tibble::as_tibble(lines)
  out$measured_intensity <- measured
  out$SA <- sa
  out$intensity <- measured / sa
  out$reference <- seq_len(nrow(lines)) %in% ref
  attr(out, "temperature") <- temperature
  out
}
