#' @title Calibration-Free LIBS Quantification
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Estimates the elemental composition of a sample from the intensities of
#' its emission lines, without calibration standards, by the
#' calibration-free LIBS (CF-LIBS) method of Ciucci et al. (1999).
#'
#' @details
#' In a plasma in local thermodynamic equilibrium (LTE), optically thin and
#' of the same composition as the sample, the intensity of a line of
#' species \eqn{s} satisfies
#' \deqn{\ln\frac{I\lambda}{g_k A_{ki}} = -\frac{E_k}{k_B T} + \ln\frac{F C_s}{U_s(T)}}
#' where \eqn{C_s} is the concentration of the species, \eqn{U_s(T)} its
#' partition function and \eqn{F} an experimental factor common to all
#' lines. The Boltzmann plots of all species are therefore parallel lines:
#'  1. The temperature is estimated from their common slope, by a
#'     least-squares fit with one intercept per plot (unless `temperature`
#'     is given).
#'  2. The relative number density of each species follows from the
#'     intercept \eqn{q_s} of its plot, \eqn{F C_s = U_s(T) e^{q_s}}.
#'  3. The density of an ionization stage that is not observed is computed
#'     with the Saha equation, from the electron density (see
#'     [electron_density()]) and the ionization energy.
#'  4. The factor \eqn{F} is eliminated by closure (the concentrations of
#'     the elements sum to 1), or, with `reference`, by the known mass
#'     fraction of one element (internal reference), which is useful when
#'     some elements of the sample are not measured.
#'
#' Two methods place the lines on the plots:
#'  - `"boltzmann"` (Ciucci et al., 1999): one Boltzmann plot per species,
#'    with its own intercept. The temperature rests on the range of
#'    upper-level energies within each species, often narrow (1 to 2 eV),
#'    so it can be imprecise.
#'  - `"saha-boltzmann"` (the default when `electron_density` is given): one
#'    Saha-Boltzmann plot per element, on which the lines of the ion are
#'    placed with the Saha equation, as in [saha_boltzmann()]. The
#'    energy range is extended by the ionization energy, which gives a much
#'    more precise temperature, and the ionization balance of each element
#'    follows from the Saha equation. It needs the ionization energies of
#'    all elements.
#'
#' At least one plot needs two lines or more of different upper-level
#' energies.
#'
#' When the partition functions are computed from energy levels, the levels
#' of the neutral atoms above their ionization energy, lowered by the
#' Debye-Hückel correction when `electron_density` is given, are left out
#' (see [partition_function()]). This needs the ionization energies of all
#' elements observed as neutral atoms.
#'
#' The mass fractions are computed from the atomic fractions with the
#' standard atomic weights. The result is only as good as its assumptions:
#' all major elements must be measured (with closure), the lines must be
#' optically thin (see [self_absorption()]), the intensities corrected for
#' the spectral response of the instrument, and the atomic data accurate
#' (prefer lines of accuracy B or better in [nist_lines()]).
#'
#' @param lines A data frame with one row per line and columns `species`
#'   (such as `"Ca I"` or `"Ca II"`; stages I and II), `intensity`
#'   (integrated line intensity), `wavelength` (nm), `Aki` (s\eqn{^{-1}}),
#'   `gk` and `Ek` (eV), such as the lines of [nist_lines()] with measured
#'   intensities.
#' @param electron_density The electron density, in cm\eqn{^{-3}}. Needed
#'   for the elements observed in one ionization stage only; without it,
#'   only the observed stage of these elements is counted, with a warning.
#' @param temperature The plasma temperature, in K. By default, it is
#'   estimated from the lines.
#' @param method `"boltzmann"` or `"saha-boltzmann"` (see details). By
#'   default, `"saha-boltzmann"` when `electron_density` is given,
#'   `"boltzmann"` otherwise.
#' @param partition The partition functions: `NULL` (default) to compute
#'   them from the levels of the NIST Atomic Spectra Database (see
#'   [nist_levels()]; needs an internet connection), a data frame of levels
#'   with columns `species`, `g` and `energy` (eV), or a function of the
#'   species and the temperature returning the partition functions.
#' @param ionization_energy The ionization energies of the neutral atoms,
#'   in eV, named by element (`c(Ca = 6.113)`) or neutral species
#'   (`c("Ca I" = 6.113)`): for all elements with the Saha-Boltzmann
#'   method, and otherwise for the elements observed in one stage only and
#'   to truncate the partition functions computed from levels. By
#'   default, they are taken from the NIST database (see
#'   [nist_ionization_energy()]).
#' @param reference An optional known mass fraction of one element, named
#'   by the element, such as `c(Ca = 0.35)`, used instead of closure.
#' @param units The units of `intensity`: `"energy"` (default) or
#'   `"photons"`, as in [boltzmann()].
#' @param timeout The timeout of the NIST downloads, in seconds. Default is
#'   120.
#'
#' @return An object of class `specproc_cflibs`, a list with
#'  - `composition`: a tibble with, for each element, the `atomic_fraction`
#'    and `mass_fraction`, and the `stages` observed (a stage computed by
#'    the Saha equation is noted "Saha");
#'  - `species`: a tibble with, for each species, the number of lines, the
#'    `intercept` of its own Boltzmann plot, its `partition` function, its
#'    relative number `density` and whether it was `observed`;
#'  - `temperature`, `temperature_se` (K) and `electron_density`;
#'  - `points`: the points of the plots (for the Saha-Boltzmann method, the
#'    abscissa of ions includes the ionization energy and the ordinate the
#'    Saha correction), `intercept`, the intercept of each plot, and `fit`,
#'    the `lm` fit;
#'  - `method` and the number of `iterations` of the Saha-Boltzmann fit.
#'
#' Use [plot_boltzmann()] to draw the Boltzmann plots.
#'
#' @references
#'  - Ciucci, A., Corsi, M., Palleschi, V., Rastelli, S., Salvetti, A.,
#'    Tognoni, E. (1999). New procedure for quantitative elemental analysis
#'    by laser-induced plasma spectroscopy. Applied Spectroscopy,
#'    53(8):960-964.
#'  - Tognoni, E., Cristoforetti, G., Legnaioli, S., Palleschi, V. (2010).
#'    Calibration-free laser-induced breakdown spectroscopy: state of the
#'    art. Spectrochimica Acta Part B, 65(1):1-14.
#'
#' @seealso [saha_boltzmann()], [electron_density()], [nist_lines()],
#'   [nist_levels()], [plot_boltzmann()]
#' @export cf_libs
#'
#' @examples
#' data(forageLIBS)
#' mean_spectrum <- colMeans(forageLIBS[-(1:14)])
#' # calcium and magnesium lines, with their atomic data from the NIST database
#' atomic <- data.frame(
#'   species = c("Ca I", "Ca I", "Ca I", "Ca II", "Ca II", "Mg I", "Mg I", "Mg I", "Mg II", "Mg II"),
#'   wavelength = c(428.301, 430.253, 445.478, 315.887, 317.933,
#'                  516.732, 517.268, 518.360, 279.078, 279.800),
#'   Aki = c(4.34e7, 1.36e8, 8.70e7, 3.10e8, 3.60e8, 1.13e7, 3.37e7, 5.61e7, 4.01e8, 4.79e8),
#'   gk = c(5, 5, 7, 4, 6, 3, 3, 3, 4, 6),
#'   Ek = c(4.780, 4.780, 4.681, 7.047, 7.050, 5.108, 5.108, 5.108, 8.864, 8.864)
#' )
#' # partition functions, interpolated from a table (see partition_function())
#' partition <- function(species, temperature) {
#'   grid <- c(6000, 8000, 10000, 12000)
#'   table <- rbind(`Ca I` = c(1.407, 2.401, 4.499, 8.356), `Ca II` = c(2.389, 2.917, 3.560, 4.262),
#'                  `Mg I` = c(1.049, 1.207, 1.597, 2.463), `Mg II` = c(2.001, 2.010, 2.036, 2.087))
#'   vapply(species, function(s) exp(stats::approx(grid, log(table[s, ]), xout = temperature)$y),
#'          numeric(1))
#' }
#' fit <- cf_libs(line_intensities(mean_spectrum, atomic, baseline = TRUE),
#'                electron_density = 1.9e17, partition = partition,
#'                ionization_energy = c(Ca = 6.113, Mg = 7.646))
#' fit
#' plot_boltzmann(fit)
#'
cf_libs <- function(lines, electron_density = NULL, temperature = NULL, method = NULL,
                    partition = NULL, ionization_energy = NULL, reference = NULL,
                    units = "energy", timeout = 120) {
  units <- match.arg(units, c("energy", "photons"))
  if (is.null(method)) {
    method <- if (is.null(electron_density)) "boltzmann" else "saha-boltzmann"
  }
  method <- match.arg(method, c("boltzmann", "saha-boltzmann"))
  if (method == "saha-boltzmann" && is.null(electron_density)) {
    stop("The Saha-Boltzmann method needs `electron_density`.", call. = FALSE)
  }
  lines <- check_lines(lines, c("species", "intensity", "wavelength", "Aki", "gk", "Ek"))
  parsed <- lapply(as.character(lines$species), parse_species)
  lines$species <- vapply(parsed, `[[`, character(1), "label")
  element <- vapply(parsed, `[[`, character(1), "element")
  stage <- vapply(parsed, `[[`, integer(1), "charge") + 1L
  if (any(stage > 2)) {
    stop("`cf_libs()` handles neutral atoms and singly charged ions (stages I and II) only.",
         call. = FALSE)
  }
  if (!is.null(electron_density)) {
    check_number(electron_density, "electron_density", lower = 0, lower_open = TRUE)
  }
  if (!is.null(temperature)) {
    check_number(temperature, "temperature", lower = 0, lower_open = TRUE)
  }
  check_reference(reference, element)

  elements <- unique(element)
  saha_plane <- method == "saha-boltzmann"
  if (saha_plane) {
    e_ion <- cf_ionization_energy(ionization_energy, elements, timeout)
  }

  # 1. temperature and intercepts: parallel Boltzmann plots (one per species)
  # or Saha-Boltzmann plots (one per element, ions placed with the Saha equation)
  ion <- stage == 2
  group <- if (saha_plane) element else lines$species
  points <- tibble::tibble(species = factor(lines$species, levels = unique(lines$species)),
                           element = element, stage = stage,
                           group = factor(group, levels = unique(group)),
                           x = lines$Ek + if (saha_plane) ion * unname(e_ion[element]) else 0,
                           y = boltzmann_ordinate(lines, units))
  y0 <- points$y
  saha_shift <- function(t) if (saha_plane) ion * log(saha_factor(t, electron_density)) else 0
  fit <- NULL
  temperature_se <- NA_real_
  iterations <- 0L
  if (is.null(temperature)) {
    informative <- tapply(points$x, points$group, function(v) length(unique(v)) > 1)
    if (!any(informative)) {
      stop("At least one ", if (saha_plane) "element" else "species",
           " needs two lines of different upper-level energies ",
           "to estimate the temperature; otherwise give `temperature`.", call. = FALSE)
    }
    formula <- if (nlevels(points$group) > 1) y ~ 0 + group + x else y ~ x
    temperature <- 1e4
    repeat {
      iterations <- iterations + 1L
      points$y <- y0 - saha_shift(temperature)
      fit <- stats::lm(formula, data = points)
      slope <- stats::coef(fit)[["x"]]
      if (!is.finite(slope) || slope >= 0) {
        stop("The Boltzmann plots have a non-negative common slope; check the line data.",
             call. = FALSE)
      }
      new_temperature <- -1 / (k_boltzmann_ev * slope)
      converged <- !saha_plane || abs(new_temperature - temperature) <= 1e-8 * new_temperature
      temperature <- new_temperature
      if (converged || iterations >= 100) break
    }
    if (!converged) {
      warning("The temperature did not converge in 100 iterations.", call. = FALSE)
    }
    points$y <- y0 - saha_shift(temperature)
    fit <- stats::lm(formula, data = points)
    est <- suppressWarnings(summary(fit))$coefficients
    if (stats::df.residual(fit) > 0) {
      temperature_se <- est["x", "Std. Error"] / (k_boltzmann_ev * est["x", "Estimate"]^2)
    }
  } else {
    points$y <- y0 - saha_shift(temperature)
  }
  kt <- k_boltzmann_ev * temperature
  intercept <- tapply(points$y + points$x / kt, points$group, mean)
  intercept <- stats::setNames(as.vector(intercept), names(intercept))

  # 2. species densities, with the Saha equation for the stages not observed
  observed <- unique(data.frame(species = lines$species, element = element, stage = stage,
                                stringsAsFactors = FALSE))
  if (saha_plane) {
    all_species <- as.vector(t(outer(elements, c("I", "II"), paste)))
    partitions <- cf_partition(partition, all_species, temperature, timeout,
                               cf_max_energy(partition, elements, e_ion, temperature, electron_density))
    density <- numeric()
    species_intercept <- numeric()
    for (el in elements) {
      neutral <- paste(el, "I")
      ionized <- paste(el, "II")
      log_saha <- log(saha_factor(temperature, electron_density)) - e_ion[[el]] / kt
      species_intercept[[neutral]] <- intercept[[el]]
      species_intercept[[ionized]] <- intercept[[el]] + log_saha
      density[[neutral]] <- partitions[[neutral]] * exp(intercept[[el]])
      density[[ionized]] <- partitions[[ionized]] * exp(species_intercept[[ionized]])
    }
  } else {
    one_stage <- names(which(table(observed$element) == 1))
    saha_elements <- if (is.null(electron_density)) character() else one_stage
    if (is.null(electron_density) && length(one_stage) > 0) {
      warning("No `electron_density`: the other ionization stage of ",
              paste(one_stage, collapse = ", "), " is neglected.", call. = FALSE)
    }
    missing_species <- unlist(lapply(saha_elements, function(el) {
      paste(el, c("I", "II")[3L - observed$stage[observed$element == el]])
    }))
    all_species <- c(observed$species, missing_species)
    neutral_elements <- unique(sub(" I$", "", all_species[endsWith(all_species, " I")]))
    e_ion <- if (!is.function(partition) && length(neutral_elements) > 0) {
      cf_ionization_energy(ionization_energy, neutral_elements, timeout)
    }
    partitions <- cf_partition(partition, all_species, temperature, timeout,
                               cf_max_energy(partition, neutral_elements, e_ion, temperature,
                                             electron_density))
    species_intercept <- intercept[observed$species]
    density <- stats::setNames(partitions[observed$species] * exp(species_intercept),
                               observed$species)
    if (length(saha_elements) > 0) {
      e_ion <- cf_ionization_energy(ionization_energy, saha_elements, timeout)
      for (el in saha_elements) {
        ratio <- saha_factor(temperature, electron_density) *
          partitions[[paste(el, "II")]] / partitions[[paste(el, "I")]] * exp(-e_ion[[el]] / kt)
        have <- observed$species[observed$element == el]
        other <- setdiff(paste(el, c("I", "II")), have)
        density[[other]] <- if (endsWith(have, " I")) density[[have]] * ratio else density[[have]] / ratio
      }
    }
  }
  species_names <- names(density)
  species_el <- sub(" .*", "", species_names)
  species_tbl <- tibble::tibble(
    species = species_names, element = species_el,
    stage = ifelse(endsWith(species_names, " II"), 2L, 1L),
    n_lines = as.integer(table(factor(lines$species, levels = species_names))),
    intercept = unname(species_intercept[species_names]),
    partition = unname(partitions[species_names]),
    density = unname(density), observed = species_names %in% observed$species
  )

  # 3. composition
  n_element <- vapply(elements, function(el) sum(density[species_el == el]), numeric(1))
  pt <- periodic_table()
  mass <- pt$mass[match(elements, pt$symbol)]
  atomic <- n_element / sum(n_element)
  mass_fraction <- atomic * mass / sum(atomic * mass)
  if (!is.null(reference)) {
    mass_fraction <- mass_fraction * reference[[1]] / mass_fraction[[names(reference)]]
  }
  stages <- vapply(elements, function(el) {
    st <- species_tbl[species_tbl$element == el, ]
    st <- st[order(st$stage), ]
    paste(ifelse(st$observed, c("I", "II")[st$stage], paste(c("I", "II")[st$stage], "(Saha)")),
          collapse = ", ")
  }, character(1))
  composition <- tibble::tibble(element = elements, atomic_fraction = unname(atomic),
                                mass_fraction = unname(mass_fraction), stages = unname(stages))

  structure(
    list(composition = composition, species = species_tbl, temperature = temperature,
         temperature_se = temperature_se, electron_density = electron_density,
         points = points, fit = fit, intercept = intercept, units = units,
         reference = reference, method = method, iterations = iterations),
    class = "specproc_cflibs"
  )
}

#' @export
print.specproc_cflibs <- function(x, ...) {
  cat("Calibration-free LIBS (", nrow(x$points), " lines, ", nlevels(x$points$species),
      " species; ", if (x$method == "boltzmann") "Boltzmann" else "Saha-Boltzmann",
      " plots)\n\n", sep = "")
  cat("Temperature:  ", format(round(x$temperature)),
      if (is.finite(x$temperature_se)) paste0(" +/- ", format(round(x$temperature_se))),
      " K", if (is.null(x$fit)) " (given)", "\n", sep = "")
  if (!is.null(x$electron_density)) {
    cat("Ne:           ", format(x$electron_density, digits = 3), " cm-3\n", sep = "")
  }
  cat("Normalization:", if (is.null(x$reference)) " closure" else
    paste0(" ", names(x$reference), " = ", format(x$reference[[1]]), " (mass fraction)"), "\n\n",
    sep = "")
  comp <- as.data.frame(x$composition)
  comp$atomic_fraction <- sprintf("%.4f", comp$atomic_fraction)
  comp$mass_fraction <- sprintf("%.4f", comp$mass_fraction)
  print(comp, row.names = FALSE, right = FALSE)
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

check_reference <- function(reference, element) {
  if (is.null(reference)) return(invisible())
  if (!is.numeric(reference) || length(reference) != 1 || is.null(names(reference)) ||
      !is.finite(reference) || reference <= 0 || reference > 1) {
    stop("'reference' must be one mass fraction between 0 and 1, named by its element, ",
         "such as c(Ca = 0.35).", call. = FALSE)
  }
  if (!names(reference) %in% element) {
    stop("The reference element ", names(reference), " has no line in 'lines'.", call. = FALSE)
  }
  invisible()
}

# Lowering of the ionization energy of a neutral atom (eV), Debye-Hueckel.
ionization_lowering <- function(temperature, electron_density) {
  if (is.null(electron_density)) return(0)
  e <- 1.602176634e-19
  eps0 <- 8.8541878128e-12
  debye <- sqrt(eps0 * 1.380649e-23 * temperature / (electron_density * 1e6 * e^2))
  e / (4 * pi * eps0 * debye)
}

# Highest level energy of the neutral atoms in their partition function.
cf_max_energy <- function(partition, elements, e_ion, temperature, electron_density) {
  if (is.function(partition) || length(elements) == 0) return(Inf)
  stats::setNames(e_ion[elements] - ionization_lowering(temperature, electron_density),
                  paste(elements, "I"))
}

# Partition functions of the species at the temperature, as a named vector.
cf_partition <- function(partition, species, temperature, timeout, max_energy = Inf) {
  values <- if (is.null(partition)) {
    pf <- partition_function(nist_levels(species, timeout = timeout), temperature, max_energy)
    stats::setNames(pf$partition, pf$species)[species]
  } else if (is.data.frame(partition)) {
    absent <- setdiff(species, partition$species)
    if (length(absent) > 0) {
      stop("'partition' has no levels for ", paste(absent, collapse = ", "), ".", call. = FALSE)
    }
    pf <- partition_function(partition[partition$species %in% species, , drop = FALSE],
                             temperature, max_energy)
    stats::setNames(pf$partition, pf$species)[species]
  } else if (is.function(partition)) {
    as.numeric(partition(species, temperature))
  } else {
    stop("'partition' must be NULL, a data frame of levels or a function.", call. = FALSE)
  }
  values <- as.numeric(values)
  if (length(values) != length(species) || anyNA(values) || any(values <= 0)) {
    stop("The partition functions must be positive numbers, one per species (",
         paste(species, collapse = ", "), ").", call. = FALSE)
  }
  stats::setNames(values, species)
}

# Ionization energies of the neutral atoms of the elements, as a named vector.
cf_ionization_energy <- function(ionization_energy, elements, timeout) {
  if (is.null(ionization_energy)) {
    values <- nist_ionization_energy(paste(elements, "I"), timeout = timeout)
    return(stats::setNames(unname(values), elements))
  }
  if (!is.numeric(ionization_energy) || is.null(names(ionization_energy))) {
    stop("'ionization_energy' must be a numeric vector named by element, such as c(Ca = 6.113).",
         call. = FALSE)
  }
  names(ionization_energy) <- sub(" I$", "", trimws(names(ionization_energy)))
  absent <- setdiff(elements, names(ionization_energy))
  if (length(absent) > 0) {
    stop("'ionization_energy' has no value for ", paste(absent, collapse = ", "), ".", call. = FALSE)
  }
  ionization_energy[elements]
}
