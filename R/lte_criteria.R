#' @title Criteria of Local Thermodynamic Equilibrium in Transient Plasmas
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Checks the local thermodynamic equilibrium (LTE) of a laser-induced
#' plasma by the McWhirter criterion and by the temporal and spatial
#' criteria of Cristoforetti et al. (2010) for transient and inhomogeneous
#' plasmas.
#'
#' @details
#' The McWhirter criterion (see [mcwhirter_criterion()]) gives the minimum
#' electron density for collisions to dominate the population of the
#' levels in a stationary, homogeneous plasma. It is necessary but not
#' sufficient in a laser-induced plasma, whose temperature and density
#' change in time and in space (Cristoforetti et al., 2010):
#'  - Temporal criterion: the plasma must change little during the time
#'    \eqn{\tau_{rel}} it takes to relax to equilibrium,
#'    \eqn{|T(t + \tau_{rel}) - T(t)| / T(t) \ll 1}, and likewise for
#'    \eqn{N_e}, with
#'    \deqn{\tau_{rel} \approx \frac{6.3 \times 10^4}{N_e f \langle g \rangle}
#'      \Delta E\, (k_B T)^{1/2} \exp\left(\frac{\Delta E}{k_B T}\right)\ \mathrm{s}}
#'  - Spatial criterion: the temperature and density must change little
#'    over the distance \eqn{\lambda} an atom diffuses during
#'    \eqn{\tau_{rel}},
#'    \deqn{\lambda \approx \frac{1.4 \times 10^{12}}{N_e} (k_B T)^{3/4}
#'      \left(\frac{\Delta E}{M f \langle g \rangle}\right)^{1/2}
#'      \exp\left(\frac{\Delta E}{2 k_B T}\right)\ \mathrm{cm}}
#'    which must be much shorter than the length over which they change,
#'    such as the size of the plasma.
#'
#' Here \eqn{N_e} is in cm\eqn{^{-3}}, \eqn{\Delta E} and \eqn{k_B T} in eV,
#' \eqn{f} is the absorption oscillator strength of the transition across
#' the gap \eqn{\Delta E}, \eqn{M} the atomic mass of the species (u) and
#' \eqn{\langle g \rangle} the effective Gaunt factor (Van Regemorter,
#' 1962). The slowest transition to equilibrate is usually the resonance
#' transition between the ground state and the first excited level of the
#' species, such as Ca II 393.37 nm (\eqn{\Delta E} = 3.15 eV, \eqn{f} =
#' 0.68); [nist_lines()] gives the oscillator strengths (`fik`). The
#' effective Gaunt factor depends on the transition and on \eqn{\Delta E /
#' k_B T}; the default, 0.2, errs toward long relaxation times. The criteria
#' are order-of-magnitude checks.
#'
#' The temporal criterion needs the temperature and density at several
#' times, `time` (such as acquisition delays): their relative changes
#' during \eqn{\tau_{rel}} are computed from their rates of change between
#' the measurements (finite differences of their logarithms). The spatial
#' criterion needs the length over which they change, `length_scale`. A
#' criterion is satisfied when the relative change, or the ratio of the
#' diffusion length to `length_scale`, is at most `tolerance`.
#'
#' @param temperature The plasma temperature, in K.
#' @param electron_density The electron density, in cm\eqn{^{-3}}.
#' @param delta_e The energy gap of the transition, in eV.
#' @param oscillator_strength The absorption oscillator strength of the
#'   transition.
#' @param mass The atomic mass of the species, in u, or its element symbol
#'   (such as `"Ca"`).
#' @param gaunt The effective Gaunt factor. Default is 0.2.
#' @param time Optional times of the measurements (one per temperature),
#'   in s, for the temporal criterion.
#' @param length_scale Optional length over which the temperature and
#'   density change, in mm, for the spatial criterion.
#' @param tolerance The largest relative change (or ratio of lengths) for a
#'   criterion to be satisfied. Default is 0.1.
#'
#' @return A tibble with one row per measurement (the arguments are recycled
#'   to a common length): the `temperature`, `electron_density` and
#'   `delta_e`; the McWhirter `minimum_density` and whether it is reached
#'   (`mcwhirter`); the `relaxation_time` (s) and `diffusion_length` (mm);
#'   with `time`, the `time`, the relative changes of the temperature and of
#'   the density during the relaxation time (`temperature_change`,
#'   `density_change`) and whether the `temporal` criterion is satisfied;
#'   with `length_scale`, the `length_ratio` of the diffusion length to it
#'   and whether the `spatial` criterion is satisfied.
#'
#' @references
#'  - Cristoforetti, G., De Giacomo, A., Dell'Aglio, M., Legnaioli, S.,
#'    Tognoni, E., Palleschi, V., Omenetto, N. (2010). Local thermodynamic
#'    equilibrium in laser-induced breakdown spectroscopy: beyond the
#'    McWhirter criterion. Spectrochimica Acta Part B, 65(1):86-95.
#'  - Van Regemorter, H. (1962). Rate of collisional excitation in stellar
#'    atmospheres. The Astrophysical Journal, 136:906-915.
#'
#' @seealso [mcwhirter_criterion()], [boltzmann()], [electron_density()]
#' @export lte_criteria
#'
#' @examples
#' # Ca II resonance transition (393.37 nm), in a plasma of 1 mm
#' lte_criteria(temperature = 9000, electron_density = 1e17, delta_e = 3.15,
#'              oscillator_strength = 0.68, mass = "Ca", length_scale = 1)
#'
#' # a plasma cooling from 1 to 5 microseconds
#' lte_criteria(temperature = c(11000, 9500, 8500, 7800),
#'              electron_density = c(3e17, 1.5e17, 8e16, 5e16), delta_e = 3.15,
#'              oscillator_strength = 0.68, mass = "Ca", time = c(1, 2, 3, 5) * 1e-6)
lte_criteria <- function(temperature, electron_density, delta_e, oscillator_strength, mass,
                         gaunt = 0.2, time = NULL, length_scale = NULL, tolerance = 0.1) {
  positive <- function(v, arg) {
    if (!is.numeric(v) || length(v) == 0 || anyNA(v) || any(v <= 0)) {
      stop("'", arg, "' must contain positive values.", call. = FALSE)
    }
  }
  positive(temperature, "temperature")
  positive(electron_density, "electron_density")
  positive(delta_e, "delta_e")
  positive(oscillator_strength, "oscillator_strength")
  positive(gaunt, "gaunt")
  mass <- atomic_mass(mass)
  check_number(tolerance, "tolerance", lower = 0, lower_open = TRUE)
  n <- max(length(temperature), length(electron_density), length(delta_e),
           length(oscillator_strength), length(mass), length(gaunt))
  t <- rep_len(temperature, n)
  ne <- rep_len(electron_density, n)
  de <- rep_len(delta_e, n)
  fg <- rep_len(oscillator_strength, n) * rep_len(gaunt, n)
  kt <- k_boltzmann_ev * t

  mcw <- mcwhirter_criterion(t, de, ne)
  relaxation <- 6.3e4 / (ne * fg) * de * sqrt(kt) * exp(de / kt)
  diffusion <- 1.4e12 / ne * kt^0.75 * sqrt(de / (rep_len(mass, n) * fg)) * exp(de / (2 * kt))
  out <- tibble::tibble(temperature = t, electron_density = ne, delta_e = de,
                        minimum_density = mcw$minimum_density, mcwhirter = mcw$satisfied,
                        relaxation_time = relaxation, diffusion_length = 10 * diffusion)

  if (!is.null(time)) {
    if (!is.numeric(time) || length(time) != n || n < 2 || anyNA(time) ||
        anyDuplicated(time)) {
      stop("'time' must give distinct times, one per measurement (at least 2).", call. = FALSE)
    }
    out$time <- time
    out$temperature_change <- abs(expm1(log_rate(time, t) * relaxation))
    out$density_change <- abs(expm1(log_rate(time, ne) * relaxation))
    out$temporal <- out$temperature_change <= tolerance & out$density_change <= tolerance
  }
  if (!is.null(length_scale)) {
    positive(length_scale, "length_scale")
    out$length_ratio <- out$diffusion_length / rep_len(length_scale, n)
    out$spatial <- out$length_ratio <= tolerance
  }
  out
}

# Rate of change of log(v) at each time, by finite differences (central
# between neighbors, one-sided at the ends).
log_rate <- function(time, v) {
  o <- order(time)
  t <- time[o]
  lv <- log(v[o])
  m <- length(t)
  rate <- numeric(m)
  rate[1] <- (lv[2] - lv[1]) / (t[2] - t[1])
  rate[m] <- (lv[m] - lv[m - 1]) / (t[m] - t[m - 1])
  if (m > 2) {
    inner <- 2:(m - 1)
    rate[inner] <- (lv[inner + 1] - lv[inner - 1]) / (t[inner + 1] - t[inner - 1])
  }
  rate[order(o)]
}
