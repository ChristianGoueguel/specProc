#' @title Doppler Width of an Emission Line
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes the full width at half maximum of the Doppler (thermal)
#' broadening of a line emitted by atoms or ions of a given mass at a given
#' temperature.
#'
#' @details
#' The thermal motion of the emitters gives a Gaussian profile of full width
#' at half maximum
#' \deqn{w_D = \lambda \sqrt{\frac{8 k_B T \ln 2}{m c^2}}}
#' where \eqn{\lambda} is the wavelength, \eqn{T} the temperature and \eqn{m}
#' the mass of the emitter. In LIBS plasmas, it is usually much smaller than
#' the instrumental width, except for light elements such as hydrogen. Both
#' are Gaussian and add in quadrature, \eqn{w_G = \sqrt{w_{inst}^2 + w_D^2}},
#' the Gaussian width to hold fixed in a Voigt fit (see [peak_fit()]).
#'
#' @param wavelength The wavelength of the line, in nm.
#' @param temperature The temperature of the emitters, in K.
#' @param mass The mass of the emitter: its atomic mass, in u, or its element
#'   symbol (such as `"Ca"`), for the standard atomic weight.
#'
#' @return A numeric vector of Doppler widths (FWHM), in nm. The arguments
#'   are recycled to a common length.
#'
#' @seealso [peak_fit()], [voigt_fwhm()]
#' @export doppler_width
#'
#' @examples
#' # Ca II 393.37 nm and H-alpha at 10000 K
#' doppler_width(c(393.37, 656.28), temperature = 10000, mass = c("Ca", "H"))
#'
#' # with an instrumental width of 0.05 nm: the Gaussian width of a Voigt fit
#' sqrt(0.05^2 + doppler_width(393.37, 10000, "Ca")^2)
doppler_width <- function(wavelength, temperature, mass) {
  if (!is.numeric(wavelength) || anyNA(wavelength) || any(wavelength <= 0)) {
    stop("'wavelength' must contain positive values (nm).", call. = FALSE)
  }
  if (!is.numeric(temperature) || anyNA(temperature) || any(temperature <= 0)) {
    stop("'temperature' must contain positive values (K).", call. = FALSE)
  }
  if (is.character(mass)) {
    pt <- periodic_table()
    unknown <- setdiff(mass, pt$symbol)
    if (length(unknown) > 0) {
      stop("Unknown element(s): ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    mass <- pt$mass[match(mass, pt$symbol)]
  }
  if (!is.numeric(mass) || anyNA(mass) || any(mass <= 0)) {
    stop("'mass' must contain positive atomic masses (u) or element symbols.", call. = FALSE)
  }
  k_b <- 1.380649e-23        # J / K
  u <- 1.66053906660e-27     # kg
  c_light <- 299792458       # m / s
  wavelength * sqrt(8 * k_b * temperature * log(2) / (mass * u * c_light^2))
}
