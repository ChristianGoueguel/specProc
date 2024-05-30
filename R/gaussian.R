#' @title Gaussian Function
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes the values of a Gaussian function (also known as the normal distribution) given specific parameters.
#' The Gaussian profile is a bell-shaped curve that is symmetric around its center, making it distinct
#' from other profiles like the Lorentzian profile, which has heavier tails.
#'
#' @details
#' The Gaussian function is defined as:
#'
#' \deqn{f(x) = y_{0} + \frac{A}{w_{G} \sqrt{\pi / (4 \log(2))}} \exp \left( -\frac{4 \log(2) (x - x_{c})^2}{w_{G}^2} \right)}
#'
#' where:
#' \itemize{
#'   \item \eqn{A} is the peak area
#'   \item \eqn{x_c} is the center of the peak
#'   \item \eqn{w_G} is the full width at half maximum (FWHM)
#'   \item \eqn{y_0} is the baseline offset
#' }
#'
#' In spectroscopy, the Gaussian function is widely used to model the shape of spectral lines broadened
#' by Doppler broadening, which arises from the thermal motion of atoms or molecules emitting or absorbing
#' the radiation. However, in some cases, other broadening mechanisms, such as
#' pressure broadening or natural broadening, may contribute to the overall line shape, leading to
#' deviations from a pure Gaussian profile. In these situations, more complex line shape models,
#' such as the Voigt profile (a convolution of Gaussian and Lorentzian profiles), may be required.
#'
#' Additionally, instrumental broadening, caused by the finite resolution of the spectrometer or other
#' experimental apparatus, is another important factor that can contribute to the
#' observed spectral line shape, and it's often modeled using a Gaussian profile as well.
#'
#' @param x A numeric vector representing the independent variable (e.g., wavelength, frequency).
#' @param y0 A numeric value specifying the baseline offset.
#' @param xc A numeric value representing the center of the peak.
#' @param wG A numeric value specifying the full width at half maximum (FWHM).
#' @param A A numeric value representing the peak area.
#'
#' @return A numeric vector containing the values of the Gaussian function evaluated
#' at the provided `x` values.
#'
#' @examples
#' x <- seq(-10, 10, length.out = 100)
#' y <- gaussian(x, y0 = 0, xc = 0, wG = 2, A = 1)
#' plot(x, y, type = "l", col = "red", main = "Gaussian Profile")
#'
#' @export gaussian
gaussian <- function(x, y0, xc, wG, A) {
  if (!is.numeric(x) || !is.vector(x)) {
    stop("'x' must be a numeric vector.")
  }

  if (!is.numeric(y0) || length(y0) != 1) {
    stop("'y0' must be a single numeric value.")
  }

  if (!is.numeric(xc) || length(xc) != 1) {
    stop("'xc' must be a single numeric value.")
  }

  if (!is.numeric(wG) || length(wG) != 1 || wG <= 0) {
    stop("'wG' must be a positive numeric value.")
  }

  if (!is.numeric(A) || length(A) != 1 || A <= 0) {
    stop("'A' must be a positive numeric value.")
  }

  y0 + A / (wG * sqrt(pi / (4 * log(2)))) * exp((-4 * log(2) * (x - xc)^2) / (wG^2))
}
