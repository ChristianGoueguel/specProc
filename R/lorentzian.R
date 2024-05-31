#' @title Lorentzian Function
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes the Lorentzian function, which is a peaked function with a distinctive
#' bell-shaped curve and wider tails than the Gaussian function. The Lorentzian
#' function is commonly used to model spectral line shapes.
#'
#' @details
#' The Lorentzian function is defined as:
#'
#' \deqn{y = y_{0} + \frac{A}{\pi} \frac{\gamma}{(x - x_c)^2 + \gamma^2}}
#'
#' where:
#' \itemize{
#'   \item \eqn{A} is the peak area
#'   \item \eqn{x_c} is the center of the peak
#'   \item \eqn{\gamma = w_{L} / 2} is the half-width at half maximum (HWHM)
#'   \item \eqn{y_0} is the baseline offset
#' }
#'
#' The Lorentzian function arises naturally in various physical processes,
#' such as the decay of excited states in atoms or molecules, and is commonly
#' used to model phenomena that exhibit homogeneous broadening, where all atoms
#' or molecules experience the same broadening mechanism.
#'
#' @param x A numeric vector representing the independent variable (e.g., wavelength, frequency).
#' @param y0 A numeric value specifying the baseline offset.
#' @param xc A numeric value representing the center of the peak.
#' @param wL A numeric value specifying the full width at half maximum (FWHM).
#' @param A A numeric value representing the peak area.
#'
#' @return A numeric vector containing the values of the Lorentzian function
#' evaluated at the provided `x` values.
#'
#' @examples
#' x <- seq(-10, 10, length.out = 100)
#' y <- lorentzian(x, y0 = 0, xc = 0, wL = 2, A = 1)
#' plot(x, y, type = "l", col = "red", main = "Lorentzian Profile")
#'
#' @export lorentzian
lorentzian <- function(x, y0, xc, wL, A) {
  if (!is.numeric(x) || !is.vector(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (!is.numeric(y0) || length(y0) != 1) {
    stop("'y0' must be a single numeric value.")
  }
  if (!is.numeric(xc) || length(xc) != 1) {
    stop("'xc' must be a single numeric value.")
  }
  if (!is.numeric(wL) || length(wL) != 1 || wL <= 0) {
    stop("'wL' must be a positive numeric value.")
  }
  if (!is.numeric(A) || length(A) != 1 || A <= 0) {
    stop("'A' must be a positive numeric value.")
  }

  y0 + (2 * A / pi) * (wL / (4 * (x - xc)^2 + wL^2))

}
