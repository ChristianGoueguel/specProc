#' @title Pseudo-Voigt Function
#'
#' @description
#' The pseudo-Voigt function is a linear combination of the Gaussian and Lorentzian functions,
#' weighted by a mixing parameter \eqn{\eta} (`eta`) that determines the relative
#' contribution of each component.
#'
#' @details
#' The Pseudo-Voigt function is given by:
#'
#' \deqn{f(x) = y_0 + A \cdot [ \eta \cdot L(x, x_c, w_L) + (1 - \eta) \cdot G(x, x_c, w_G)]}
#'
#' where:
#' \itemize{
#'   \item \eqn{A} is the peak area
#'   \item \eqn{y_0} is the baseline offset
#'   \item \eqn{G(x, x_0, w_G)} is the Gaussian component with center \eqn{x_c}
#'   and the full width at half maximum \eqn{w_G}
#'   \item \eqn{L(x, x_0, w_G)} is the Lorentzian component with center \eqn{x_c}
#'   and the full width at half maximum \eqn{w_L}
#'   \item \eqn{\eta} is the mixing parameter, with \eqn{0 < \eta < 1}. The
#'   mixing parameter \eqn{\eta} allows the pseudo-Voigt function to describe a wide
#'   range of line shapes, from pure Gaussian (\eqn{\eta = 0}) to pure Lorentzian (\eqn{\eta = 1}),
#'   and various intermediate shapes in between.
#' }
#'
#' @param x A numeric vector representing the independent variable (e.g., wavelength or frequency).
#' @param y0 A numeric value specifying the baseline offset.
#' @param xc A numeric value representing the center of the peak.
#' @param wG A numeric value specifying the Gaussian full width at half maximum (FWHM).
#' @param wL A numeric value specifying the Lorentzian FWHM.
#' @param A A numeric value representing the peak area.
#' @param eta A numeric value between 0 and 1, representing the mixing parameter.
#'
#' @return A numeric vector of the same length as `x`, containing the
#' computed pseudo-Voigt function values.
#'
#' @export pseudo_voigt
#' @examples
#' x <- seq(-2, 2, length.out = 100)
#' y1 <- pseudo_voigt(x, y0 = 0, xc = 0, wG = 1, wL = 0.5, A = 2, eta = 0)
#' y2 <- pseudo_voigt(x, y0 = 0, xc = 0, wG = 1, wL = 0.5, A = 2, eta = 0.5)
#' y3 <- pseudo_voigt(x, y0 = 0, xc = 0, wG = 1, wL = 0.5, A = 2, eta = 1)
#' plot(x, y1, type = "l", col = "red", main = "Pseudo-Voigt Profile", ylim = c(0, 2.5))
#' lines(x, y2, col = "blue")
#' lines(x, y3, col = "green")
#' legend("topright", legend = c("eta = 1", "eta = 0.5", "eta = 0"),
#' col = c("red", "blue", "green"), lty = 1)
#'
pseudo_voigt <- function(x, y0, xc, wG, wL, A, eta) {
  if (!is.numeric(x) || !is.vector(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (!is.numeric(xc) || length(xc) != 1) {
    stop("'x0' must be a single numeric value.")
  }
  if (!is.numeric(wG) || length(wG) != 1 || wG <= 0) {
    stop("'wG' must be a positive numeric value.")
  }
  if (!is.numeric(wL) || length(wL) != 1 || wL <= 0) {
    stop("'wL' must be a positive numeric value.")
  }
  if (!is.numeric(eta) || length(eta) != 1 || eta < 0 || eta > 1) {
    stop("'eta' must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(A) || length(A) != 1 || A <= 0) {
    stop("'A' must be a positive numeric value.")
  }
  if (!is.numeric(y0) || length(y0) != 1) {
    stop("'y0' must be a single numeric value.")
  }

  y <- y0 + A * ((1 - eta) * gaussian(x, y0, xc, wG, A = 1) + eta * lorentzian(x, y0, xc, wL, A = 1))

}
