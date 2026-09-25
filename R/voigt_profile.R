#' @title Voigt Function
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes the exact Voigt profile, the convolution of a Gaussian and a
#' Lorentzian profile. In laser-induced plasmas, the Gaussian component
#' typically reflects Doppler and instrumental broadening, and the Lorentzian
#' component Stark (pressure) and natural broadening.
#'
#' @details
#' The Voigt function is defined as:
#'
#' \deqn{y = y_0 + A \int_{-\infty}^{\infty} G(t; w_G)\, L(x - x_c - t; w_L)\, dt
#' = y_0 + A \frac{\mathrm{Re}[w(z)]}{\sigma\sqrt{2\pi}}, \quad
#' z = \frac{x - x_c + i\gamma}{\sigma\sqrt{2}}}
#'
#' where \eqn{w(z) = e^{-z^2}\mathrm{erfc}(-iz)} is the Faddeeva function,
#' \eqn{\sigma = w_G / (2\sqrt{2\ln 2})} is the standard deviation of the
#' Gaussian component and \eqn{\gamma = w_L / 2} is the half width at half
#' maximum of the Lorentzian component. Both components have unit area, so
#' \eqn{A} is the area of the line.
#'
#' The Faddeeva function is evaluated in C++ with Weideman's (1994) rational
#' approximation using 32 terms. Its relative error is below \eqn{10^{-10}}
#' over the range of widths met in emission spectroscopy. A width of zero gives
#' the pure Gaussian or pure Lorentzian profile. See [pseudo_voigt_profile()]
#' for the faster pseudo-Voigt approximation.
#'
#' @references
#'  - Weideman, J.A.C., (1994). Computation of the complex error function.
#'    SIAM Journal on Numerical Analysis, 31(5):1497-1518.
#'  - Armstrong, B.H., (1967). Spectrum line profiles: the Voigt function.
#'    J. Quant. Spectrosc. Radiat. Transfer, 7(1):61-88.
#'
#' @param x A numeric vector representing the independent variable (e.g., wavelength).
#' @param y0 A numeric value specifying the baseline offset.
#' @param xc A numeric value representing the center of the peak.
#' @param wG A non-negative numeric value specifying the Gaussian full width at half maximum (FWHM).
#' @param wL A non-negative numeric value specifying the Lorentzian FWHM.
#' @param A A numeric value representing the peak area.
#'
#' @return A numeric vector containing the values of the Voigt function
#' evaluated at the provided `x` values.
#'
#' @seealso [pseudo_voigt_profile()], [gaussian_profile()], [lorentzian_profile()], [peak_fit()]
#'
#' @export voigt_profile
#'
#' @examples
#' x <- seq(-3, 3, length.out = 200)
#' v <- voigt_profile(x, y0 = 0, xc = 0, wG = 1, wL = 0.5, A = 1)
#' pv <- pseudo_voigt_profile(x, y0 = 0, xc = 0, wG = 1, wL = 0.5, A = 1)$y
#' plot(x, v, type = "l", main = "Voigt vs pseudo-Voigt")
#' lines(x, pv, col = "red", lty = 2)
#' max(abs(v - pv)) / max(v) # pseudo-Voigt relative error
#'
voigt_profile <- function(x, y0, xc, wG, wL, A) {
  if (!is.numeric(x) || !is.vector(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (!is.numeric(y0) || length(y0) != 1) {
    stop("'y0' must be a single numeric value.")
  }
  if (!is.numeric(xc) || length(xc) != 1) {
    stop("'xc' must be a single numeric value.")
  }
  if (!is.numeric(wG) || length(wG) != 1 || is.na(wG) || wG < 0) {
    stop("'wG' must be a non-negative numeric value.")
  }
  if (!is.numeric(wL) || length(wL) != 1 || is.na(wL) || wL < 0) {
    stop("'wL' must be a non-negative numeric value.")
  }
  if (wG == 0 && wL == 0) {
    stop("'wG' and 'wL' cannot both be zero.")
  }
  if (!is.numeric(A) || length(A) != 1 || A < 0) {
    stop("'A' must be a non-negative numeric value.")
  }
  y0 + A * profile_voigt(x, xc, wG, wL)
}
