#' @title Voigt Function
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes the Voigt function, which is the convolution of a Gaussian function
#' with a Lorentzian function. This function is commonly used in spectroscopy
#' to model the line shape of spectral peaks, which often exhibit a combined
#' Gaussian-Lorentzian profile due to various broadening effects.
#'
#' @details
#' The Voigt function is the convolution of a Gaussian function with a Lorentzian
#' function. It is based on the Faddeeva function, which is the complex convolution
#' of the two component functions. The Voigt function accurately describes the
#' line shape resulting from the combined effects of Doppler broadening
#' (modeled by the Gaussian component) and pressure or natural broadening
#' (modeled by the Lorentzian component).
#'
#' Mathematically, the Voigt function is given by:
#'
#' \deqn{V(x, x_c, \sigma, \gamma, A) = \frac{A}{\sigma \sqrt{2\pi}} \int_{-\infty}^{\infty} \exp\left(-\frac{(x-x_c)^2}{2\sigma^2}\right) \frac{\gamma_L}{\pi((x-x_c)^2 + \gamma^2)} dx}
#'
#' where:
#' \itemize{
#'   \item \eqn{A} is the peak area
#'   \item \eqn{\sigma} is the standard deviation of the Gaussian component
#'   \item \eqn{\gamma} is the half-width at half maximum (HWHM) of the Lorentzian component
#'   \item \eqn{x_c} is the center of the peak
#' }
#'
#' @param x A numeric vector representing the independent variable (e.g., wavelength or frequency).
#' @param y0 A numeric value specifying the baseline offset.
#' @param xc A numeric value representing the center of the peak.
#' @param wG A numeric value specifying the Gaussian full width at half maximum (FWHM).
#' @param wL A numeric value specifying the Lorentzian FWHM.
#' @param A A numeric value representing the peak area.
#' @param real A logical value indicating whether to return the real or
#' imaginary Voigt function. If `TRUE` (default) returns the real part.
#'
#' @return A numeric vector containing the values of the Voigt function evaluated
#' at the provided `x` values.
#'
#' @examples
#' # x <- seq(-2, 2, length.out = 100)
#' # y1 <- voigt(x, y0 = 0, xc = 0, wG = 1, wL = 0.5, A = 2)
#' # y2 <- voigt(x, y0 = 0, xc = 0, wG = 1, wL = 0.5, A = 2, real = FALSE)
#' # plot(x, y1, type = "l", col = "red", main = "Voigt Profile")
#' # lines(x, y2, col = "blue")
#' # legend("topright", legend = c("Real", "Imag"),
#' # col = c("red", "blue"), lty = 1)
#'
#' @export voigt
voigt <- function(x, y0, xc, wG, wL, A, real = TRUE) {
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
  if (!is.numeric(wL) || length(wL) != 1 || wL <= 0) {
    stop("'wL' must be a positive numeric value.")
  }
  if (!is.numeric(A) || length(A) != 1 || A <= 0) {
    stop("'A' must be a positive numeric value.")
  }
  if (!is.logical(real)) {
    stop("'real' must be a logical value (TRUE or FALSE).")
  }

  s <- wG / (2 * sqrt(2 * log(2)))
  # Issues in RcppFaddeeva not yet resolved
  #w <- RcppFaddeeva::Voigt(x, x0 = xc, sigma = s, gamma = wL, real = real)

  if (real) {
    result <- A * Re(w)
  } else {
    result <- A * Im(w)
  }
  return(result)
}




