#' @title Voigt Function
#' @author Christian L. Goueguel
#' @description Convolution of a FWHM-based Gaussian function and a Lorentzian function.
#' @param x data
#' @param y0 offset
#' @param xc center
#' @param wG Gaussian full width at half maximum (FWHM)
#' @param wL Lorentzian full width at half maximum (FWHM)
#' @param A area
#' @return fitted value
#' @export voigt_func
voigt_func <- function(x, y0, xc, wG, wL, A) {
  y0 + A* RcppFaddeeva::Voigt(x = x, x0 = xc, sigma = wG, gamma = wL, real = TRUE)
}




