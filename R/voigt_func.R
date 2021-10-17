#' @title Voigt Function
#' @author Christian L. Goueguel
#' @description Convolution of a FWHM-based Gaussian function and a Lorentzian function.
#' @param x data
#' @param y0 baseline offset
#' @param xc center of the peak
#' @param wG Gaussian FWHM
#' @param wL Lorentzian FWHM
#' @param A area under the peak
#' @return fitted value
#' @export voigt_func
voigt_func <- function(x, y0, xc, wG, wL, A) {
  y0 + A* RcppFaddeeva::Voigt(x = x, x0 = xc, sigma = wG, gamma = wL, real = TRUE)
}




