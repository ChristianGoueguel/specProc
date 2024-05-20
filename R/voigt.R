#' @title Voigt Lineshape Function
#' @author Christian L. Goueguel
#' @description Convolution of a full width at half maximum (FWHM) based Gaussian function and a Lorentzian function.
#' @param x data
#' @param y0 baseline offset
#' @param xc center of the peak
#' @param wG Gaussian FWHM
#' @param wL Lorentzian FWHM
#' @param A area under the peak
#' @param real logical, return only the real part of the complex Faddeeva
#' @return fitted value
#' @export voigt
voigt <- function(x, y0, xc, wG, wL, A, real = TRUE) {

  pVoigt <- function(.x, x0, sigma, gamma, .real = real){
    z <- (.x - x0 + gamma*1i) / (sigma * sqrt(2))
    w <- exp(-z^2)*pracma::erfc(-1i*z) # Faddeeva function
    if(.real) return (Re(w) / (sigma * sqrt(2*pi))) else w / (sigma * sqrt(2*pi))
  }

  s <- wG/(2*sqrt(2*log(2)))
  y0 + A* pVoigt(.x = x, x0 = xc, sigma = s, gamma = wL, .real = real)
}




