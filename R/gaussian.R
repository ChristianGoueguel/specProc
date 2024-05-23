#' @title Gaussian Function
#' @author Christian L. Goueguel
#' @description Full width at half maximum (FWHM) version of Gaussian Function.
#' @param x data
#' @param y0 baseline offset
#' @param xc center of the peak
#' @param w FWHM
#' @param A area under the peak
#' @return fitted value
#' @export gaussian
gaussian <- function(x, y0, xc, w, A) {
  y0 + A/(w*sqrt(pi/(4*log(2)))) * exp((-4*log(2)*(x - xc)^2)/(w^2))
}
