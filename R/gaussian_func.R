#' @title Gaussian Function
#' @author Christian L. Goueguel
#' @description Full width at half maximum version of Gaussian Function.
#' @param x data
#' @param y0 offset
#' @param xc center
#' @param w full width at half maximum (FWHM)
#' @param A area
#' @return fitted value
#' @export gaussian_func
gaussian_func <- function(x, y0, xc, w, A) {
  y0 + (A / (w*sqrt(pi / 2))) * exp(-2*((x - xc)^2 / w^2))
}
