#' @title Lorentzian Function
#' @author Christian L. Goueguel
#' @description Lorentzian peak function with bell shape and much wider tails than Gaussian function.
#' @param x data
#' @param y0 baseline offset
#' @param xc center of the peak
#' @param w full width at half maximum (FWHM)
#' @param A area under the peak
#' @return fitted value
#' @export lorentzian_func
lorentzian_func <- function(x, y0, xc, w, A) {
  y0 + (2*A / pi) * (w / (4*(x - xc)^2 + w^2))
}
