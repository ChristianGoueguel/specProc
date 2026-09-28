#' @title Full Width at Half Maximum of a Voigt Profile
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes the full width at half maximum (FWHM) of a Voigt profile from
#' its Gaussian and Lorentzian widths, with the approximation of Olivero and
#' Longbothum (1977), accurate to about 0.02%:
#' \deqn{f_V \approx 0.5346\,f_L + \sqrt{0.2166\,f_L^2 + f_G^2}}
#'
#' @details
#' The total width of a fitted line is often better determined than its
#' split into Gaussian and Lorentzian parts, which trade off against each
#' other when a line is noisy or sampled by few channels. It is the width to
#' use for the H\eqn{\alpha} line in [electron_density()], whose Stark
#' profile is not Lorentzian.
#'
#' @param wG,wL The Gaussian and Lorentzian full widths at half maximum, for
#'   example the `wG` and `wL` parameters of a Voigt fit with [peak_fit()].
#'   Vectors are recycled.
#'
#' @return A numeric vector of Voigt FWHM, in the units of `wG` and `wL`.
#'
#' @references
#'  - Olivero, J.J., Longbothum, R.L. (1977). Empirical fits to the Voigt
#'    line width: a brief review. Journal of Quantitative Spectroscopy and
#'    Radiative Transfer, 17(2):233-236.
#'
#' @seealso [voigt_profile()], [peak_fit()], [electron_density()]
#' @export voigt_fwhm
#'
#' @examples
#' voigt_fwhm(wG = 0.1, wL = 0)    # Gaussian limit
#' voigt_fwhm(wG = 0, wL = 0.1)    # Lorentzian limit
#' voigt_fwhm(wG = 0.1, wL = 0.1)
voigt_fwhm <- function(wG, wL) {
  if (!is.numeric(wG) || !is.numeric(wL) || anyNA(wG) || anyNA(wL) || any(wG < 0) || any(wL < 0)) {
    stop("'wG' and 'wL' must be non-negative numbers.")
  }
  0.5346 * wL + sqrt(0.2166 * wL^2 + wG^2)
}
