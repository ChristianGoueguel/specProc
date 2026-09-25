#' @title Multiple Peaks Fitting
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Fitting of multiple spectral lines by the same or different lineshape
#' functions with variable parameters.
#'
#' @details
#' The function uses `minpack.lm::nlsLM`, which is based on the Levenberg-Marquardt
#' algorithm for searching the minimum value of the square of the sum of the residuals.
#' All peaks are fitted simultaneously with a common baseline offset:
#' \deqn{y = y_0 + \sum_{i} A_i \cdot f_i(x; x_{c,i}, w_i)}
#' The parameters of peak \eqn{i} are named with the suffix `_i`
#' (e.g. `xc_1`, `wG_1`, `A_1`). Initial values that are not supplied are
#' estimated from the data around each peak center. Each peak center is
#' constrained to lie within the fitted wavelength range.
#'
#' @param x A data frame or tibble of spectra, one spectrum per row. Column
#'   names are the wavelengths; an optional identifier column can be given
#'   with `id`.
#' @param peaks A numeric vector of the (approximate) peak center wavelengths.
#' @param profiles A character vector of the lineshape functions for fitting,
#'   one per peak or a single one used for all peaks: "lorentzian", "gaussian"
#'   or "voigt" (case insensitive).
#' @param wL A numeric (single value or one per peak) of the Lorentzian full width at half maximum (initial guess)
#' @param wG A numeric (single value or one per peak) of the Gaussian full width at half maximum (initial guess)
#' @param A A numeric (single value or one per peak) of the peak area (initial guess)
#' @param wlgth.min A numeric of the lower bound of the wavelength subset
#' @param wlgth.max A numeric of the upper bound of the wavelength subset
#' @param id A character specifying the name of the column holding the spectra id (optional)
#' @param max.iter A numeric specifying the maximum number of iteration (200 by default)
#'
#' @return A tibble with one row per spectrum and the columns `id` (or
#'   `spectrum`), `data`, `fit`, `tidied` and `augmented` (see [peak_fit()]).
#'   `augmented` additionally contains one column per peak (`.peak_1`, ...)
#'   with the contribution of each fitted line.
#'
#' @export multipeak_fit
#'
#' @examples
#' wl <- seq(395, 397, by = 0.02)
#' set.seed(1)
#' spec <- 5 + gaussian_profile(wl, 0, 395.8, 0.15, 20) + lorentzian_profile(wl, 0, 396.3, 0.2, 30) +
#'   rnorm(length(wl), sd = 0.5)
#' df <- as.data.frame(t(spec))
#' names(df) <- wl
#' res <- multipeak_fit(df, peaks = c(395.8, 396.3), profiles = c("gaussian", "lorentzian"))
#' res$tidied[[1]]
#'
multipeak_fit <- function(
    x,
    peaks,
    profiles,
    wL = NULL,
    wG = NULL,
    A = NULL,
    wlgth.min = NULL,
    wlgth.max = NULL,
    id = NULL,
    max.iter = 200) {

  if (missing(x) || is.null(x)) {
    stop("Apparently you forgot to provide the spectra.")
  }
  if (!is.data.frame(x)) {
    stop("Data must be of class tbl_df, tbl or data.frame")
  }
  if (!is.numeric(peaks) || length(peaks) < 1 || anyNA(peaks)) {
    stop("Please enter a valid vector of wavelengths")
  }
  if (!is.character(profiles)) {
    stop("Profiles must be a valid vector of lineshape functions: Lorentzian, Gaussian and Voigt")
  }
  profiles <- tolower(profiles)
  if (!all(profiles %in% c("lorentzian", "gaussian", "voigt"))) {
    stop("Profiles must be a valid vector of lineshape functions: Lorentzian, Gaussian and Voigt")
  }
  if (length(profiles) == 1) {
    profiles <- rep(profiles, length(peaks))
  }
  if (length(peaks) != length(profiles)) {
    stop("Peaks and profiles must have the same length")
  }
  if (!is.numeric(max.iter)) {
    stop("Maximum number of iteration must be numeric")
  }

  spectra <- long_spectra(x, id, wlgth.min, wlgth.max)
  fit_spectra(spectra, peaks = peaks, profiles = profiles,
              wL = wL, wG = wG, A = A, max.iter = max.iter, single = FALSE)
}
