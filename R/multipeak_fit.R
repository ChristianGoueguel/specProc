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
#' The lines of a multiplet, or of a fine-structure doublet, are related,
#' and constraints make the fit of overlapping lines much more stable:
#'  - `lock_centers`: the lines keep the separations of their `peaks`
#'    (accurate wavelengths, such as those of the NIST database) and move
#'    together by one fitted shift, which also absorbs an offset of the
#'    wavelength axis. The shift is listed in `tidied` as `shift` (or
#'    `shift_1_2`, ..., named by its lines, when there are several groups).
#'  - `share_widths`: the lines share their Gaussian and Lorentzian widths,
#'    as lines of the same multiplet have nearly the same Stark width.
#'  - `area_ratios`: the areas of the lines are in fixed ratios, with one
#'    fitted area. In an optically thin plasma in LTE, the ratio of two
#'    lines is \eqn{g_k A_{ki} \lambda^{-1} \exp(-E_k / k_B T)} (energy
#'    units), independent of the temperature for lines from the same upper
#'    level: the `relative_intensity` of [libs_lines()]. Self-absorbed lines,
#'    such as resonance lines, break these ratios.
#'
#' `lock_centers` and `share_widths` take `TRUE` (all lines), a logical
#' vector (the lines constrained together) or one group label per line
#' (`NA` for a line left free), so that, for example, a multiplet can be
#' constrained while an interfering line of another element is not.
#' `area_ratios` gives a ratio per line, `NA` for a line with a free area.
#'
#' `tidied` lists the center, widths and area of every line (named with the
#' suffix `_i`), with standard errors propagated from the fitted parameters:
#' lines that share a parameter have the same width, or centers that differ
#' by a constant.
#'
#' @param x A data frame or tibble of spectra, one spectrum per row. Column
#'   names are the wavelengths; an optional identifier column can be given
#'   with `id`.
#' @param peaks A numeric vector of the (approximate) peak center wavelengths.
#' @param profiles A character vector of the lineshape functions for fitting,
#'   one per peak or a single one used for all peaks: "lorentzian", "gaussian",
#'   "voigt" (exact) or "pseudo_voigt" (case insensitive).
#' @param wL A numeric (single value or one per peak) of the Lorentzian full width at half maximum (initial guess)
#' @param wG A numeric (single value or one per peak) of the Gaussian full width at half maximum (initial guess)
#' @param A A numeric (single value or one per peak) of the peak area (initial guess)
#' @param wlgth.min A numeric of the lower bound of the wavelength subset
#' @param wlgth.max A numeric of the upper bound of the wavelength subset
#' @param id A character specifying the name of the column holding the spectra id (optional)
#' @param max.iter A numeric specifying the maximum number of iteration (200 by default)
#' @param fixed The widths held at their given values (`wG`, `wL`, one
#'   value or one per peak) instead of being fitted: `"wG"`, `"wL"` or
#'   both, for the peaks whose profile has that width. Default is `NULL`
#'   (all widths fitted). See [peak_fit()] for holding the Gaussian width at
#'   the instrumental and Doppler width.
#' @param lock_centers Keep the separations of `peaks` (see details):
#'   `FALSE` (default), `TRUE`, a logical vector or group labels, one per
#'   peak.
#' @param share_widths Share the widths of the lines (see details): `FALSE`
#'   (default), `TRUE`, a logical vector or group labels, one per peak. The
#'   lines of a group must have the same profile.
#' @param area_ratios The fixed relative areas of the lines, one per peak
#'   (`NA` for a free area), or `NULL` (default) for free areas.
#'
#' @return A tibble with one row per spectrum and the columns `id` (or
#'   `spectrum`), `data`, `fit`, `tidied` and `augmented` (see [peak_fit()]).
#'   `augmented` additionally contains one column per peak (`.peak_1`, ...)
#'   with the contribution of each fitted line.
#'
#' @export multipeak_fit
#'
#' @examples
#' data(forageLIBS)
#' mean_spectrum <- colMeans(forageLIBS[-(1:14)])
#' wl <- as.numeric(names(mean_spectrum))
#' # the overlapping lines of the K I doublet
#' doublet <- tibble::as_tibble(as.list(mean_spectrum[wl > 404.1 & wl < 405.0]))
#' res <- multipeak_fit(doublet, peaks = c(404.414, 404.721), profiles = "gaussian")
#' res$tidied[[1]]
#'
#' # the same doublet with the NIST separation of its lines and a common width
#' res <- multipeak_fit(doublet, peaks = c(404.414, 404.721), profiles = "gaussian",
#'                      lock_centers = TRUE, share_widths = TRUE)
#' res$tidied[[1]]
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
    max.iter = 200,
    fixed = NULL,
    lock_centers = FALSE,
    share_widths = FALSE,
    area_ratios = NULL) {

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
  if (!all(profiles %in% lineshapes)) {
    stop("Profiles must be a valid vector of lineshape functions: Lorentzian, Gaussian, Voigt and pseudo_Voigt")
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

  centers <- line_groups(lock_centers, length(peaks), "lock_centers")
  widths <- line_groups(share_widths, length(peaks), "share_widths")
  for (g in unique(stats::na.omit(widths))) {
    if (length(unique(profiles[which(widths == g)])) > 1) {
      stop("The lines that share their widths must have the same profile.", call. = FALSE)
    }
  }
  if (!is.null(area_ratios)) {
    if (!is.numeric(area_ratios) || length(area_ratios) != length(peaks) ||
        all(is.na(area_ratios)) || any(area_ratios <= 0, na.rm = TRUE)) {
      stop("'area_ratios' must give one positive ratio per peak (NA for a free area).",
           call. = FALSE)
    }
  }

  spectra <- long_spectra(x, id, wlgth.min, wlgth.max)
  fit_spectra(spectra, peaks = peaks, profiles = profiles,
              wL = wL, wG = wG, A = A, max.iter = max.iter, single = FALSE, fixed = fixed,
              centers = centers, widths = widths, area_ratios = area_ratios)
}

# Group numbers of the lines from TRUE (all lines), FALSE (none), a logical
# vector (the lines of one group) or group labels (NA for a free line).
line_groups <- function(v, k, arg) {
  if (is.null(v) || isFALSE(v)) return(rep(NA_integer_, k))
  if (isTRUE(v)) return(rep(1L, k))
  if (length(v) != k || !(is.logical(v) || is.numeric(v) || is.character(v) || is.factor(v))) {
    stop("'", arg, "' must be TRUE, FALSE, or one value per peak (a logical, or group ",
         "labels with NA for a free peak).", call. = FALSE)
  }
  if (is.logical(v)) return(ifelse(!is.na(v) & v, 1L, NA_integer_))
  as.integer(factor(v))
}
