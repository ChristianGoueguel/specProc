#' @title Spectra Normalization
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements normalization methods based on background,
#' total area, internal standard, and the L1, L2 and maximum norms of each
#' spectrum.
#'
#' @details
#' The three normalization methods:
#'    - **Normalization to the background:** Spectra are divided by the intensity
#'      of the background emission. Note that it is recommended that the detector
#'      dark current be subtracted prior to the normalization.
#'    - **Normalization to the total area:** Each spectrum is divided by the total
#'      area of the spectrum over the whole spectral range. The detector dark current
#'      must be subtracted prior to this normalization. The total area is calculated
#'      as the sum of all intensity levels.
#'    - **L1 normalization** (`"l1"`): each spectrum is divided by the sum of
#'      the absolute values of its intensities. It equals the total area for
#'      non-negative spectra, and also suits derivative spectra, whose values
#'      change sign.
#'    - **L2 (vector) normalization** (`"l2"`): each spectrum is divided by
#'      its Euclidean norm, so that its sum of squares is 1. Unlike SNV, the
#'      spectrum is not centered.
#'    - **Maximum normalization** (`"max"`): each spectrum is divided by its
#'      largest absolute intensity, so that its strongest feature is 1 (or
#'      -1).
#'    - **Normalization to an internal standard:** The peak intensity (or area) of the
#'      emission line related to the analyte is divided by the peak intensity (or area)
#'      of a selected emission line related to the internal standard. The internal
#'      standard concentration is assumed constant or known.
#'
#' @references
#'    - De Giacomo, A., Dell’Aglio, M., De Pascale, O., Gaudiuso, R.,
#'      Santagata, A., Teghil, R., (2008).
#'      Laser-induced breakdown spectroscopy methodology for the analysis of
#'      copper based alloys used in ancient artworks.
#'      Spectrochimica Acta Part B, 63(5):585-590
#'    - Body, D., Chadwick, B.L., (2001).
#'      Optimization of the spectral data processing in a LIBS simultaneous
#'      elemental analysis system.
#'      Spectrochimica Acta Part B, 56(6):725-736.
#'    - Rinnan, A., Van den Berg, F., Balling Engelsen, S., (2009).
#'      Review of the most common preprocessing techniques for near-infrared
#'      spectra, Trends in Analytical Chemistry, 28(10):1201-1222.
#'
#' @param x A numeric matrix or data frame containing the spectra.
#' @param method A character vector specifying the normalization method to apply. Available methods are: "area", "l1", "l2", "max", "background", and "internal". Spectra whose L1, L2 or maximum norm is zero are set to `NA`, with a warning.
#' @param bkg A numeric matrix or data frame of the same dimension as `x`, specifying the intensity of the continuum radiation (background emission) used for normalizing `x`. Required for "background" method.
#' @param wlength A character vector of the selected wavelength(s) (column names). Required for the "internal" method, where the intensities at these wavelengths are summed to give the internal standard. Optional for the "background" method, where only these wavelengths are normalized and returned.
#' @param drop.na A logical value indicating whether to remove spectra (rows) containing missing values before normalizing. Default is `TRUE`.
#'
#' @return A tibble of normalized spectra.
#'
#' @export normalize
#'
#' @examples
#' x <- data.frame(`400` = c(1, 2), `401` = c(3, 6), `402` = c(1, 2), check.names = FALSE)
#' normalize(x, method = "area")
#' normalize(x, method = "l2")
#' normalize(x, method = "internal", wlength = "401")
#'
normalize <- function(x, method = "area", bkg = NULL, wlength = NULL, drop.na = TRUE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  method <- match.arg(method, c("area", "l1", "l2", "max", "background", "internal"))
  x <- as.data.frame(as_numeric_matrix(x, "x"), check.names = FALSE)
  keep <- if (drop.na) stats::complete.cases(x) else rep(TRUE, nrow(x))

  norm_spectra <- switch(
    method,
    "area" = {
      x <- x[keep, , drop = FALSE]
      x / rowSums(x)
    },
    "l1" = ,
    "l2" = ,
    "max" = {
      x <- x[keep, , drop = FALSE]
      x / spectral_norm_factor(as.matrix(x), method)
    },
    "background" = {
      if (is.null(bkg)) {
        stop("'bkg' argument is missing for the 'background' method.")
      }
      if (!is.numeric(bkg) && !(is.data.frame(bkg) && all(vapply(bkg, is.numeric, logical(1))))) {
        stop("'bkg' must be a numeric data frame or matrix.")
      }
      bkg <- as.data.frame(as_numeric_matrix(bkg, "bkg"), check.names = FALSE)
      if (!identical(dim(x), dim(bkg))) {
        stop("Dimensions of 'x' and 'bkg' must be the same for the 'background' method.")
      }
      names(bkg) <- names(x)
      if (drop.na) {
        keep <- keep & stats::complete.cases(bkg)
      }
      x <- x[keep, , drop = FALSE]
      bkg <- bkg[keep, , drop = FALSE]
      if (!is.null(wlength)) {
        if (!is.character(wlength)) {
          stop("'wlength' must be a character vector.")
        }
        x <- x %>% dplyr::select(dplyr::all_of(wlength))
        bkg <- bkg %>% dplyr::select(dplyr::all_of(wlength))
      }
      x / bkg
    },
    "internal" = {
      if (is.null(wlength)) {
        stop("'wlength' argument is missing for the 'internal' method.")
      }
      if (!is.character(wlength)) {
        stop("'wlength' must be a character vector.")
      }
      x <- x[keep, , drop = FALSE]
      wlength_cols <- dplyr::select(x, dplyr::all_of(wlength))
      internal_std <- rowSums(wlength_cols)
      x / internal_std
    }
  )
  return(tibble::as_tibble(norm_spectra, .name_repair = "minimal"))
}

# Norm of each spectrum (row of x); NA, with a warning, when it is zero.
spectral_norm_factor <- function(x, method) {
  f <- switch(
    method,
    area = rowSums(x),
    l1 = rowSums(abs(x)),
    l2 = sqrt(rowSums(x^2)),
    max = apply(abs(x), 1, max)
  )
  bad <- !is.finite(f) | f == 0
  if (any(bad)) {
    warning(sum(bad), " spectra with a zero or missing ", method, " norm are set to NA.",
            call. = FALSE)
    f[bad] <- NA_real_
  }
  f
}
