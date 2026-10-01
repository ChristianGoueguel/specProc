#' @title Extended Multiplicative Signal Correction
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements extended multiplicative signal correction (EMSC),
#' as proposed by Martens and Stark (1991). EMSC extends [msc()] by modeling,
#' in addition to the multiplicative and additive effects, a smooth polynomial
#' baseline and, optionally, known interferent spectra.
#'
#' @details
#' Each spectrum \eqn{\textbf{x}_i} is modeled as a linear combination of a
#' reference spectrum \eqn{\textbf{m}}, polynomials of the wavelength
#' \eqn{\lambda} and interferent spectra \eqn{\textbf{k}_j}:
#' \deqn{\textbf{x}_i = b_i\textbf{m} + a_i + d_{i1}\lambda + d_{i2}\lambda^2 + \dots + \sum_j h_{ij}\textbf{k}_j + \textbf{e}_i}
#' The coefficients are estimated by least squares, and the corrected spectrum
#' is
#' \deqn{\textbf{x}_i^{corr} = (\textbf{x}_i - a_i - d_{i1}\lambda - \dots - \sum_j h_{ij}\textbf{k}_j) / b_i}
#' With `degree = 0` and no interferents, EMSC is identical to [msc()] with
#' `drop.offset = TRUE`.
#'
#' The wavelengths are rescaled to \eqn{[-1, 1]} before the polynomials are
#' formed, for numerical stability. Interferent spectra describe variation
#' that should be removed, for example the spectrum of a contaminant or the
#' dominant directions of repeated measurements of the same samples (see
#' [epo()]).
#'
#' The reference spectrum is estimated from `x` unless `xref` is given, so
#' new spectra should be corrected with [predict()][predict.specproc_emsc],
#' which uses the reference and design of the calibration data.
#'
#' @param x A numeric matrix or data frame, with one spectrum per row.
#' @param xref An optional numeric vector giving the reference spectrum. If
#'   `NULL` (default), the median (`robust = TRUE`) or mean spectrum of `x`
#'   is used.
#' @param degree A non-negative integer giving the degree of the polynomial
#'   baseline. Default is 2. With 0, only a constant offset is modeled.
#' @param interferents An optional numeric vector (a single spectrum), matrix
#'   or data frame of interferent spectra, one per row, with the same number
#'   of columns as `x`.
#' @param wavelength An optional numeric vector of wavelengths used to build
#'   the polynomials. If `NULL` (default), the column names of `x` are used
#'   when they are numeric, and the column indices otherwise.
#' @param robust A logical value indicating whether the median (`TRUE`,
#'   default) or the mean spectrum is used as the reference.
#'
#' @return An object of class `specproc_emsc` (a list), which
#'   [predict()][predict.specproc_emsc] applies to new spectra, with the
#'   following components:
#'  - `correction`: The corrected spectra.
#'  - `coefficients`: A tibble with the estimated coefficients of each
#'    spectrum: `slope` (\eqn{b_i}), `offset` (\eqn{a_i}), `poly1`, ...
#'    (\eqn{d_{ik}}) and `interferent1`, ... (\eqn{h_{ij}}).
#'  - `reference`: The reference spectrum.
#'  - `design`: The design matrix (one column per model term).
#'
#' @references
#'  - Martens, H., Stark, E. (1991).
#'    Extended multiplicative signal correction and spectral interference
#'    subtraction: new preprocessing methods for near infrared spectroscopy.
#'    Journal of Pharmaceutical and Biomedical Analysis, 9(8):625-635.
#'  - Afseth, N.K., Kohler, A. (2012).
#'    Extended multiplicative signal correction in vibrational spectroscopy,
#'    a tutorial. Chemometrics and Intelligent Laboratory Systems,
#'    117:92-99.
#'
#' @seealso [msc()], [step_emsc()] to use EMSC in a tidymodels recipe.
#'
#' @export emsc
#'
#' @examples
#' data(forageLIBS)
#' spectra <- forageLIBS[-(1:14)]  # the spectral channels
#' fit <- emsc(spectra[1:300, ], degree = 2)
#' head(fit$coefficients)
#' # new spectra are corrected with the calibration reference
#' corrected <- predict(fit, spectra[301:368, ])
#' dim(corrected)
emsc <- function(x, xref = NULL, degree = 2, interferents = NULL, wavelength = NULL, robust = TRUE) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  x <- as_numeric_matrix(x, "x")
  if (anyNA(x)) {
    stop("'x' contains missing values; remove or impute them first.")
  }
  check_count(degree, "degree", lower = 0)
  check_flag(robust, "robust")
  p <- ncol(x)
  if (!is.null(xref) && (!is.numeric(xref) || length(xref) != p || anyNA(xref))) {
    stop("'xref' must be a numeric vector with one value per column of 'x'.")
  }
  if (is.null(xref)) {
    xref <- if (robust) col_medians(x) else colMeans(x)
  }
  xref <- as.numeric(xref)

  if (is.null(wavelength)) {
    wavelength <- parse_wavelength(colnames(x) %||% character(0))
    if (length(wavelength) != p || anyNA(wavelength)) {
      wavelength <- seq_len(p)
    }
  } else if (!is.numeric(wavelength) || length(wavelength) != p || anyNA(wavelength)) {
    stop("'wavelength' must be a numeric vector with one value per column of 'x'.")
  }

  if (!is.null(interferents)) {
    if (is.null(dim(interferents))) {
      interferents <- matrix(interferents, nrow = 1)  # a single spectrum
    }
    interferents <- as_numeric_matrix(interferents, "interferents")
    if (ncol(interferents) != p) {
      stop("'interferents' must have the same number of columns as 'x'.")
    }
    if (anyNA(interferents)) {
      stop("'interferents' cannot contain missing values.")
    }
  }

  design <- emsc_design(xref, wavelength, degree, interferents)
  if (qr(design)$rank < ncol(design)) {
    stop("The EMSC model is rank deficient: lower 'degree' or remove interferents ",
         "that are linear combinations of the other terms.")
  }
  if (nrow(design) <= ncol(design)) {
    stop("EMSC needs more spectral points than model terms.")
  }

  model <- new_filter(list(reference = xref, design = design), "specproc_emsc", x)
  fit <- emsc_apply(model, x)
  res <- list(
    correction = fit$correction,
    coefficients = fit$coefficients,
    reference = xref,
    design = design
  )
  new_filter(res, "specproc_emsc", x)
}

#' @export
print.specproc_emsc <- function(x, ...) {
  terms <- colnames(x$design)
  cat("Extended multiplicative signal correction (EMSC)\n\n")
  cat("Variables:            ", attr(x, "nvar"), "\n", sep = "")
  cat("Polynomial degree:    ", sum(startsWith(terms, "poly")), "\n", sep = "")
  cat("Interferents:         ", sum(startsWith(terms, "interferent")), "\n", sep = "")
  if (!is.null(x$correction)) {
    cat("Calibration spectra:  ", nrow(x$correction), "\n", sep = "")
  }
  cat("\nUse predict(<emsc>, newdata) to correct new spectra.\n")
  invisible(x)
}

# Design matrix: reference, constant, polynomials of the rescaled wavelength,
# interferents.
emsc_design <- function(xref, wavelength, degree, interferents) {
  rng <- range(wavelength)
  w <- if (diff(rng) > 0) 2 * (wavelength - rng[1]) / diff(rng) - 1 else wavelength * 0
  poly <- if (degree > 0) outer(w, seq_len(degree), `^`) else NULL
  if (!is.null(poly)) colnames(poly) <- paste0("poly", seq_len(degree))
  inter <- if (is.null(interferents)) NULL else t(interferents)
  if (!is.null(inter)) colnames(inter) <- paste0("interferent", seq_len(ncol(inter)))
  cbind(slope = xref, offset = 1, poly, inter)
}

# Least-squares EMSC coefficients of each spectrum, and the corrected spectra.
emsc_apply <- function(object, x) {
  design <- object$design
  coefs <- qr.coef(qr(design), t(x))                  # terms x spectra
  slope <- coefs["slope", ]
  if (any(abs(slope) < .Machine$double.eps)) {
    stop("Some spectra have a zero multiplicative coefficient and cannot be corrected.")
  }
  nuisance <- design[, -1, drop = FALSE] %*% coefs[-1, , drop = FALSE]  # p x n
  corrected <- (x - t(nuisance)) / slope
  list(
    correction = as_tbl(corrected, attr(object, "variables")),
    coefficients = tibble::as_tibble(t(coefs))
  )
}

#' @title Apply EMSC to New Spectra
#'
#' @description
#' Corrects new spectra with the reference spectrum, polynomial basis and
#' interferents of a model fitted by [emsc()].
#'
#' @param object An object returned by [emsc()].
#' @param newdata A numeric matrix or data frame of new spectra, with the same
#'   variables as the calibration data.
#' @param ... Not used.
#'
#' @return A tibble of corrected spectra.
#'
#' @seealso [emsc()], [predict.specproc_filter()]
#' @export
predict.specproc_emsc <- function(object, newdata, ...) {
  x <- filter_newdata(object, newdata)
  if (anyNA(x)) {
    stop("'newdata' contains missing values.", call. = FALSE)
  }
  emsc_apply(object, x)$correction
}
