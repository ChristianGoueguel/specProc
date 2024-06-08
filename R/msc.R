#' @title Multiplicative Signal Correction
#'
#' @author Christian L. Goueguel
#'
#' @description
#'  This function performs multiplicative scatter/signal correction (MSC) on an
#'  input data of spectra. It corrects for multiplicative and additive effects
#'  in the spectral data by regressing against a reference spectrum.
#'
#' @param x A numeric matrix or data frame containing the input spectra.
#' Each row represents a sample, and each column represents a spectral variable.
#' @param xref An optional numeric vector representing the reference spectrum.
#' If `NULL` (default), the mean or median of `x` is used as the reference.
#' @param drop.offset A logical value indicating whether the offset should be removed from
#' the spectra (default is `TRUE`).
#' @param robust A logical value indicating whether the mean or median of `x` is used as `xref`.
#' @param window An optional list of numeric vectors specifying the indices of
#' spectral windows. If provided, MSC is performed separately for each window.
#' @param drop.na A logical value indicating whether to remove missing values
#' (NA) from the calculations. If `TRUE` (the default), missing values will be
#' removed. If `FALSE`, missing values will be included.
#'
#' @return A list with the following components:
#'   \item{`correction`}{The corrected spectra.}
#'   \item{`offset`}{The intercepts/offsets.}
#'   \item{`slope`}{The multiplicative scatter factors/slopes.}
#'
#' @export msc
#'
msc <- function(x, xref = NULL, drop.offset = TRUE, robust = TRUE, window = NULL, drop.na = TRUE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.null(xref) && (!is.numeric(xref) || length(xref) != ncol(x))) {
    stop("'xref' must be a numeric vector of the same length as the number of columns in 'x'.")
  }
  if (!is.logical(drop.offset)) {
    stop("'drop.offset' must be a logical value (TRUE or FALSE).")
  }
  if (!is.logical(robust)) {
    stop("'robust' must be a logical value (TRUE or FALSE).")
  }
  if (!is.logical(drop.na)) {
    stop("'drop.na' must be a logical value (TRUE or FALSE).")
  }

  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }
  if (drop.na) {
    x <- na.omit(x)
  }

  if (is.null(xref)) {
    if (robust) {
      xref <- apply(x, 2, median)
    } else {
      xref <- apply(x, 2, mean)
    }
  }

  if (is.null(window)) {
    alpha <- colMeans(x[, subind, drop = FALSE] - xref[subind])
    beta <- apply(x[, subind, drop = FALSE], 2, function(y) cov(y, xref[subind]) / var(xref[subind]))
    sx <- sweep(x, 2, alpha, `-`) / beta
  } else {
    sx <- x
    for (w in window) {
      alpha <- colMeans(x[, w, drop = FALSE] - xref[w])
      beta <- apply(x[, w, drop = FALSE], 2, function(y) cov(y, xref[w]) / var(xref[w]))
      sx[, w] <- sweep(x[, w, drop = FALSE], 2, alpha, `-`) / beta
    }
    alpha <- beta <- NULL
  }

  if (!drop.offset) {
    sx <- sweep(sx, 2, colMeans(sx), `-`)
  }

  res <- list(
    "correction" = tibble::as_tibble(sx),
    "offset" = alpha,
    "slope" = beta
    )

  return(res)
}
