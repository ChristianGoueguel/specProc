#' @title Multiplicative Signal Correction
#'
#' @author Christian L. Goueguel
#'
#' @description
#'  This function performs multiplicative scatter/signal correction (MSC) on an
#'  input data of spectra. It corrects for multiplicative and additive effects
#'  in the spectral data by regressing against a reference spectrum.
#'
#' @details
#' Each spectrum \eqn{\textbf{x}_i} is regressed against the reference spectrum,
#' \eqn{\textbf{x}_i = a_i + b_i \textbf{x}_{ref} + \textbf{e}_i}, and corrected as
#' \eqn{(\textbf{x}_i - a_i) / b_i}. When `window` is given, the regression and
#' correction are carried out separately in each spectral window (piecewise MSC);
#' variables outside all windows are left unchanged.
#'
#' To correct new spectra consistently, pass the returned `reference` as `xref`.
#'
#' @param x A numeric matrix or data frame containing the input spectra.
#' Each row represents a sample, and each column represents a spectral variable.
#' @param xref An optional numeric vector representing the reference spectrum.
#' If `NULL` (default), the mean or median of `x` is used as the reference.
#' @param drop.offset A logical value indicating whether the additive offset
#' \eqn{a_i} should be removed from the spectra (default is `TRUE`). If `FALSE`,
#' only the multiplicative effect is corrected: \eqn{\textbf{x}_i / b_i}.
#' @param robust A logical value indicating whether the median (`TRUE`, default)
#' or the mean (`FALSE`) of `x` is used as `xref`.
#' @param window An optional list of numeric vectors specifying the column indices of
#' spectral windows. If provided, MSC is performed separately for each window.
#' @param drop.na A logical value indicating whether to remove spectra (rows)
#' containing missing values. If `TRUE` (the default), such rows are removed.
#'
#' @return A list with the following components:
#'   \item{`correction`}{The corrected spectra.}
#'   \item{`offset`}{The intercepts/offsets \eqn{a_i} (a matrix with one column per window when `window` is given).}
#'   \item{`slope`}{The multiplicative scatter factors/slopes \eqn{b_i} (a matrix with one column per window when `window` is given).}
#'   \item{`reference`}{The reference spectrum used.}
#'
#' @export msc
#'
#' @examples
#' set.seed(1)
#' base <- sin(seq(0, pi, length.out = 50))
#' x <- t(sapply(1:10, function(i) runif(1, 0, 1) + runif(1, 0.5, 2) * base))
#' res <- msc(x)
#' range(apply(as.matrix(res$correction), 2, sd))
#'
msc <- function(
    x,
    xref = NULL,
    drop.offset = TRUE,
    robust = TRUE,
    window = NULL,
    drop.na = TRUE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  x <- as_numeric_matrix(x, "x")
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

  if (drop.na) {
    x <- x[stats::complete.cases(x), , drop = FALSE]
  } else if (anyNA(x)) {
    stop("'x' contains missing values; set 'drop.na = TRUE' to remove them.")
  }
  if (nrow(x) < 1) {
    stop("'x' has no complete spectra.")
  }

  if (is.null(xref)) {
    xref <- if (robust) apply(x, 2, stats::median) else colMeans(x)
  }
  xref <- as.numeric(xref)

  if (is.null(window)) {
    window <- list(seq_len(ncol(x)))
  } else {
    if (!is.list(window)) {
      window <- list(window)
    }
    ok <- vapply(window, function(w) is.numeric(w) && length(w) >= 2 &&
                   all(w %in% seq_len(ncol(x))), logical(1))
    if (!all(ok)) {
      stop("'window' must be a list of column index vectors, each of length >= 2.")
    }
  }

  sx <- x
  offset <- slope <- matrix(NA_real_, nrow(x), length(window))
  for (k in seq_along(window)) {
    w <- window[[k]]
    r <- xref[w]
    rc <- r - mean(r)
    if (sum(rc^2) == 0) {
      stop("The reference spectrum is constant within a window.")
    }
    xw <- x[, w, drop = FALSE]
    b <- drop(sweep(xw, 1, rowMeans(xw)) %*% rc) / sum(rc^2)
    a <- rowMeans(xw) - b * mean(r)
    sx[, w] <- if (drop.offset) (xw - a) / b else xw / b
    offset[, k] <- a
    slope[, k] <- b
  }
  if (length(window) == 1) {
    offset <- drop(offset)
    slope <- drop(slope)
  }

  res <- list(
    "correction" = as_tbl(sx, colnames(x)),
    "offset" = offset,
    "slope" = slope,
    "reference" = xref
  )
  return(res)
}
