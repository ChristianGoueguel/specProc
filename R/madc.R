#' @title Unbiased Median Absolute Deviation Scale Estimator
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function calculates the Median Absolute Deviation (MAD) scale
#' estimator for a numeric vector, with a sample correction factor to make
#' the mad unbiased at the normal distribution.
#'
#' @param x A numeric vector.
#' @param drop.na A logical value indicating whether to remove missing values
#' (NA) from the calculations. If `TRUE` (the default), missing values will be
#' removed. If `FALSE`, missing values will be included.
#'
#' @return The MAD scale estimate for the input vector `x`.
#'
#' @export madc
madc <- function(x, drop.na = TRUE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (!is.logical(drop.na) || length(drop.na) != 1) {
    stop("'drop.na' must be a single logical value (TRUE or FALSE).")
  }

  if (drop.na) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  med_x <- stats::median(x)
  mad <- stats::median(abs(x - med_x))

  if (n > 9) {
    b_n <- n / (n - 0.8)
  } else {
    b_n <- 1
  }

  madc_estimate <- b_n * 1.4826 * mad

  return(madc_estimate)
}
