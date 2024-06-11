#' @title Min-Max Normalization
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function rescales a numeric vector using the min-max normalization technique,
#' which linearly transforms the values to a new range specified by the `a` and
#' `b` arguments. The minimum value in the original vector is mapped to `a`, and
#' the maximum value is mapped to `b`.
#'
#' @param x A numeric vector.
#' @param a The minimum value of the new range (default: 0).
#' @param b The maximum value of the new range (default: 1).
#' @param drop.na A logical value indicating whether to remove missing values
#' (NA) from the calculations. If `TRUE` (the default), missing values will be
#' removed. If `FALSE`, missing values will be included.
#'
#' @return A numeric vector of the same length as `x`, with values rescaled to
#' the new range `[a, b]`.
#'
#' @export minmax
#'
minmax <- function(x, a = 0, b = 1, drop.na = TRUE) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (!is.numeric(a) || !is.numeric(b)) {
    stop("'a' and 'b' must be numeric.")
  }
  if (b <= a) {
    stop("'b' must be greater than 'a'.")
  }
  if (!is.logical(drop.na)) {
    stop("'drop.na' must be a logical value (TRUE or FALSE).")
  }

  if (drop.na) {
    x <- stats::na.omit(x)
  }

  x_min <- min(x, na.rm = drop.na)
  x_max <- max(x, na.rm = drop.na)

  if (x_min == x_max) {
    return(rep(a, length(x)))
  }

  x_rescaled <- a + ((x - x_min) * (b - a)) / (x_max - x_min)

  return(x_rescaled)
}
