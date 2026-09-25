#' @title Biweight Scale
#'
#' @description
#' This function computes the biweight scale, a robust measure of scale or dispersion
#' for a numeric vector. The biweight scale is less sensitive to outliers
#' than the sample standard deviation.
#'
#' @details
#' The biweight scale is the square root of the biweight midvariance:
#' \deqn{\zeta = \frac{\sqrt{n \sum_{|u_i|<1} (x_i - M)^2 (1 - u_i^2)^4}}
#' {\left| \sum_{|u_i|<1} (1 - u_i^2)(1 - 5u_i^2) \right|},
#' \quad u_i = \frac{x_i - M}{c \cdot \mathrm{MAD}}}
#' where \eqn{M} is the location (the median by default) and MAD is the
#' (unscaled) median absolute deviation. For normally distributed data the
#' biweight scale is a consistent estimator of the standard deviation.
#'
#' @param x A numeric vector.
#' @param loc The location about which the scale is computed (default: median of `x`).
#' @param c A numeric value specifying the tuning constant for the biweight estimator (`c = 9` by default).
#' @param reduced A logical value specifying whether the sample size, *n*, should be reduced to the number of non-rejected values. If `TRUE`, *n* is reduced to the number of observations that pass a rejection criteria (\eqn{|u_i| < 1}). If `FALSE` (default), *n* is equal to the length of `x` (the input data).
#' @param drop.na A logical value indicating whether to remove missing values (\code{NA}) from the calculations. If \code{TRUE}, missing values will be removed. If \code{FALSE} (the default), the result is \code{NA} when `x` contains missing values.
#'
#' @return The biweight scale of `x`.
#'
#' @author Christian L. Goueguel
#'
#' @references
#'    - Mosteller, F., and Tukey, J. W. (1977).
#'      Data Analysis and Regression: A Second Course in Statistics.
#'      Addison-Wesley, pp. 203-209.
#'    - Beers, T.C., Flynn, K., Gebhardt, K., (1990).
#'      Measures of location and scale for velocities in clusters of galaxies - A robust approach.
#'      The Astronomical Journal, 100:32-46.
#'
#' @examples
#' # Example 1: Compute biweight scale for a vector
#' x <- c(seq(1,100))
#' tibble::tibble(
#' sd = stats::sd(x),
#' mad = stats::mad(x),
#' biscale = biweight_scale(x)
#' )
#'
#' # Example 2: Biweight scale is robust to outliers
#' x <- c(seq(1,99), 1e3) # An outlier at 1000
#' tibble::tibble(
#' sd = stats::sd(x),
#' mad = stats::mad(x),
#' biscale = biweight_scale(x)
#' )
#'
#' @export biweight_scale
#'
biweight_scale <- function(x, loc = NULL, c = 9, reduced = FALSE, drop.na = FALSE) {

  if (missing(x)) {
    stop("Input 'x' must be provided.")
  }
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (!is.numeric(c)) {
    stop("'c' must be numeric.")
  }
  if (!is.logical(reduced)) {
    stop("'reduced' must be a logical value (TRUE or FALSE).")
  }
  check_flag(drop.na, "drop.na")

  if (drop.na) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(NA_real_)
  }
  if (length(x) < 2) {
    stop("'x' must have at least two elements.")
  }
  if (length(unique(x)) == 1) {
    stop("'x' cannot be a constant vector.")
  }

  if (is.null(loc)) {
    loc <- stats::median(x)
  }
  mad_x <- stats::mad(x, center = loc, constant = 1)
  if (mad_x == 0) {
    return(0)
  }

  u <- (x - loc) / (c * mad_x)
  keep <- abs(u) < 1
  u2 <- u[keep]^2
  num <- sum((x[keep] - loc)^2 * (1 - u2)^4)
  den <- sum((1 - u2) * (1 - 5 * u2))
  n <- if (reduced) sum(keep) else length(x)

  return(sqrt(n * num) / abs(den))
}
