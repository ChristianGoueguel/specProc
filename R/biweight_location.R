#' @title Biweight Location
#'
#' @description
#' This function computes the biweight location, a robust measure of central tendency
#' for a numeric vector. The biweight location is less sensitive to outliers
#' than the sample mean.
#'
#' @details
#' Starting from the initial location \eqn{M} (the median by default), the
#' biweight location is
#' \deqn{\zeta = M + \frac{\sum_{|u_i|<1} (x_i - M)(1 - u_i^2)^2}{\sum_{|u_i|<1} (1 - u_i^2)^2},
#' \quad u_i = \frac{x_i - M}{c \cdot \mathrm{MAD}}}
#' where MAD is the (unscaled) median absolute deviation. The estimate is
#' iterated, replacing \eqn{M} by \eqn{\zeta}, until the change is smaller
#' than `tol` or `max_iter` iterations are reached.
#'
#' @param x A numeric vector.
#' @param loc Initial guess for the location (default: median of `x`).
#' @param c A numeric value specifying the tuning constant for the biweight estimator (`c = 6` by default).
#' @param tol Convergence tolerance for the iterative computation (default: 1e-6).
#' @param max_iter Maximum number of iterations (default: 50).
#' @param drop.na A logical value indicating whether to remove missing values (\code{NA}) from the calculations. If \code{TRUE}, missing values will be removed. If \code{FALSE} (the default), the result is \code{NA} when `x` contains missing values.
#'
#' @return The biweight location of `x`.
#'
#' @author Christian L. Goueguel
#'
#' @references
#'  - Mosteller, F., and Tukey, J. W. (1977).
#'    Data Analysis and Regression: A Second Course in Statistics.
#'    Addison-Wesley, pp. 203-209.
#'  - Beers, T.C., Flynn, K., Gebhardt, K., (1990).
#'    Measures of location and scale for velocities in clusters of galaxies - A robust approach.
#'    The Astronomical Journal, 100:32-46.
#'
#' @examples
#' # Example 1: Compute biweight location for a vector
#' x <- c(seq(1,100))
#' tibble::tibble(
#' mean = mean(x),
#' med = stats::median(x),
#' biloc = biweight_location(x)
#' )
#'
#' # Example 2: Biweight location is robust to outliers
#' x <- c(seq(1,99), 1e3)  # An outlier at 1000
#' tibble::tibble(
#' mean = mean(x),
#' med = stats::median(x),
#' biloc = biweight_location(x)
#' )
#'
#' @export biweight_location
#'
biweight_location <- function(x, loc = NULL, c = 6, tol = 1e-6, max_iter = 50, drop.na = FALSE) {

  if (missing(x)) {
    stop("Input 'x' must be provided.")
  }
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }
  check_number(c, "c", lower = 0, lower_open = TRUE)
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

  biloc <- if (is.null(loc)) stats::median(x) else loc
  mad_x <- stats::mad(x, center = stats::median(x), constant = 1)
  if (mad_x == 0) {
    return(stats::median(x))
  }

  for (iter in seq_len(max_iter)) {
    u <- (x - biloc) / (c * mad_x)
    keep <- abs(u) < 1
    w <- (1 - u[keep]^2)^2
    new_biloc <- biloc + sum((x[keep] - biloc) * w) / sum(w)
    converged <- abs(new_biloc - biloc) < tol
    biloc <- new_biloc
    if (converged) break
  }
  return(biloc)
}
