#' @title Biweight Scale
#'
#' @description
#' This function computes the biweight scale, a robust measure of scale or dispersion
#' for a numeric vector. The biweight scale is less sensitive to outliers
#' than the sample standard deviation.
#'
#' @param X A numeric vector.
#' @param loc Initial guess for the location (default: median of `X`).
#' @param c A numeric value specifying the tuning constant for the biweight estimator
#' (`c = 9` by default).
#' @param reduced A logical value specifying whether the sample size, *n*,
#' should be reduced to the number of non-rejected values. If `TRUE`, *n* is
#' reduced to the number of observations that pass a rejection criteria.
#' If `FALSE` (default), *n* is equal to the length of `X` (the input data).
#' @param tol Convergence tolerance for the iterative computation (default: 1e-6).
#' @param max_iter Maximum number of iterations (default: 50).
#' @return The biweight scale of `X`.
#' @author Christian L. Goueguel
#' @references
#'  - Mosteller, F., and Tukey, J. W. (1977). Data Analysis and Regression: A Second
#'    Course in Statistics. Addison-Wesley, pp. 203-209.
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
#' @export biweight_scale
biweight_scale <- function(X, loc = stats::median(X), c = 9, reduced = FALSE, tol = 1e-6, max_iter = 50) {
  if (missing(X)) {
    stop("Input 'X' must be provided.")
  }
  if (!is.numeric(X)) {
    stop("'X' must be a numeric vector.")
  }
  if (length(unique(X)) == 1) {
    stop("'X' cannot be a constant vector.")
  }
  if (length(X) < 2) {
    stop("'X' must have at least two elements.")
  }
  if (!is.numeric(c)) {
    stop("'c' must be numeric.")
  }
  if (!is.logical(reduced)) {
    stop("'reduced' must be a logical value (TRUE or FALSE).")
  }

  mad_x <- stats::mad(X, center = loc)
  biscale <- mad_x

  for (iter in 1:max_iter) {
    if (mad_x == 0) {
      biscale <- 0
    } else {
      u <- (X - loc) / (c * mad_x)
      u_mask <- abs(u) < 1

      p <- sum((X[u_mask] - loc)^2 * (1 - u[u_mask]^2)^4)
      q <- sum((1 - u[u_mask]^2)^2 * (1 - 5 * u[u_mask]^2))

      n <- dplyr::if_else(reduced == FALSE, length(X), length(u_mask))
      new_biscale <- sqrt(n) * (sqrt(p) / abs(q))

      if (abs(new_biscale - biscale) < tol) {
        break
      }
      biscale <- new_biscale
    }
  }
  return(biscale)
}
