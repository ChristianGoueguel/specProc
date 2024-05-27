#' @title Biweight Scale
#'
#' @description
#' This function computes the biweight scale, a robust measure of scale or dispersion
#' for a numeric vector. The biweight scale is less sensitive to outliers
#' than the sample standard deviation. The computation is done iteratively,
#' following the algorithm described in Mosteller and Tukey (1977).
#'
#' @param X A numeric vector.
#' @param loc Location estimate to use (default: biweight location of `X`).
#' @param tol Convergence tolerance for the iterative computation (default: 1e-6).
#' @param max_iter Maximum number of iterations (default: 50).
#' @return The biweight scale of `X`.
#'
#' @references
#'  - Mosteller, F., and Tukey, J. W. (1977). Data Analysis and Regression: A Second
#'    Course in Statistics. Addison-Wesley, pp. 203-209.
#'
#' @examples
#' # Example 1: Compute biweight scale for a vector
#' x <- c(1, 2, 3, 4, 5)
#' stats::sd(x)
#' stats::mad(x)
#' biweight_scale(x)
#'
#' # Example 2: Biweight scale is robust to outliers
#' x <- c(1, 2, 3, 4, 100)  # An outlier at 100
#' stats::sd(x)
#' stats::mad(x)
#' biweight_scale(x)
#'
#' @export biweight_scale
biweight_scale <- function(X, loc = biweight_location(X), tol = 1e-6, max_iter = 50) {
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

  biweight_scale <- stats::mad(X, center = loc)

  for (iter in 1:max_iter) {
    u <- (X - loc) / (9 * stats::qnorm(0.75) * biweight_scale)
    w <- dplyr::if_else(abs(u) <= 1, (1 - u^2)^2, 0)
    new_biweight_scale <- stats::mad(X * w, center = 0, constant = 1) / stats::mad(w, center = 0, constant = 1)

    if (abs(new_biweight_scale - biweight_scale) < tol) {
      break
    }

    biweight_scale <- new_biweight_scale
  }

  return(biweight_scale)
}
