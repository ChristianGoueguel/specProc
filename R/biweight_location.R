#' @title Biweight Location
#'
#' @description
#' This function computes the biweight location, a robust measure of central tendency
#' for a numeric vector. The biweight location is less sensitive to outliers
#' than the sample mean. The computation is done iteratively, following the
#' algorithm described in Mosteller and Tukey (1977).
#'
#' @param X A numeric vector.
#' @param tol Convergence tolerance for the iterative computation (default: 1e-6).
#' @param max_iter Maximum number of iterations (default: 50).
#' @return The biweight location of `X`.
#' @author Christian L. Goueguel
#' @references
#'  - Mosteller, F., and Tukey, J. W. (1977). Data Analysis and Regression: A Second
#'    Course in Statistics. Addison-Wesley, pp. 203-209.
#' @examples
#' # Example 1: Compute biweight location for a vector
#' x <- c(1, 2, 3, 4, 5)
#' mean(x)
#' biweight_location(x)
#'
#' # Example 2: Biweight location is robust to outliers
#' x <- c(1, 2, 3, 4, 100)  # An outlier at 100
#' mean(x)
#' biweight_location(x)
#'
#' @export biweight_location
biweight_location <- function(X, tol = 1e-6, max_iter = 50) {
  if (missing(X)) {
    stop("Input 'x' must be provided.")
  }
  if (!is.numeric(X)) {
    stop("'x' must be a numeric vector.")
  }
  if (length(unique(X)) == 1) {
    stop("'x' cannot be a constant vector.")
  }
  if (length(X) < 2) {
    stop("'x' must have at least two elements.")
  }

  biweight_loc <- stats::median(x)

  for (iter in 1:max_iter) {
    mad <- stats::mad(x, center = biweight_loc)
    u <- (x - biweight_loc) / (9 * stats::qnorm(0.75) * mad)
    w <- dplyr::if_else(abs(u) <= 1, (1 - u^2)^2, 0)
    new_biweight_loc <- sum(x * w) / sum(w)

    if (abs(new_biweight_loc - biweight_loc) < tol) {
      break
    }

    biweight_loc <- new_biweight_loc
  }

  return(biweight_loc)
}
