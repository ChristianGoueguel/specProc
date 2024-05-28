#' @title Biweight Location
#'
#' @description
#' This function computes the biweight location, a robust measure of central tendency
#' for a numeric vector. The biweight location is less sensitive to outliers
#' than the sample mean. The computation is done iteratively, following the
#' algorithm described in Mosteller and Tukey (1977).
#'
#' @param X A numeric vector.
#' @param loc Initial guess for the location (default: median of `X`).
#' @param c A numeric value specifying the tuning constant for the biweight estimator
#' (`c = 6` by default).
#' @param tol Convergence tolerance for the iterative computation (default: 1e-6).
#' @param max_iter Maximum number of iterations (default: 50).
#' @return The biweight location of `X`.
#' @author Christian L. Goueguel
#' @references
#'  - Mosteller, F., and Tukey, J. W. (1977). Data Analysis and Regression: A Second
#'    Course in Statistics. Addison-Wesley, pp. 203-209.
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
biweight_location <- function(X, loc = stats::median(X), c = 6, tol = 1e-6, max_iter = 50) {
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

  biloc <- loc
  mad_x <- stats::mad(X, center = loc)

  for (iter in 1:max_iter) {
    if (mad_x == 0) {
      biloc <- stats::median(X)
    } else {
      u <- (X - loc) / (c * mad_x)
      u_mask <- abs(u) < 1

      p <- sum((X[u_mask] - loc) * (1 - u[u_mask]^2)^2)
      q <- sum((1 - u[u_mask]^2)^2)

      new_biloc <- loc + (p / q)

      if (abs(new_biloc - biloc) < tol) {
        break
      }
      biloc <- new_biloc
    }
  }
  return(biloc)
  }
