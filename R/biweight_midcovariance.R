#' @title Biweight Midcovariance
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function computes the biweight midcovariance, a robust measure of
#' covariance between two numerical vectors. The biweight midcovariance is
#' less sensitive to outliers than the traditional covariance.
#'
#' @references
#'    - Wilcox, R., (1997).
#'      Introduction to Robust Estimation and Hypothesis Testing.
#'      Academic Press
#'
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as `x`.
#' @return The biweight midcovariance between `x` and `y`.
#'
#' @examples
#' # Example 1: Compute biweight midcovariance for two vectors
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 3, 4, 5, 6)
#' stats::cov(x, y)
#' biweight_midcovariance(x, y)
#'
#' # Example 2: Biweight midcovariance is robust to outliers
#' x <- c(1, 2, 3, 4, 100)  # An outlier at 100
#' y <- c(2, 3, 4, 5, 6)
#' stats::cov(x, y)
#' biweight_midcovariance(x, y)
#'
#' @export biweight_midcovariance
#'
biweight_midcovariance <- function(x, y) {
  check_bivariate(x, y)
  if (anyNA(x) || anyNA(y)) {
    return(NA_real_)
  }
  wx <- biweight_terms(x)
  wy <- biweight_terms(y)
  if (is.null(wx) || is.null(wy)) {
    return(0)
  }
  return(length(x) * sum(wx$a * wy$a) / (sum(wx$d) * sum(wy$d)))
}

check_bivariate <- function(x, y) {
  if (missing(x) || missing(y)) {
    stop("Inputs 'x' and 'y' must be provided.", call. = FALSE)
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both 'x' and 'y' must be numeric vectors.", call. = FALSE)
  }
  if (length(x) != length(y)) {
    stop("'x' and 'y' must have the same length.", call. = FALSE)
  }
  if (length(x) < 2) {
    stop("'x' and 'y' must have at least two elements.", call. = FALSE)
  }
  if (length(unique(x)) == 1 || length(unique(y)) == 1) {
    stop("'x' and 'y' cannot be constant vectors.", call. = FALSE)
  }
  invisible(TRUE)
}
