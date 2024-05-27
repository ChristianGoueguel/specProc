#' @title Biweight Midcovariance
#'
#' @description
#' This function computes the biweight midcovariance, a robust measure of
#' covariance between two numerical vectors. The biweight midcovariance is
#' less sensitive to outliers than the traditional covariance.
#'
#' @references
#'  - Wilcox, R., (1997). Introduction to Robust Estimation and Hypothesis Testing.
#'    Academic Press
#'
#' @param X A numeric vector.
#' @param Y A numeric vector of the same length as `X`.
#' @return The biweight midcovariance between `X` and `Y`.
#' @examples
#' # Example 1: Compute biweight midcovariance for two vectors
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 3, 4, 5, 6)
#' biweight_midcovariance(x, y)
#'
#' # Example 2: Biweight midcovariance is robust to outliers
#' x <- c(1, 2, 3, 4, 100)  # An outlier at 100
#' y <- c(2, 3, 4, 5, 6)
#' biweight_midcovariance(x, y)
#' @export biweight_midcovariance
biweight_midcovariance <- function(X, Y) {
  if (missing(X) || missing(Y)) {
    stop("Inputs 'X' and 'Y' must be provided.")
  }
  if (!is.numeric(X) || !is.numeric(Y)) {
    stop("Both 'X' and 'Y' must be numeric vectors.")
  }
  if (length(X) != length(Y)) {
    stop("'X' and 'Y' must have the same length.")
  }
  if (length(unique(X)) == 1 || length(unique(Y)) == 1) {
    stop("'X' and 'Y' cannot be constant vectors.")
  }
  if (length(X) < 2 || length(Y) < 2) {
    stop("'X' and 'Y' must have at least two elements.")
  } else {
    n <- length(X)
  }

  med_x <- stats::median(X)
  med_y <- stats::median(Y)
  mad_x <- stats::mad(X)
  mad_y <- stats::mad(Y)

  beta <- (X - med_x) / (9 * stats::qnorm(0.75) * mad_x)
  theta <- (Y - med_y) / (9 * stats::qnorm(0.75) * mad_y)

  alpha <- dplyr::if_else(beta <= -1 | beta >= 1, 0, 1)
  kappa <- dplyr::if_else(theta <= -1 | theta >= 1, 0, 1)

  A <- n * sum((alpha * (X - med_x)) * ((1 - beta^2)^2) * (kappa * (Y - med_y)) * ((1 - theta^2)^2))
  B <- sum((alpha * (1 - beta^2)) * (1 - 5 * beta^2)) * sum((kappa * (1 - theta^2)) * (1 - 5 * theta^2))
  bicovar <- A / B

  return(bicovar)
}
