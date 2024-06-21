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

  if (missing(x) || missing(y)) {
    stop("Inputs 'x' and 'y' must be provided.")
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both 'x' and 'y' must be numeric vectors.")
  }
  if (length(x) != length(y)) {
    stop("'x' and 'y' must have the same length.")
  }
  if (length(unique(x)) == 1 || length(unique(y)) == 1) {
    stop("'x' and 'y' cannot be constant vectors.")
  }
  if (length(x) < 2 || length(y) < 2) {
    stop("'x' and 'y' must have at least two elements.")
  } else {
    n <- length(x)
  }

  med_x <- stats::median(x)
  med_y <- stats::median(y)
  mad_x <- stats::mad(x)
  mad_y <- stats::mad(y)

  beta <- (x - med_x) / (9 * stats::qnorm(0.75) * mad_x)
  theta <- (y - med_y) / (9 * stats::qnorm(0.75) * mad_y)

  alpha <- dplyr::if_else(beta <= -1 | beta >= 1, 0, 1)
  kappa <- dplyr::if_else(theta <= -1 | theta >= 1, 0, 1)

  A <- n * sum((alpha * (x - med_x)) * ((1 - beta^2)^2) * (kappa * (y - med_y)) * ((1 - theta^2)^2))
  B <- sum((alpha * (1 - beta^2)) * (1 - 5 * beta^2)) * sum((kappa * (1 - theta^2)) * (1 - 5 * theta^2))
  bicovar <- A / B

  return(bicovar)
}
