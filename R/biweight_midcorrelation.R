#' @title Biweight Midcorrelation
#'
#' @description
#' This function computes the biweight midcorrelation between two numeric vectors.
#' The biweight midcorrelation is a robust measure of correlation that is less
#' sensitive to outliers than the traditional Pearson's correlation coefficient.
#'
#' @details
#' The biweight midcorrelation is calculated using the biweight midvariances
#' and biweight midcovariance, as described by Wilcox (1994).
#'
#' @references
#'    - Wilcox, R. R. (1994).
#'      The Biweight Midcorrelation: A Robust Correlation Technique for Two Samples.
#'      Journal of Statistical Computation and Simulation, 48(2):103-110.
#'
#' @author Christian L. Goueguel
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as `x`.
#'
#' @return The biweight midcorrelation between `x` and `y`.
#'
#' @examples
#' x <- rnorm(100)
#' y <- 2 * x + rnorm(100)
#' biweight_midcorrelation(x, y)
#'
#' @export biweight_midcorrelation
#'
biweight_midcorrelation <- function(x, y) {
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

  # Biweight midvariances
  nx <- sqrt(n) * sqrt(sum(alpha * ((x - med_x)^2) * ((1 - beta^2)^4)))
  ny <- sqrt(n) * sqrt(sum(kappa * ((y - med_y)^2) * ((1 - theta^2)^4)))
  dx <- abs(sum(alpha * (1 - beta^2) * (1 - 5 * beta^2)))
  dy <- abs(sum(kappa * (1 - theta^2) * (1 - 5 * theta^2)))

  Sxx <- (nx / dx)^2
  Syy <- (ny / dy)^2

  # Biweight midcovariance
  Sxy <- A / B

  # Biweight midcorrelation
  bicor <- Sxy / sqrt(Sxx * Syy)

  return(bicor)
}
