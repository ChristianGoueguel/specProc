#' @title Biweight Midcorrelation
#' @description This function computes the biweight midcorrelation between two numeric vectors.
#' The biweight midcorrelation is a robust measure of correlation that is less
#' sensitive to outliers than the traditional Pearson correlation coefficient.
#' @details
#' The biweight midcorrelation is calculated using the biweight midvariances
#' and biweight midcovariance, as described in the following paper:
#'
#' Wilcox, R. R. (1994). The Biweight Midcorrelation: A Robust Correlation
#' Technique for Two Samples. Journal of Statistical Computation and Simulation,
#' 48(2), 103-110.
#'
#' @author Christian L. Goueguel
#' @param X A numeric vector.
#' @param Y A numeric vector of the same length as X.
#' @return The biweight midcorrelation between X and Y.
#' @examples
#' x <- rnorm(100)
#' y <- 2 * x + rnorm(100)
#' biweight_midcorrelation(x, y)
#' @export biweight_midcorrelation
biweight_midcorrelation <- function(X, Y) {
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

  # Biweight midvariances
  nx <- sqrt(n) * sqrt(sum(alpha * ((X - med_x)^2) * ((1 - beta^2)^4)))
  ny <- sqrt(n) * sqrt(sum(kappa * ((Y - med_y)^2) * ((1 - theta^2)^4)))
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
