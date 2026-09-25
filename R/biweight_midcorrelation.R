#' @title Biweight Midcorrelation
#'
#' @description
#' This function computes the biweight midcorrelation between two numeric vectors.
#' The biweight midcorrelation is a robust measure of correlation that is less
#' sensitive to outliers than the traditional Pearson's correlation coefficient.
#'
#' @details
#' The biweight midcorrelation is calculated using the biweight midvariances
#' and biweight midcovariance, as described by Wilcox (1994). It is bounded
#' between -1 and 1.
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
#' set.seed(11230)
#' x <- rnorm(100)
#' y <- 2 * x + rnorm(100)
#' biweight_midcorrelation(x, y)
#'
#' @export biweight_midcorrelation
#'
biweight_midcorrelation <- function(x, y) {
  check_bivariate(x, y)
  if (anyNA(x) || anyNA(y)) {
    return(NA_real_)
  }
  wx <- biweight_terms(x)
  wy <- biweight_terms(y)
  if (is.null(wx) || is.null(wy)) {
    return(NA_real_)
  }
  return(sum(wx$a * wy$a) / sqrt(sum(wx$a^2) * sum(wy$a^2)))
}
