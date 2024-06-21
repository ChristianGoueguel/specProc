#' @title Biweight Midvariance
#'
#' @description
#' This function calculates the biweight midvariance of a numeric vector,
#' which is a robust measure of scale that can be used to estimate the
#' variability of the data while being resistant to the influence of outliers.
#'
#' @details
#' For scale estimators, the standard deviation (or variance) is the optimal
#' estimator for Gaussian data. However, it is not resistant and it does not
#' have robustness of efficiency. In robust statistics, the median absolute
#' deviation (MAD) is a resistant estimate, but it has only modest robustness
#' of efficiency, while the biweight midvariance estimator is both resistant
#' and robust of efficiency.
#'
#' @references
#'    - Wilcox, R., (1997).
#'      Introduction to Robust Estimation and Hypothesis Testing.
#'      Academic Press
#'
#' @author Christian L. Goueguel
#'
#' @param x A numeric vector.
#' @param drop.na A logical value indicating whether to remove missing values (\code{NA}) from the calculations. If \code{TRUE} (the default), missing values will be removed. If \code{FALSE}, missing values will be included in the calculations.
#'
#' @return The biweight midvariance of the input vector.
#'
#' @examples
#' vec <- c(1, 2, 3, 4, 4, 2)
#' stats::var(vec)
#' biweight_midvariance(vec)
#'
#' vec <- c(1, 2, 3, 4, 4, 100)
#' stats::var(vec)
#' biweight_midvariance(vec)
#'
#' @export biweight_midvariance
#'
biweight_midvariance <- function(x, drop.na = FALSE) {

  if (missing(x)) {
    stop("Input 'x' must be provided.")
  }
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector.")
  }
  if (length(unique(x)) == 1) {
    stop("Input 'x' cannot be constant a vector.")
  }

  if (drop.na) {
    x <- x[!is.na(x)]
  }

  if (length(x) < 2) {
    stop("The length of 'x' cannot be less than 2.")
  } else {
    n <- length(x)
  }

  med_x <- stats::median(x)
  mad_x <- stats::mad(x)
  beta <- (x - med_x) / (9 * stats::qnorm(0.75) * mad_x)
  alpha <- dplyr::if_else(beta <= -1 | beta >= 1, 0, 1)

  nx <- sqrt(n) * sqrt(sum(alpha * ((x - med_x)^2) * ((1 - beta^2)^4)))
  dx <- abs(sum(alpha * (1 - beta^2) * (1 - 5 * beta^2)))

  biweight_var <- (nx / dx)^2

  return(biweight_var)
}
