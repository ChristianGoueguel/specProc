#' @title Biweight Midvariance
#' @description This function calculates the biweight midvariance of a numeric vector,
#'   which is a robust measure of scale that can be used to estimate the variability of the data while being resistant to the influence of outliers.
#' @details
#' For scale estimators, the standard deviation (or variance) is the optimal estimator for Gaussian data. However, it is not resistant and it does not have robustness of efficiency.
#' In robust statistics, the median absolute deviation (MAD) is a resistant estimate, but it has only modest robustness of efficiency, while the biweight midvariance estimator is both resistant and robust of efficiency.
#' Wilcox (1997), "Introduction to Robust Estimation and Hypothesis Testing," Academic Press.
#'
#' @author Christian L. Goueguel
#' @param X A numeric vector.
#' @return The biweight midvariance of the input vector.
#' @examples
#' vec <- c(1, 2, 3, 4, 5, 100)
#' biweight_midvariance(X = vec)
#' @export biweight_midvariance
biweight_midvariance <- function(X) {
  value <- NULL
  if (!rlang::is_null(X)) {
    if (!is.numeric(X)) {
      stop("Input 'X' must be a numeric vector.")
    }

    beta <- (X - stats::median(X)) / (9 * stats::qnorm(0.75) * stats::mad(X))
    alpha <- dplyr::if_else(beta <= -1 | beta >= 1, 0, 1)
    n <- nrow(as.matrix(X))
    nx <- sqrt(n) * sqrt(sum(alpha * (X - stats::median(X))^2 * (1 - beta^2)^4))
    dx <- abs(sum(alpha * (1 - beta^2) * (1 - 5 * beta^2)))
    res <- (nx / dx)^2

    return(res)
  } else {
    stop("Input 'X' must be provided.")
  }
}
