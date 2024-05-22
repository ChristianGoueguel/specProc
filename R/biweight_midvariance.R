#' @title Biweight Midvariance
#' @description This function calculates the biweight midvariance of a numeric vector,
#'   which is a robust measure of scale that can be used to estimate the variability while being resistant to the influence of outliers.
#' @details
#' For scale estimaors, the standard deviation (or variance) is the optimal estimator for Gaussian data. However, it is not resistant and it does not have robustness of efficiency. The median absolute deviation (MAD) is a resistant estimate, but it has only modest robustness of efficiency.
#' The biweight midvariance estimator is both resistant and robust of efficiency. Mosteller and Tukey (1977) recommend using the MAD or interquartile range for exploratory work where moderate efficiency in a variety of situations is adequate. The biweight midvariance estimator can be considered for situations where high performance is needed.
#' Reference: Wilcox (1997), "Introduction to Robust Estimation and Hypothesis Testing," Academic Press.
#'
#' @author Christian L. Goueguel
#' @param x A numeric vector.
#' @return The biweight midvariance of the input vector.
#' @examples
#' vec <- c(1, 2, 3, 4, 5, 100)
#' biweight_midvariance(x = vec)
#' @export biweight_midvariance
biweight_midvariance <- function(x) {
  if (!rlang::is_null(x)) {
    if (!is.numeric(x)) {
      stop("Input 'x' must be a numeric vector.")
    }

    X <- dplyr::as_tibble(x) %>% dplyr::pull(value)

    beta <- (X - stats::median(X)) / (9 * stats::qnorm(0.75) * stats::mad(X))
    alpha <- dplyr::if_else(beta <= -1 | beta >= 1, 0, 1)
    n <- dplyr::n()
    nx <- sqrt(n) * sqrt(sum(alpha * (X - stats::median(X))^2 * (1 - beta^2)^4))
    dx <- abs(sum(alpha * (1 - beta^2) * (1 - 5 * beta^2)))
    res <- (nx / dx)^2

    return(res)
  } else {
    stop("Input 'x' must be provided.")
  }
}
