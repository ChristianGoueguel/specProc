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
#' @param drop.na A logical value indicating whether to remove missing values (\code{NA}) from the calculations. If \code{TRUE}, missing values will be removed. If \code{FALSE} (the default), the result is \code{NA} when `x` contains missing values.
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
  check_flag(drop.na, "drop.na")

  if (drop.na) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(NA_real_)
  }
  if (length(x) < 2) {
    stop("The length of 'x' cannot be less than 2.")
  }
  if (length(unique(x)) == 1) {
    stop("Input 'x' cannot be constant a vector.")
  }

  w <- biweight_terms(x)
  if (is.null(w)) {
    return(0)
  }
  return(length(x) * sum(w$a^2) / sum(w$d)^2)
}

# Terms shared by the biweight midvariance, midcovariance and midcorrelation
# (Wilcox, 1997): a_i = (x_i - M)(1 - u_i^2)^2 and d_i = (1 - u_i^2)(1 - 5u_i^2)
# over |u_i| < 1, with u_i = (x_i - M) / (9 * MAD). Returns NULL when MAD = 0.
biweight_terms <- function(x) {
  med <- stats::median(x)
  mad_x <- stats::mad(x, center = med, constant = 1)
  if (mad_x == 0) {
    return(NULL)
  }
  u <- (x - med) / (9 * mad_x)
  keep <- as.numeric(abs(u) < 1)
  list(
    a = keep * (x - med) * (1 - u^2)^2,
    d = keep * (1 - u^2) * (1 - 5 * u^2)
  )
}
