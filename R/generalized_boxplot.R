#' @title Generalized Boxplot
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function computes the Generalized Boxplot, a robust data visualization technique
#' for detecting outliers and skewness in a distribution, as proposed by
#' Bruffaerts *et al*. (2014). It uses the Tukey g-and-h distribution (from the package `TukeyGH77`)
#' to model the data.
#'
#' @param x A numeric vector containing the data.
#' @param alpha A numeric value between 0 and 1 specifying the desired coverage rate. Default is 0.95.
#' @param gamma gamma to be defined
#' @return A list containing the following components:
#'   \describe{
#'     \item{lower.fence}{The lower fence value.}
#'     \item{lower.quartile}{The lower quartile value.}
#'     \item{median}{The median value.}
#'     \item{upper.quartile}{The upper quartile value.}
#'     \item{upper.fence}{The upper fence value.}
#'     \item{lower.outliers}{A vector containing the lower outliers.}
#'     \item{upper.outliers}{A vector containing the upper outliers.}
#'   }
#'
#' @references
#' Hubert, M., and Vandervieren, E. (2008). An adjusted boxplot for skewed distributions.
#' Computational Statistics & Data Analysis, 52(12):5186-5201.
#'
#' Bruffaerts, C., Verardi, V., Vermandele, C. (2014). A generalized boxplot for
#' skewed and heavy-tailed distributions. Statistics and Probability Letters 95(C):110–117
#'
#' @examples
#' # Compute the Generalized Boxplot for a normal distribution
#' x <- rnorm(100)
#' gb <- generalized_boxplot(x)
#' print(gb)
#'
#' # Compute the Generalized Boxplot for a skewed distribution
#' x <- rexp(100)
#' gb <- generalized_boxplot(x)
#' print(gb)
#'
#' @export generalized_boxplot
generalized_boxplot <- function(x, alpha = 0.95, gamma = 0.5) {



  n <- length(x)
  x <- sort(x)

  # Compute the medcouple (robust skewness measure)
  mc <- robustbase::mc(x)

  # Compute the kernel weights
  kernel_weights <- function(x, gamma) {
    n <- length(x)
    weights <- numeric(n)
    for (i in seq_len(n)) {
      weights[i] <- (1 - (abs(x[i] - stats::median(x)) / (gamma * stats::mad(x, constant = 1.4826)))^3)^3 * (x[i] <= stats::median(x) + gamma * stats::mad(x, constant = 1.4826)) *
        (x[i] >= stats::median(x) - gamma * stats::mad(x, constant = 1.4826))
    }
    return(weights)
  }

  w <- kernel_weights(x, gamma)

  # Compute the weighted quantiles
  weighted_quantile <- function(x, probs, weights) {
    n <- length(x)
    x <- sort(x)
    p <- pmax(pmin(probs, 1 - 1e-10), 1e-10)
    n_product <- sum(weights)
    quantiles <- vapply(p, function(p) {
      cumsum_weights <- cumsum(weights) / n_product
      x[max(which(cumsum_weights <= p))]
    }, numeric(1))
    return(quantiles)
  }

  q_alpha <- weighted_quantile(x, probs = c(alpha / 2, 0.5, 1 - alpha / 2), weights = w)
  lower_fence <- q_alpha[1]
  lower_quartile <- stats::median(x[x <= q_alpha[2]])
  median <- q_alpha[2]
  upper_quartile <- stats::median(x[x >= q_alpha[2]])
  upper_fence <- q_alpha[3]

  lower_outliers <- x[x < lower_fence]
  upper_outliers <- x[x > upper_fence]

  result <- list(
    lower.fence = lower_fence,
    lower.quartile = lower_quartile,
    median = median,
    upper.quartile = upper_quartile,
    upper.fence = upper_fence,
    lower.outliers = lower_outliers,
    upper.outliers = upper_outliers
  )

  return(result)
}
