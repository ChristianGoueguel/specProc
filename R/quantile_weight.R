#' @title Quantile Tail Weight Measure
#'
#' @description
#' This function calculates the left quantile weight (LQW) and the right
#' quantile weight (RQW) for a given numeric vector. These weights serve as
#' robust measures of tail heaviness, providing insights into the distribution's
#' behavior in the left and right tails, respectively.
#'
#' @details
#' The quantile weights, comprising the left quantile weight (LQW) and the right
#' quantile weight (RQW), are robust measures of tail weight in probability
#' distributions. They have a breakdown value of 12.5%, meaning that they are
#' resistant to the influence of up to 12.5% of outliers or contaminated data.
#'
#' The concept of quantile weights is derived from quartile skewness, introduced
#' by D.V. Hinkley in 1975. Quartile skewness measures the skewness or asymmetry
#' of a distribution by comparing the differences between the quartiles, which
#' are robust measures of location and scale.
#'
#' Specifically, the quantile weights are obtained by applying quartile
#' skewness to either the left half or the right half of the probability mass,
#' divided at the median of the univariate distribution (Brys *et al.* 2006):
#' \deqn{LQW(p) = -\frac{Q((1-p)/2) + Q(p/2) - 2Q(0.25)}{Q((1-p)/2) - Q(p/2)}}
#' \deqn{RQW(q) = \frac{Q((1+q)/2) + Q(1-q/2) - 2Q(0.75)}{Q((1+q)/2) - Q(1-q/2)}}
#' where \eqn{Q} is the quantile function of the data. The defaults
#' \eqn{p = 0.125} and \eqn{q = 0.875} give a breakdown value of 12.5%.
#'
#' Interpretation of Quantile Weights:
#'  - At the normal distribution, LQW = RQW ≈ 0.2.
#'  - Larger values indicate heavier tails than the normal distribution, and
#'    smaller values lighter tails.
#'
#' @references
#'  - Brys, G., Hubert, M., and Struyf, A. (2006). Robust measures of tail weight.
#'    Computational Statistics & Data Analysis, 50(3):733-759
#'  - Hinkley, D.V., (1975). On power transformations to symmetry.
#'    Biometrika, 62(1):101–111.
#'
#' @author Christian L. Goueguel
#'
#' @param x A numeric vector.
#' @param p A numeric value between 0 and 0.5 (`p = 0.125` by default).
#' @param q A numeric value between 0.5 and 1 (`q = 0.875` by default).
#' @param drop.na Logical value indicating whether to remove missing values (NA). If `FALSE` (default) and `x` contains missing values, `NA` is returned.
#'
#' @return A tibble with two numeric columns:
#'  - `LQW`: Left quantile weight.
#'  - `RQW`: Right quantile weight.
#'
#' @examples
#' vec <- c(-100, 0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100)
#' # non-robust approach
#' moments::kurtosis(vec)
#'
#' # robust approach
#' quantile_weight(vec)
#'
#' @export quantile_weight
#'
quantile_weight <- function(x, p = 0.125, q = 0.875, drop.na = FALSE) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x)) {
    stop("The input 'x' must be a numeric vector.")
  }
  if (!is.logical(drop.na)) {
    stop("The input 'drop.na' must be a logical value (TRUE or FALSE).")
  }
  check_number(p, "p", lower = 0, upper = 0.5, lower_open = TRUE, upper_open = TRUE)
  check_number(q, "q", lower = 0.5, upper = 1, lower_open = TRUE, upper_open = TRUE)

  if (drop.na) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(tibble::tibble(LQW = NA_real_, RQW = NA_real_))
  }

  Q <- function(prob) unname(stats::quantile(x, prob, type = 7))

  ql1 <- Q((1 - p) / 2)
  ql2 <- Q(p / 2)
  qr1 <- Q((1 + q) / 2)
  qr2 <- Q(1 - q / 2)

  w_tbl <- tibble::tibble(
    LQW = -(ql1 + ql2 - 2 * Q(0.25)) / (ql1 - ql2),
    RQW = (qr1 + qr2 - 2 * Q(0.75)) / (qr1 - qr2)
  )

  return(w_tbl)
}
