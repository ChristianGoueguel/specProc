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
#' Specifically, the quantile weights are calculated when applying quartile
#' skewness to either the left half or the right half of the probability mass,
#' divided at the median of the univariate distribution. The left quantile weight
#' (LQW) is the proportion of the data below the median, divided by the expected
#' proportion (0.5) if the data were normally distributed. The right quantile
#' weight (RQW) is the proportion of the data above the median, divided by 0.5.
#'
#' Interpretation of Quantile Weights:
#'  - Values closer to 0 indicate lighter tails compared to the normal distribution.
#'  - Values closer to 1 signify heavier tails compared to the normal distribution.
#'  - Values significantly greater than 1 suggest the presence of outliers or
#'    extreme values in the respective tail.
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
#' @param p A numeric value between 0 and 0.5 (`p = 0.25` by default).
#' @param q A numeric value between 0.5 and 1 (`q = 0.75` by default).
#' @param drop.na Logical value indicating whether to remove missing values (NA) or not.
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
quantile_weight <- function(x, p = 0.25, q = 0.75, drop.na = FALSE) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x)) {
    stop("The input 'x' must be a numeric vector.")
  }
  if (!is.logical(drop.na)) {
    stop("The input 'drop.na' must be a logical value (TRUE or FALSE).")
  }

  value <- NULL
  below_med <- NULL
  above_med <- NULL
  med <- stats::median(x, na.rm = drop.na)

  left <- tibble::enframe(x) %>%
    dplyr::mutate(below_med = value <= med) %>%
    dplyr::filter(below_med) %>%
    dplyr::pull(value)

  right <- tibble::enframe(x) %>%
    dplyr::mutate(above_med = value >= med) %>%
    dplyr::filter(above_med) %>%
    dplyr::pull(value)

  ql1 <- stats::quantile(left, (1 - p)/2)
  ql2 <- stats::quantile(left, p/2)

  qr1 <- stats::quantile(right, (1 + q)/2)
  qr2 <- stats::quantile(right, 1 - (q/2))

  w_tbl <- tibble::tibble(
    LQW = (-1) * (ql1 + ql2 - 2 * stats::quantile(left, 0.25)) / (ql1 - ql2),
    RQW = (qr1 + qr2 - 2 * stats::quantile(right, 0.75)) / (qr1 - qr2)
  )

  return(w_tbl)
}
