#' @title Quantile Tail Weight Measure
#'
#' @description
#' The function calculates the left quantile weight (LQW) and the right quantile
#' weight (RQW) for a given numeric vector. Values closer to 0 indicate lighter
#' tails, while values closer to 1 signify heavier tails.
#'
#' @details
#' The quantile weights are a robust measure of tail weight in probability
#' distributions (breakdown value of 12.5%). They are derived from the concept of quartile skewness,
#' introduced by D.V. Hinkley in 1975. Quartile skewness measures the skewness
#' or asymmetry of a distribution by comparing the differences between the quartiles.
#' Specifically, the quantile weights are calculated when applying quartile skewness
#' to either the left half or the right half of the probability mass, divided at
#' the median of the univariate distribution.
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
#' @return A tibble with two numeric columns:
#'  - `LQW`: Left quantile weight.
#'  - `RQW`: Right quantile weight.
#'
#' @examples
#' vec <- c(-100, 0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100)
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
