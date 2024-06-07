#' @title Medcouple Tail Weight Measure
#'
#' @description
#' This function calculates the left medcouple (LMC) and the right medcouple
#' (RMC) for a given numeric vector. The left and right medcouples provide a
#' robust measure of the distribution's tail behavior in the presence of outliers
#' or contaminated data.
#'
#' @details
#' The medcouple is a robust measure of skewness based on the median and the
#' medcouple (Brys *et al.* 2004), which is a kernel estimator of the cumulative
#' distribution function (CDF). It is calculated by comparing the median
#' to a robust measure of location within each half of the distribution,
#' divided at the median.
#'
#' Specifically, the left medcouple (LMC) measures the skewness in the lower tail
#' of the distribution, while the right medcouple (RMC) measures the skewness in
#' the upper tail. The left and right medcouples are robust to outliers and have
#' a breakdown value of 25%.
#'
#' The interpretation of LMC and RMC is as follows:
#'  - Values close to 0 indicate a symmetric distribution or light tails.
#'  - Positive values indicate right-skewness or a heavier right tail.
#'  - Negative values indicate left-skewness or a heavier left tail.
#'
#' @references
#'  - Brys, G., Hubert, M., and Struyf, A. (2006).
#'    Robust measures of tail weight.
#'    Computational Statistics & Data Analysis, 50(3):733-759
#'  - Brys, G., Hubert, M., and Struyf, A. (2004).
#'    A robust measure of skewness.
#'    Journal of Computational and Graphical Statistics, 13(4):996-1017
#'
#' @author Christian L. Goueguel
#' @param x A numeric vector.
#' @param drop.na Logical value indicating whether to remove missing values (NA) or not.
#'
#' @return A tibble with two numeric columns:
#'  - `LMC`: Left medcouple.
#'  - `RMC`: Right medcouple.
#'
#' @examples
#' vec <- c(-100, 0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100)
#' # non-robust approach
#' moments::kurtosis(vec)
#'
#' # robust approach
#' medcouple_weight(vec)
#'
#' @export medcouple_weight
#'
medcouple_weight <- function(x, drop.na = FALSE) {
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

  w_tbl <- tibble::tibble(
    LMC = (-1) * robustbase::mc(left, na.rm = drop.na),
    RMC = robustbase::mc(right, na.rm = drop.na)
  )

  return(w_tbl)
}
