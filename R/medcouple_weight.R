#' @title Medcouple Tail Weight Measure
#'
#' @description
#' This function calculates the left medcouple (LMC) and the right medcouple (RMC) for a given numeric vector.
#'
#' @details
#' The medcouple weights are calculated by splitting the data into two parts: values below
#' the median (left) and values above the median (right). The medcouple weights are then
#' computed as a robust measure of heavy-tailed distributions, where traditional
#' measure of kurtosis may be distorted by outliers.
#'
#' @references
#'  - Brys, G., Hubert, M., and Struyf, A. (2006). Robust measures of tail weight.
#'    Computational Statistics & Data Analysis, 50(3):733-759
#'  - Brys, G., Hubert, M., and Struyf, A. (2004). A robust measure of skewness.
#'    Journal of Computational and Graphical Statistics, 13(4):996-1017
#'
#' @author Christian L. Goueguel
#' @param x A numeric vector.
#' @param drop.na Logical value indicating whether to remove missing values (NA) or not.
#' @return A tibble with two numeric columns:
#'  - `LMC`: Left medcouple.
#'  - `RMC`: Right medcouple.
#' @examples
#' x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100)
#' medcouple_weight(x)
#' @export medcouple_weight
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
