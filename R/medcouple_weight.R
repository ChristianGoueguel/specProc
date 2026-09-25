#' @title Medcouple Tail Weight Measure
#'
#' @description
#' This function calculates the left medcouple (LMC) and the right medcouple
#' (RMC) for a given numeric vector. The left and right medcouples provide a
#' robust measure of the distribution's tail behavior in the presence of outliers
#' or contaminated data.
#'
#' @details
#' The left and right medcouples are a robust measure of tail weight based on the
#' median and the medcouple (Brys *et al.* 2004): the left medcouple is minus the
#' medcouple of the observations below the median, and the right medcouple is the
#' medcouple of the observations above the median. The left and right medcouples are
#' robust to outliers and have a breakdown value of 25%. Specifically, the left
#' medcouple (LMC) measures the skewness in the lower tail
#' of the distribution, while the right medcouple (RMC) measures the skewness in
#' the upper tail.
#'
#' The interpretation of LMC and RMC is as follows:
#'  - At the normal distribution, LMC = RMC ≈ 0.2.
#'  - Larger values indicate heavier tails than the normal distribution, and
#'    smaller values lighter tails.
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
#' @param drop.na Logical value indicating whether to remove missing values (NA). If `FALSE` (default) and `x` contains missing values, `NA` is returned.
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

  if (drop.na) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(tibble::tibble(LMC = NA_real_, RMC = NA_real_))
  }

  med <- stats::median(x)
  left <- x[x < med]
  right <- x[x > med]
  if (length(left) < 2 || length(right) < 2) {
    stop("'x' must have at least two values on each side of the median.")
  }

  w_tbl <- tibble::tibble(
    LMC = -medcouple(left),
    RMC = medcouple(right)
  )

  return(w_tbl)
}
