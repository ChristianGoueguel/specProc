#' Classical or Robust Z-Score
#'
#' This function is a wrapper around the `robustHD` package and calculates classical or robust z-score (standardization) for a numeric vector.
#'
#' Z-scores are useful for comparing data points from different distributions because they are
#' dimensionless and standardized. A positive z-score indicates that the data point is above the
#' mean (or the median in the robust approach), while a negative z-score indicates that the data point is below the mean (or the median). One common rule
#' to detect outliers using z-scores is the "three-sigma rule", in which data points with an absolute
#' z-score greater than 3 (|z| > 3) can be considered potential outliers, as they fall outside the range
#' that covers 99.7% of the data points in a normal distribution. (Note that a cutoff of |z| > 2.5 is also often used).
#'
#' - Rousseeuw, P. J., and Croux, C. (1993). Alternatives to the median absolute deviation.
#'   Journal of the American Statistical Association, 88(424), 1273-1283.
#' - Rousseeuw, P. J., and Hubert, M. (2011). Robust statistics for outlier detection.
#'   Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 1(1), 73-79.
#'
#' @author Christian L. Goueguel
#' @param x A numeric vector.
#' @param robust A logical value indicating whether to calculate classical or robust (default) z-score.
#' @return A tibble with two columns:
#'   - `data`: The original numeric values.
#'   - `score`: The calculated z-scores.
#' @examples
#' vec <- c(1, 2, 3, 4, 5, 100)
#' zscore(vec)
#' zscore(vec, robust = FALSE)
#' @export zscore
zscore <- function(x, robust = TRUE) {
  if (!is.numeric(x)) {
    stop("The input 'x' must be a numeric vector.")
  }
  if (!is.logical(robust)) {
    stop("The input 'robust' must be a logical value (TRUE or FALSE).")
  }
  requireNamespace("robustHD", quietly = TRUE)

  value <- NULL
  score <- NULL
  name <- NULL
  x <- tibble::enframe(x)
  if (robust) {
    z <- dplyr::mutate(x, score = robustHD::robStandardize(value))
  } else {
    z <- dplyr::mutate(x, score = robustHD::standardize(value))
  }

  z <- z %>%
    dplyr::select(-name) %>%
    dplyr::rename(data = value) %>%
    dplyr::arrange(dplyr::desc(score))

  return(z)
}