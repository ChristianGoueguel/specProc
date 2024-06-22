#' @title Classical or Robust Z-Score
#'
#' @description
#' This function calculates classical or robust z-score (standardization) for
#' a numeric vector.
#'
#' @details
#' Z-scores are useful for comparing data points from different distributions because they are
#' dimensionless and standardized. A positive z-score indicates that the data point is above the
#' mean (or the median in the robust approach), while a negative z-score indicates that the data point is below the mean (or the median). One common rule
#' to detect outliers using z-scores is the "three-sigma rule", in which data points with an absolute
#' z-score greater than 3 (|z| > 3) can be considered potential outliers, as they fall outside the range
#' that covers 99.7% of the data points in a normal distribution. (Note that a cutoff of |z| > 2.5 is also often used).
#'
#' @references
#' - Rousseeuw, P. J., and Croux, C. (1993). Alternatives to the median absolute deviation.
#'   Journal of the American Statistical Association, 88(424), 1273-1283.
#' - Rousseeuw, P. J., and Hubert, M. (2011). Robust statistics for outlier detection.
#'   Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 1(1), 73-79.
#'
#' @author Christian L. Goueguel
#'
#' @param x A numeric vector.
#' @param robust A logical value indicating whether to calculate classical or robust (default) z-score.
#' @param drop.na A logical value indicating whether to remove missing values (\code{NA}) from the calculations. If \code{TRUE}, missing values will be removed. If \code{FALSE} (the default), missing values will be included in the calculations.
#'
#' @return A tibble with two columns:
#'   - `data`: The original numeric values.
#'   - `score`: The calculated z-scores.
#'
#' @examples
#' x <- c(1:5, 100)
#' # Non-robust approach
#' zscore(x, robust = FALSE)
#'
#' # Robust approach
#' zscore(x)
#'
#' @export zscore
#'
zscore <- function(x, robust = TRUE, drop.na = FALSE) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x)) {
    stop("The input 'x' must be a numeric vector.")
  }
  if (!is.logical(robust)) {
    stop("The input 'robust' must be a logical value (TRUE or FALSE).")
  }

  if (drop.na) {
    x <- x[!is.na(x)]
  }

  value <- NULL
  score <- NULL
  name <- NULL
  x <- tibble::enframe(x)

  if (robust) {
    z <- dplyr::mutate(x, score = standardize(value, loc.fun = stats::median, scale.fun = stats::mad))
  } else {
    z <- dplyr::mutate(x, score = standardize(value, loc.fun = mean, scale.fun = sd))
  }

  z <- z %>%
    dplyr::select(-name) %>%
    dplyr::rename(data = value) %>%
    dplyr::arrange(dplyr::desc(score))

  return(z)
}


standardize <- function(x, loc.fun, scale.fun) {
  c <- loc.fun(x)
  s <- scale.fun(x)
  res <- (x - c) / s
  return(res)
}







