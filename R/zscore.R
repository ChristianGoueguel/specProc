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
#' z-score greater than 3 (|z| > 3) can be considered potential outliers (default), as they fall outside the range
#' that covers 99.7% of the data points in a normal distribution. (Note that a cutoff of |z| > 2.5 is also often used).
#'
#' @references
#' - Rousseeuw, P. J., and Croux, C. (1993).
#'   Alternatives to the median absolute deviation.
#'   Journal of the American Statistical Association, 88(424), 1273-1283.
#' - Rousseeuw, P. J., and Hubert, M. (2011).
#'   Robust statistics for outlier detection.
#'   Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 1(1), 73-79.
#' - Donoho, D., (1982).
#'   Breakdown properties of multivariate location estimators.
#'   Ph.D. Qualifying paper, Dept. Statistics, Harvard University, Boston.
#' - Stahel, W., (1981).
#'   Robuste Schätzungen: infinitesimale Optimalität und Schätzungen von Kovarianzmatrizen.
#'   PhD thesis, ETH Zürich.
#'
#' @author Christian L. Goueguel
#'
#' @param x A numeric vector.
#' @param robust A logical value indicating whether to calculate classical or robust z-score. If `FALSE` (the default), uses the classical approach. If `TRUE`, computes the robust method, i.e. the so-called Stahel-Donoho outlyingness.
#' @param drop_na A logical value indicating whether to remove missing values (\code{NA}) from the calculations. If \code{TRUE}, missing values will be removed. If \code{FALSE} (the default), missing values are kept in the output (with a missing score) and ignored when computing the location and scale.
#' @param cutoff A numeric value indicating the threshold above which data points are identified and flagged as potential outliers. By default, `cutoff = 3`.
#' @param drop.na `r lifecycle::badge("deprecated")` Use `drop_na`.
#'
#' @return A tibble with three columns, sorted by decreasing score:
#'   - `data`: The original numeric values.
#'   - `score`: The calculated z-scores.
#'   - `flag`: `TRUE` if the corresponding data point is flagged as a potential outlier, and `FALSE` otherwise.
#'
#' @seealso [iqr_outliers()], [directional_outlyingness()] for skewed data.
#'
#' @examples
#' # 90 regular values and a cluster of 10 outliers near 62
#' set.seed(1)
#' x <- c(rnorm(90, mean = 50, sd = 2), rnorm(10, mean = 62, sd = 0.5))
#' group <- rep(c("regular", "outlier"), c(90, 10))
#' ggplot2::ggplot(data.frame(x, group), ggplot2::aes(x)) +
#'   ggplot2::geom_density(fill = "#2166ac", alpha = 0.3) +
#'   ggplot2::geom_rug(ggplot2::aes(colour = group)) +
#'   ggplot2::scale_colour_manual(values = c(outlier = "#b2182b", regular = "#2166ac"))
#'
#' # classical: the cluster inflates the standard deviation, which hides it
#' # (masking): the outliers score below 3 and none is flagged
#' head(zscore(x), 11)
#' # robust: the median and the MAD hardly move, and the 10 outliers are
#' # flagged
#' head(zscore(x, robust = TRUE), 11)
#'
#' data(forageLIBS)
#' # iron contents (mg/kg): a few samples are far above the others
#' fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
#' head(zscore(fe))
#' head(zscore(fe, robust = TRUE))
#'
#' @export zscore
#'
zscore <- function(x, cutoff = 3, robust = FALSE, drop_na = FALSE,
                   drop.na = deprecated()) {
  drop_na <- renamed_arg(drop.na, drop_na, "zscore", "drop.na", "drop_na")
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  check_numeric_vector(x, "zscore")
  if (!is.logical(robust)) {
    stop("The input 'robust' must be a logical value (TRUE or FALSE).")
  }
  check_number(cutoff, "cutoff", lower = 0, lower_open = TRUE)
  check_flag(drop_na, "drop_na")

  if (drop_na) {
    x <- x[!is.na(x)]
  }

  value <- NULL
  score <- NULL
  name <- NULL
  x <- tibble::enframe(x)

  if (robust) {
    z <- dplyr::mutate(x, score = standardize(value, loc.fun = stats::median, scale.fun = stats::mad))
  } else {
    z <- dplyr::mutate(x, score = standardize(value, loc.fun = mean, scale.fun = stats::sd))
  }

  z <- z %>%
    dplyr::select(-name) %>%
    dplyr::rename(data = value) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::mutate(flag = abs(score) > cutoff)

  return(z)
}


standardize <- function(x, loc.fun, scale.fun) {
  c <- loc.fun(x, na.rm = TRUE)
  s <- scale.fun(x, na.rm = TRUE)
  res <- (x - c) / s
  return(res)
}
