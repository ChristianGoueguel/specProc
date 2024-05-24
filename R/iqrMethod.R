#' Interquartile Range Method
#'
#' This function identifies potential outliers in a numeric vector using the
#' interquartile range (IQR) method.
#'
#' This method is robust to the presence of outliers and is suitable for skewed distributions.
#' Any data points that fall outside the range defined by the lower (Q1 - k × IQR) and upper (Q3 + k × IQR) fences is considered an outlier, where Q1 and Q3 are the 25th and 75th percentiles, respectively.
#' The fence factor, k, can be adjusted to make the method more or less robust (often 1.5 or 3).
#'
#'  - Everitt, B. S.; Skrondal, A. (2010), The Cambridge Dictionary of Statistics, Cambridge University Press.
#'  - Tukey, J. (1977). Exploratory Data Analysis, Addison-Wesley
#'
#' @author Christian L. Goueguel
#' @param x A numeric vector.
#' @param k A scalar value specifying the fence factor. `k = 1.5` is the default and is often considered a conservative choice for identifying outliers.
#' `k = 3` is more lenient and is sometimes used when a higher tolerance for outliers is desired.
#' @return A tibble with two columns:
#'   - `data`: The original numeric values.
#'   - `outlier`: A logical vector indicating whether each value is an outlier or not.
#' @examples
#' iqrMethod(c(1, 2, 3, 4, 5, 10))
#' iqrMethod(c(1, 2, 3, 4, 5, 10), k = 3)
#' @export iqrMethod
iqrMethod <- function(x, k = 1.5) {
  if (!is.numeric(x)) {
    stop("The input 'x' must be a numeric vector.")
  }
  if (!is.numeric(k) || k <= 0) {
    stop("The input 'k' must be a positive numeric scalar.")
  }
  q1 <- stats::quantile(x, 0.25)
  q3 <- stats::quantile(x, 0.75)
  iqr <- q3 - q1
  lower_fence <- q1 - k * iqr
  upper_fence <- q3 + k * iqr

  value <- NULL
  outlier <- NULL
  name <- NULL
  x_tbl <- tibble::enframe(x)
  x_tbl <- x_tbl %>%
    dplyr::mutate(outlier = value < lower_fence | value > upper_fence) %>%
    dplyr::select(-name) %>%
    dplyr::rename(data = value) %>%
    dplyr::arrange(dplyr::desc(outlier))

  return(x_tbl)
}
