#' Interquartile Range Method
#'
#' This function identifies potential outliers in a numeric vector using the
#' interquartile range (IQR) method.
#'
#' For symmetric distributions, observations that fall outside the range defined
#' by the lower fence (Q1 - k × IQR) and upper fence (Q3 + k × IQR) are considered as
#' potential outliers, where Q1 and Q3 are the 25th and 75th percentiles, respectively.
#' The fence factor, k, can be adjusted to make the method more or less robust
#' (often 1.5 or 3). Optionally, the method can account for skewness in data
#' distributions by incorporating the medcouple. In such case, the lower and
#' upper fences are expressed as functions of the medcouple, adjusting the
#' fences asymmetrically to better accommodate skewed distributions.
#' Note that the implemented method does not explicitly account for tail heaviness.
#' While the medcouple can provide some robustness against heavy tails, the method
#' may still struggle to accurately identify potential outliers in distributions with extreme
#' kurtosis or long-tailed behavior.
#'
#'  - Everitt, B. S., and Skrondal, A. (2010). The Cambridge Dictionary of Statistics.
#'    Cambridge University Press.
#'  - Tukey, J. (1977). Exploratory Data Analysis.
#'    Addison-Wesley
#'  - Hubert M., and Vandervieren E. (2008). An Adjusted Boxplot for Skewed Distributions.
#'    Computational Statistics & Data Analysis, 52(12):5186-5201
#'
#' @author Christian L. Goueguel
#' @param x A numeric vector.
#' @param k A numeric value specifying the fence factor. `k = 1.5` is the default
#' and is often considered a conservative choice for identifying outliers.
#' `k = 3` is more lenient and is sometimes used when a higher tolerance for
#' outliers is desired.
#' @param skew A logical value indicating whether to calculate the version of
#' the fences that accounts for skewness in the underlying data distribution.
#' By default, `skew = FALSE`, which calculates the fences assuming a symmetric distribution.
#' However, if `skew = TRUE`, the formulas used to calculate the lower and upper fences incorporate the medcouple,
#' to account for potential asymmetry in the underlying data distribution.
#' These formulas are explicitly derived and optimized for the scenario where `k = 1.5` (Hubert and Vandervieren, 2008).
#' Consequently, if the user attempts to use a value of `k` other than 1.5, the code will issue a warning message indicating that the formula is only defined for `k = 1.5`.
#' In such cases, the code will automatically reset `k` to 1.5 and proceed with the calculations using the appropriate formulas and constants.
#' @return A tibble with two columns:
#'   - `data`: The original numeric values.
#'   - `outlier`: A logical vector indicating whether each value is a potential outlier or not.
#' @examples
#' iqrMethod(c(1, 2, 3, 4, 5, 10))
#' iqrMethod(c(1, 2, 3, 4, 5, 10), k = 3)
#' @export iqrMethod
iqrMethod <- function(x, k = 1.5, skew = FALSE) {
  if (!is.numeric(x)) {
    stop("The input 'x' must be a numeric vector.")
  }
  if (!is.numeric(k) || k <= 0) {
    stop("The input 'k' must be a positive numeric scalar.")
  }
  if (!is.logical(skew)) {
    stop("'skew' must be of type boolean (TRUE or FALSE)")
  }
  q1 <- stats::quantile(x, 0.25)
  q3 <- stats::quantile(x, 0.75)
  iqr <- q3 - q1

  value <- NULL
  outlier <- NULL
  name <- NULL
  x_tbl <- tibble::enframe(x)

  if (skew == FALSE) {
    lower_fence <- q1 - k * iqr
    upper_fence <- q3 + k * iqr
  } else {
    medcouple <- robustbase::mc(x)
    alpha <- dplyr::if_else(medcouple >= 0, 4, 3)
    beta <- dplyr::if_else(medcouple >= 0, 3, 4)

    if (k != 1.5) {
      cat("Warning: The formula is only defined for k = 1.5. Resetting k to 1.5.\n")
      k <- 1.5
    }

    lower_fence <- q1 - k * exp(-alpha * medcouple) * iqr
    upper_fence <- q3 + k * exp(beta * medcouple) * iqr
  }

  x_tbl <- x_tbl %>%
    dplyr::mutate(outlier = value < lower_fence | value > upper_fence) %>%
    dplyr::select(-name) %>%
    dplyr::rename(data = value) %>%
    dplyr::arrange(dplyr::desc(outlier))

  return(x_tbl)
}
