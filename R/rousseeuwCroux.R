#' @title Rousseeuw-Croux Scale Estimators
#'
#' @description
#' This function calculates either the Rousseeuw-Croux Sn or Qn scale estimator.
#'
#' @details
#' The Sn and Qn estimators, proposed by Rousseeuw and Croux (1993), are robust measures of
#' scale (or dispersion) that are designed to have a high breakdown point, which is a desirable
#' property for estimators in the presence of outliers or heavy-tailed distributions. Specifically,
#' both estimators have a breakdown point of 50%. The Sn estimator is based on
#' the median, while the Qn estimator is based on a weighted combination of order statistics.
#' Both estimators are consistent estimators of the population scale parameter
#' under normality. The function uses the `robustbase` package for estimating Sn and
#' Qn, whilst the bias-correction factors used in the calculations have been refined
#' according to Akinshin, A., (2022).
#'
#' @param x A numeric vector of data values.
#' @param estimator A character string indicating whether to calculate the "Sn" or "Qn" estimator.
#' @param drop.na A logical value indicating whether to remove missing values (`NA`) from the input vector.
#' @return A numeric value representing the calculated Sn or Qn scale estimator.
#'
#' @author Christian L. Goueguel
#'
#' @references
#'  - Rousseeuw, P.J. and Croux, C. (1993). Alternatives to the median absolute deviation.
#'    Journal of the American Statistical Association, 88(424):1273-1283.
#'  - Akinshin, A., (2022). Finite-sample Rousseeuw-Croux scale estimators.
#'    arxiv:2209.12268v1.
#'
#' @examples
#' # Example 1:
#' x <- c(seq(1,100))
#' tibble::tibble(
#' sd = stats::sd(x),
#' mad = stats::mad(x),
#' Sn = rousseeuwCroux(x, estimator = "Sn"),
#' Qn = rousseeuwCroux(x, estimator = "Qn")
#' )
#'
#' # Example 2:
#' x <- c(seq(1,99), 1e3) # An outlier at 1000
#' tibble::tibble(
#' sd = stats::sd(x),
#' mad = stats::mad(x),
#' Sn = rousseeuwCroux(x, estimator = "Sn"),
#' Qn = rousseeuwCroux(x, estimator = "Qn")
#' )
#' @export rousseeuwCroux
rousseeuwCroux <- function(x, estimator = c("Sn", "Qn"), drop.na = FALSE) {
  if (missing(x)) {
    stop("Input 'x' must be provided.")
  }
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (length(unique(x)) == 1) {
    stop("'x' cannot be a constant vector.")
  }
  if (length(x) < 2) {
    stop("'x' must have at least two elements.")
  } else {
    n <- length(x)
  }

  estimator <- match.arg(estimator)

  if (drop.na != FALSE) {
    x <- x[!is.na(x)]
  }

  if (estimator == "Sn") {
    factors <- c(NA,
      0.7430, 1.8498, 0.9551, 1.3486, 0.9941, 1.1983, 1.0050, 1.1318,
      1.0069, 1.0959, 1.0063, 1.0742, 1.0051, 1.0601, 1.0038, 1.0501,
      1.0028, 1.0430, 1.0022, 1.0374, 1.0014, 1.0331, 1.0009, 1.0297,
      1.0007, 1.0269, 1.0004, 1.0245, 1.0001, 1.0226, 0.9999, 1.0209,
      0.9997, 1.0195, 0.9998, 1.0183, 0.9996, 1.0172, 0.9997, 1.0162,
      0.9996, 1.0154, 0.9996, 1.0146, 0.9996, 1.0139, 0.9995, 1.0132,
      0.9995, 1.0126, 0.9995, 1.0123, 0.9995, 1.0117, 0.9995, 1.0113,
      0.9996, 1.0109, 0.9996, 1.0105, 0.9995, 1.0102, 0.9996, 1.0099,
      0.9997, 1.0095, 0.9996, 1.0092, 0.9997, 1.0090, 0.9997, 1.0088,
      0.9996, 1.0085, 0.9997, 1.0084, 0.9997, 1.0081, 0.9997, 1.0079,
      0.9997, 1.0076, 0.9997, 1.0076, 0.9997, 1.0074, 0.9997, 1.0072,
      0.9997, 1.0070, 0.9997, 1.0069, 0.9997, 1.0067, 0.9998, 1.0066,
      0.9997, 1.0065, 0.9998
      )
    predict_factor <- function(n) {
      n_odd <- 1 + 0.707 * n^(-1) - 7.181 * n^(-2)
      n_even <- 1 + 0.043 * n^(-1) - 6.288 * n^(-2)
      ifelse(n %% 2 == 1, n_odd, n_even)
    }
    factor <- if (n <= 100) factors[n] else predict_factor(n)
    res <- robustbase::Sn(x, constant = 1.1926 * factor)
  } else {
    factors <- c(NA,
      0.3995, 0.9939, 0.5133, 0.8441, 0.6122, 0.8589, 0.6700, 0.8736,
      0.7201, 0.8890, 0.7575, 0.9023, 0.7855, 0.9125, 0.8078, 0.9211,
      0.8260, 0.9279, 0.8410, 0.9338, 0.8537, 0.9389, 0.8644, 0.9430,
      0.8737, 0.9468, 0.8819, 0.9501, 0.8890, 0.9530, 0.8953, 0.9557,
      0.9010, 0.9579, 0.9060, 0.9600, 0.9106, 0.9619, 0.9148, 0.9636,
      0.9185, 0.9652, 0.9220, 0.9667, 0.9252, 0.9680, 0.9281, 0.9692,
      0.9309, 0.9704, 0.9333, 0.9715, 0.9357, 0.9724, 0.9378, 0.9733,
      0.9399, 0.9742, 0.9418, 0.9750, 0.9435, 0.9757, 0.9453, 0.9765,
      0.9469, 0.9771, 0.9484, 0.9777, 0.9498, 0.9784, 0.9511, 0.9789,
      0.9523, 0.9794, 0.9536, 0.9800, 0.9547, 0.9805, 0.9558, 0.9809,
      0.9568, 0.9814, 0.9578, 0.9818, 0.9587, 0.9822, 0.9597, 0.9826,
      0.9605, 0.9829, 0.9614, 0.9833, 0.9621, 0.9836, 0.9629, 0.9840,
      0.9636, 0.9843, 0.9644
    )
    predict_factor <- function(n) {
      n_odd <- 1 - 1.594 * n^(-1) + 3.220 * n^(-2)
      n_even <- 1 - 3.672 * n^(-1) + 11.087 * n^(-2)
      ifelse(n %% 2 == 1, n_odd, n_even)
    }
    factor <- if (n <= 100) factors[n] else predict_factor(n)
    res <- robustbase::Qn(x, constant = 2.2191 * factor)
  }
  return(res)
}
