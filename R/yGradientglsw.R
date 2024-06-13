#' @title y-Gradient Generalized Least Squares Weighting
#'
#' @description
#' The y-gradient generalized least squares weighting algorithm (GLSW) removes
#' variance from the data (spectra), which is orthogonal to the response.
#'
#' @details
#' The y-Gradient GLSW is an alternative method to GLSW, where a continuous \eqn{\textbf{y}}-variable
#' is used to develop pseudo-groupings of samples in \eqn{\textbf{X}} by comparing
#' the differences in the corresponding \eqn{\textbf{y}} values. This is referred to as the *"gradient method"*
#' because it utilizes a gradient of the sorted \eqn{\textbf{X}}- and
#' \eqn{\textbf{y}}-blocks to calculate a covariance matrix.
#'
#' @param x A numeric matrix, data frame or tibble, representing the predictors data.
#' @param y A numeric vector representing the response vector.
#' @param alpha A numeric value specifying the weighting parameter. Typical values range from 1 to 0.0001. Default is 0.01.
#'
#' @return A tibble containing the filtering matrix.
#'
#' @references
#'  - Zorzetti, B.M., Shaver, J.M., Harynuk, J.J., (2011).
#'    Estimation of the age of a weathered mixture of volatile organic compounds.
#'    Analytica Chimica Acta, 694(1-2):31–37.
#'
#' @export yGradientglsw
#'
yGradientglsw <- function(x, y, alpha = 0.01) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (missing(y)) {
    stop("Missing 'y' argument.")
  }
  if (is.vector(y)) {
    if (!is.numeric(y) || length(y) != nrow(x)) {
      stop("'y' must be a numeric vector with the same length as the number of rows in 'x'.")
    }
  } else {
    stop("'y' must be a numeric vector.")
  }
  if (alpha <= 0) {
    stop("'alpha' must be a positive value.")
  }

  x <- as.matrix(x)
  sorted_idx <- order(y)
  x_sorted <- x[sorted_idx, ]
  y_sorted <- y[sorted_idx, ]
  x_diff <- prospectr::savitzkyGolay(x_sorted, m = 1, p = 1, w = 5)
  y_diff <- prospectr::savitzkyGolay(y_sorted, m = 1, p = 1, w = 5)
  w_i <- 2^(-y_diff / stats::sd(y_diff))
  G <- yGradientglswCpp(x_diff, w_i, alpha)
  return(G)
}
