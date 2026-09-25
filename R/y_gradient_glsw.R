#' @title y-Gradient Generalized Least Squares Weighting
#'
#' @author Christian L. Goueguel
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
#' The samples are sorted by increasing \eqn{\textbf{y}}, and the first
#' derivatives of \eqn{\textbf{X}} and \eqn{\textbf{y}} along the sample axis are
#' computed with a Savitzky-Golay filter. Samples whose neighbours have similar
#' \eqn{\textbf{y}} values receive large weights
#' \eqn{w_i = 2^{-\Delta y_i / s_{\Delta y}}}, so the differences between their
#' spectra, \eqn{\Delta\textbf{X}}, describe variation unrelated to \eqn{\textbf{y}}.
#' The filter is then built as in [glsw()] from
#' \eqn{\textbf{C} = \Delta\textbf{X}^T\textbf{W}^2\Delta\textbf{X}}.
#'
#' @param x A numeric matrix, data frame or tibble, representing the predictors data.
#' @param y A numeric vector representing the response vector.
#' @param alpha A positive numeric value specifying the weighting parameter. Typical values range from 1 to 0.0001. Default is 0.01.
#' @param window An odd integer giving the width of the Savitzky-Golay window used to compute the gradients. Default is 5.
#'
#' @return A tibble containing the \eqn{p \times p} filtering matrix.
#'
#' @references
#'  - Zorzetti, B.M., Shaver, J.M., Harynuk, J.J., (2011).
#'    Estimation of the age of a weathered mixture of volatile organic compounds.
#'    Analytica Chimica Acta, 694(1-2):31–37.
#'
#' @export y_gradient_glsw
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(30 * 20), 30, 20)
#' y <- x[, 1] + rnorm(30, sd = 0.1)
#' G <- y_gradient_glsw(x, y, alpha = 0.01)
#' x_filtered <- x %*% as.matrix(G)
#'
y_gradient_glsw <- function(x, y, alpha = 0.01, window = 5) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (missing(y)) {
    stop("Missing 'y' argument.")
  }
  x <- as_numeric_matrix(x, "x")
  if (is.data.frame(y) && ncol(y) == 1) {
    y <- y[[1]]
  }
  if (!is.numeric(y) || !is.null(dim(y)) && NCOL(y) != 1) {
    stop("'y' must be a numeric vector.")
  }
  y <- as.vector(y)
  if (length(y) != nrow(x)) {
    stop("'y' must be a numeric vector with the same length as the number of rows in 'x'.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0) {
    stop("'alpha' must be a positive value.")
  }
  check_count(window, "window", lower = 3)
  if (window %% 2 == 0) {
    stop("'window' must be an odd integer.")
  }
  if (nrow(x) < window + 1) {
    stop("'x' must have more rows than 'window'.")
  }
  if (anyNA(x) || anyNA(y)) {
    stop("'x' and 'y' cannot contain missing values.")
  }

  sorted_idx <- order(y)
  x_sorted <- x[sorted_idx, , drop = FALSE]
  y_sorted <- y[sorted_idx]

  # Derivatives along the sample axis: prospectr filters the rows, so the
  # sample-by-variable matrix is transposed before and after filtering.
  x_diff <- t(prospectr::savitzkyGolay(t(x_sorted), m = 1, p = 2, w = window))
  y_diff <- drop(prospectr::savitzkyGolay(matrix(y_sorted, nrow = 1), m = 1, p = 2, w = window))

  s <- stats::sd(y_diff)
  w_i <- if (is.finite(s) && s > 0) 2^(-abs(y_diff) / s) else rep(1, length(y_diff))

  G <- yGradientglswCpp(unname(x_diff), w_i, alpha)
  return(as_tbl(G, colnames(x)))
}
