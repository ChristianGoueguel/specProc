#' @title Y-Gradient Generalized Least Squares Weighting
#'
#' @description
#' This function implements the Generalized Least Squares Weighting (GLSW) algorithm
#' with a specific approach for filtering out X (input data) variance orthogonal to
#' a Y (response vector). The function calculates the difference matrix X_diff by
#' first sorting the rows of X and Y in order of increasing Y value, and then
#' calculating the first-order Savitzky-Golay derivative of each column of X and Y.
#' The rows of X_diff are then re-weighted based on the differences in Y values.
#'
#' @details
#' The filtering matrix G can be used to down-weight correlations in the original
#' covariance matrix, effectively reducing the impact of interferences on the data.
#'
#' @param X A numeric matrix representing the input data.
#' @param Y A numeric vector representing the response vector.
#' @param alpha A numeric value specifying the weighting parameter. Typical values
#'   range from 1 to 0.0001. Default is 0.01.
#'
#' @return A tibble containing the filtering matrix.
#'
#' @references
#' - Geladi, P., & Kowalski, B. R. (1986). Partial least-squares regression: a tutorial.
#'   Analytica Chimica Acta, 185, 1-17.
#' - Martens, H., & Naes, T. (1989). Multivariate calibration. John Wiley & Sons.
#' - Wold, S., Ruhe, A., Wold, H., & Dunn III, W. J. (1984). The collinearity problem in linear regression.
#'   The partial least squares (PLS) approach to generalized inverses. SIAM Journal on Scientific and
#'   Statistical Computing, 5(3), 735-743.
#' - Geladi, P., & Kowalski, B. R. (1986). An example of two-block predictive partial least-squares
#'   regression with simulated data. Analytica Chimica Acta, 185, 19-32.
#' - Eigenvector Research: Advanced Preprocessing: Multivariate Filtering.
#'   https://wiki.eigenvector.com/index.php?title=Advanced_Preprocessing:_Multivariate_Filtering
#'
#' @export yGradient_glsw
yGradient_glsw <- function(X, Y, alpha = 0.01) {
  # Input validation
  if (!is.numeric(X) || !is.matrix(X)) {
    stop("'X' must be a numeric matrix.")
  }
  if (!is.numeric(Y) || length(Y) != nrow(X)) {
    stop("'Y' must be a numeric vector with the same length as the number of rows in 'X'.")
  }
  if (alpha <= 0) {
    stop("'alpha' must be a positive value.")
  }

  # Sort X and Y based on increasing Y values
  sorted_idx <- order(Y)
  X_sorted <- X[sorted_idx, ]
  Y_sorted <- Y[sorted_idx]

  # Calculate first-order Savitzky-Golay derivative
  X_diff <- apply(X_sorted, 2, stats::filter, rep(1/5, 5), sides = 2)
  Y_diff <- stats::filter(Y_sorted, rep(1/5, 5), sides = 2)

  # Calculate re-weighting matrix W
  W <- diag(1 / (Y_diff^2 + 1e-10))  # Add small constant to avoid division by zero

  # Compute covariance matrix and perform SVD
  C <- t(X_diff) %*% W %*% X_diff
  svd_result <- svd(C)
  V <- svd_result$v
  S <- svd_result$d

  # Calculate diagonal matrix D
  D <- sqrt((S^2 / alpha) + diag(ncol(X_diff)))

  # Compute filtering matrix G
  G <- V %*% (1 / D) %*% t(V)

  return(tibble::as_tibble(G))
}
