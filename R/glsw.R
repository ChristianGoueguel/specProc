#' @title Generalized Least Squares Weighting
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the Generalized Least Squares Weighting (GLSW) algorithm
#' proposed by Geladi and Kowalski (1986). The GLSW algorithm is used for calibration
#' transfer, classification, and regression applications. It calculates a covariance
#' matrix from the differences between similar samples and applies a filtering matrix
#' to down-weight correlations present in the original covariance matrix.
#'
#' The choice of the weighting parameter \eqn{\alpha} depends on the scale of the original values
#' and the similarity between the interferences and the net analyte signal. Typical values range
#' from 1 to 0.0001.
#'
#' @param X1 A numeric matrix representing the first set of data.
#' @param X2 A numeric matrix representing the second set of data.
#' @param alpha A numeric value specifying the weighting parameter. Default is 0.01.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{G}{The filtering matrix.}
#'     \item{X1_mc}{The mean-centered \eqn{X_1} matrix.}
#'     \item{X2_mc}{The mean-centered \eqn{X_2} matrix.}
#'   }
#'
#' @references
#'    - Geladi, P., & Kowalski, B. R. (1986). Partial least-squares regression: a tutorial.
#'      Analytica Chimica Acta, 185, 1-17.
#'
#' @export glsw
glsw <- function(X1, X2, alpha = 0.01) {
  if (!is.numeric(X1) || !is.numeric(X2)) {
    stop("'X1' and 'X2' must be numeric matrices.")
  }
  if (!is.matrix(X1) || !is.matrix(X2)) {
    stop("'X1' and 'X2' must be matrices.")
  }
  if (nrow(X1) != nrow(X2)) {
    stop("'X1' and 'X2' must have the same number of rows.")
  }
  if (alpha <= 0) {
    stop("'alpha' must be a positive value.")
  }

  # Mean-center the input data
  X1_mc <- X1 - matrix(colMeans(X1), nrow = nrow(X1), ncol = ncol(X1), byrow = TRUE)
  X2_mc <- X2 - matrix(colMeans(X2), nrow = nrow(X2), ncol = ncol(X2), byrow = TRUE)

  # Calculate the difference matrix
  Xd <- X2_mc - X1_mc

  # Compute the covariance matrix
  C <- t(Xd) %*% Xd

  # Singular value decomposition
  svd_result <- svd(C)
  V <- svd_result$v
  S <- svd_result$d

  # Calculate the weighted, ridged singular values
  D <- (S^2 + alpha * diag(ncol(X1)))^(-1)

  # Compute the filtering matrix
  G <- V %*% diag(D) %*% t(V)

  # Return the results as a list
  list(G = G, X1_mc = X1_mc, X2_mc = X2_mc)
}
