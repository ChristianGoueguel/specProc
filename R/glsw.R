library(Matrix)

glsw <- function(X, y, alpha = 1e-3) {

  # Check dimensions
  if (nrow(X) != nrow(y) || ncol(X) != ncol(y)) {
    stop("Matrices X and y must have the same shape")
  }

  # Center X1, X2
  X1_centered <- scale(X, center = TRUE, scale = FALSE)
  X2_centered <- scale(y, center = TRUE, scale = FALSE)

  # Build difference
  Xd <- t(X2_centered - X1_centered)

  # Covariance matrix
  C <- crossprod(Xd)

  # SVD
  svd <- svd(C)
  V <- svd$v
  S <- svd$d

  # Weight matrix
  D <- sqrt(diag(S^2 / alpha + 1))

  # Projection matrix
  G <- V %*% solve(D) %*% t(V)

  # Transformation function
  transform <- function(predictors) {
    predictors %*% G
  }

  # Return a list with the projection matrix and the transformation function
  list(G = G, transform = transform)
}
