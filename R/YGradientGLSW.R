library(matrixStats)

# YGradientGLSW algorithm
YGradientGLSW <- function(X, y) {

  # Sort the response y in increasing order
  sorted_y_indices <- order(y)
  Xsorted <- X[sorted_y_indices, ]
  ysorted <- y[sorted_y_indices]

  # Apply 5-point first derivative Savitzky-Golay filter
  Xsmoothed <- apply(Xsorted, 2, function(x) smoothRow(x))
  ysmoothed <- smoothRow(ysorted)

  # Calculate re-weighting matrix
  ysmoothedMean <- mean(ysmoothed)
  syd <- sqrt(sum((ysmoothed - ysmoothedMean)^2) / (length(ysmoothed) - 1))
  W <- diag(2^(-ysmoothed / syd))

  # Calculate covariance matrix
  return(t(Xsmoothed) %*% W %*% W %*% Xsmoothed)
}

# Smooth a given row using 5-point first derivative Savitzky-Golay filter
smoothRow <- function(x) {
  coef <- c(2/10, 1/10, 0, -1/10, -2/10)
  res <- rep(0, length(x))
  windowSize <- length(coef)
  for (m in 1:length(coef)) {
    coefIdx <- (windowSize - 1) - m + 1
    rowIdx <- seq(m-2, m+2)
    rowIdx[rowIdx < 1] <- 1
    rowIdx[rowIdx > length(x)] <- length(x)
    row <- x[rowIdx]
    rowScaled <- row * coef[coefIdx]
    res <- res + rowScaled
  }
  return(res)
}
