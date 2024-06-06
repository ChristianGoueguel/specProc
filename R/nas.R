#' @title Net Analyte Signal
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the Net Analyte Signal (NAS) algorithm, as proposed
#' by Lorber (1997), which involves the concentration matrix. NAS aims to remove
#' information from the predictor variables, that is orthogonal or irrelevant to
#' the response variable(s) by orthogonal projection.
#'
#' @details
#' The NAS algorithm aims to obtain a corrected matrix that contains
#' only the information relevant to the response variable \eqn{\textbf{Y}}. This is achieved
#' by constructing an orthogonal projection matrix based on the principal
#' components of the data matrix, \eqn{\textbf{X}}, that are orthogonal to \eqn{\textbf{Y}}. The corrected
#' matrix is then obtained by projecting \eqn{\textbf{X}} onto the subspace orthogonal
#' to the unwanted components.
#'
#'
#' @references
#'    - Lorber, A., (1997).
#'      Net analyte signal calculation in multivariate calibration.
#'      Anal. Chem., 69(8):1620-1626
#'    - Faber, N.M., (1998).
#'      Efficient computation of net analyte signal vector in inverse
#'      multivariate calibration models.
#'      Anal. Chem., 70(23):5108-5110
#'
#' @param x A matrix or data frame of the predictor variables
#' @param y A vector, matrix or data frame of the response variable(s)
#' @param ncomp An integer specifying the number of principal components to
#' retain for orthogonal processing. Default is 2.
#' @param center A logical value specifying whether to center the data. Default is `TRUE`.
#' @param scale A logical value specifying whether to scale the data. Default is `FALSE`.
#'
#' @return A tibble containing the corrected predictor variables
#' @export nas
#'
nas <- function(x, y, ncomp = 2, center = TRUE, scale = FALSE) {

  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' must be provided")
  }
  if (!is.logical(center) || !is.logical(scale)) {
    stop("Arguments 'center' and 'scale' must be boolean (TRUE or FALSE)")
  }
  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }
  if (is.data.frame(y) || tibble::is_tibble(y)) {
    y <- as.matrix(y)
  }
  if (nrow(x) != nrow(y)) {
    stop("x and y don't match.")
  }

  if (center == TRUE && scale == FALSE) {
    x <- scale(x, center = TRUE)
    y <- scale(y, center = TRUE)
  }
  if (center == FALSE && scale == TRUE) {
    x <- scale(x, scale = TRUE)
    y <- scale(y, scale = TRUE)
  }
  if (center == TRUE && scale == TRUE) {
    x <- scale(x, center = TRUE, scale = TRUE)
    y <- scale(y, center = TRUE, scale = TRUE)
  }

  if (ncomp < 1 || ncomp > min(nrow(x) - 1, ncol(x))) {
    ncomp <- min(nrow(x) - 1, ncol(x))
  }

  z <- (diag(nrow(x)) - y %*% t(y) / (t(y) %*% y)) %*% x

  pca_mod <- stats::prcomp(z, scale = FALSE)
  p <- pca_mod$rotation[, 1:ncomp]
  r <- diag(ncomp) - p %*% t(p)

  x_nas <- x %*% r

  return(tibble::as_tibble(x_nas))
}
