#' @title Direct Orthogonal Signal Correction
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the Direct Orthogonal Signal Correction (DOSC)
#' algorithm, as proposed by Westerhuis *et al.* (2001), to remove systematic
#' variation from predictor variables, \eqn{\textbf{X}}, that is orthogonal to
#' the response variable(s), \eqn{\textbf{Y}}.
#'
#' @details
#' Different from the Orthogonal Signal Correction (OSC) algorithm, Wold *et al.*
#' (1998), the DOSC algorithm firstly orthogonalizes the matrices \eqn{\textbf{X}}
#' and \eqn{\textbf{Y}}. Then principal components analysis (PCA) is performed on
#' the orthogonalized \eqn{\textbf{X}} to obtain the scores \eqn{\textbf{T}} and
#' loadings \eqn{\textbf{P}} matrices.
#'
#' @references
#'    - Westerhuis, J.A., Jong, S.D., Smilde, A.K., (2001).
#'      Direct orthogonal signal correction.
#'      Chemometrics Intell. Lab. Syst., 56(1):13-25
#'    - Wold, S., Antti, H., Lindgren, F., Ohman, J. (1998).
#'      Orthogonal signal correction of near-infrared spectra.
#'      Chemometrics Intell. Lab. Syst., 44(1):175-185.
#'
#' @param x A matrix or data frame of the predictor variables
#' @param y A vector, matrix or data frame of the response variable(s)
#' @param ncomp An integer specifying the number of principal components to
#' retain for orthogonal processing. Default is 2.
#' @param center A logical value specifying whether to center the data. Default is `TRUE`.
#' @param scale A logical value specifying whether to scale the data. Default is `FALSE`.
#'
#' @return A list with the following components:
#'  - `correction`: The corrected \eqn{\textbf{X}} matrix after DOSC.
#'  - `loading`: The loadings \eqn{\textbf{P}} matrix.
#'  - `score`: The scores \eqn{\textbf{T}} matrix.
#' @export direct_osc
#'
direct_osc <- function(x, y, ncomp = 10, center = TRUE, scale = FALSE) {

  requireNamespace("pls", quietly = TRUE)

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

  m <- t(x) %*% MASS::ginv(t(x)) %*% t(y)
  z <- x - m %*% MASS::ginv(m) %*% x

  pca_mod <- stats::prcomp(z %*% t(z), scale = TRUE)
  t <- pca_mod$x[, 1:ncomp]

  w <- pls::oscorespls.fit(x, t)$weights
  t <- x %*% w
  p <- (t(x) %*% t) / (t(t) %*% t)

  x_dosc <- x - t %*% t(p)

  result <- list(
    correction = tibble::as_tibble(x_dosc),
    loading = tibble::as_tibble(p),
    score = tibble::as_tibble(t)
  )

  return(result)
}
