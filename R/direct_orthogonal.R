#' @title Direct Orthogonalization
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the Direct Orthogonalization (DO) algorithm, as proposed
#' by Andersson (1999), to filter out variation from predictor variables,
#' \eqn{\textbf{X}}, that is orthogonal to the response variable(s), \eqn{\textbf{Y}}.
#'
#' @details
#' Contrary to the Orthogonal Signal Correction (OSC) algorithm, Wold and Lindgren (1998), which uses inverse
#' Partial Least Squares (PLS) regression to filter out the orthogonal signal, DO
#' filters out the orthogonal signal directly by orthogonalization of the
#' \eqn{\textbf{X}} matrix. Principal Components Analysis (PCA) is performed on the
#' orthogonalized \eqn{\textbf{X}} to obtain the scores \eqn{\textbf{T}} and loadings
#' \eqn{\textbf{P}} matrices. Direct Orthogonalization is typically simpler and
#' faster than OSC.
#'
#' @references
#'    - Andersson, C.A., (1999).
#'      Direct orthogonalization.
#'      Chemometrics Intell. Lab. Syst., 47(1):51-63
#'    - Pierna, J.A.F., Massart, de Noord, O.E., Ricoux, P., (2001).
#'      Direct orthogonalization: some case studies.
#'      Chemometrics Intell. Lab. Syst., 55(1-2):101-108
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
#'  - `correction`: The corrected \eqn{\textbf{X}} matrix after DO.
#'  - `loading`: The loadings \eqn{\textbf{P}} matrix.
#'  - `score`: The scores \eqn{\textbf{T}} matrix.
#' @export direct_orthogonal
#'
direct_orthogonal <- function(x, y, ncomp = 10, center = TRUE, scale = FALSE) {

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

  m <- t(X) %*% Y %*% solve(t(Y) %*% Y)
  z <- x - y %*% t(m)

  pca_mod <- stats::prcomp(z, scale = TRUE)
  t <- pca_mod$x[, 1:ncomp]
  p <- pca_mod$rotation[, 1:ncomp]

  t <- x %*% p
  x_do <- x - t %*% t(p)

  result <- list(
    correction = tibble::as_tibble(x_do),
    loading = tibble::as_tibble(p),
    score = tibble::as_tibble(t)
  )

  return(result)
}
