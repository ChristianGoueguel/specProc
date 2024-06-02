#' @title External Parameter Orthogonalization
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the External Parameter Orthogonalization (EPO)
#' algorithm as proposed by Roger *et al.* (2003). The EPO algorithm aims to
#' remove the influence of external factors or interferences from the input data,
#' effectively separating the signal of interest from unwanted perturbations.
#'
#' @details
#' The EPO algorithm works by detecting directions in the input data (or singular vectors)
#' that are affected by external variations. It then projects the input data onto
#' the subspace orthogonal to these variations, effectively removing the unwanted
#' perturbations and extracting the signal of interest.
#'
#' Let \eqn{\textbf{X}} be the input data. The EPO algorithm aims to split \eqn{\textbf{X}} into:
#'
#' \deqn{\textbf{X} = \textbf{XP} + \textbf{XQ} + \textbf{R}}
#'
#' where \eqn{\textbf{P}} and \eqn{\textbf{Q}} are, respectively, the projection
#' matrices of \eqn{\textbf{X}} onto the useful and perturbation subspaces.
#' \eqn{\textbf{R}} is the residual matrix.
#'
#' @param data A numeric matrix, data frame or tibble.
#' @param ncomp An integer specifying the number of singular vectors to
#' orthogonalize against. If not provided, the default value is 2.
#'
#' @return The function returns three components:
#' \itemize{
#'   \item \code{correction}: The input data after orthogonalization, representing the signal of interest (\eqn{\textbf{XP}}).
#'   \item \code{perturbation}: The perturbation filtered out (\eqn{\textbf{XQ}}).
#'   \item \code{loadings}: The loadings (singular vectors) of the input data used for the orthogonalization.
#' }
#'
#' @references
#'  - Roger, J.-M., Chauchard, F., Bellon-Maurel, V. (2003). EPO-PLS external
#'    parameter orthogonalisation of PLS application to temperature-independent
#'    measurement of sugar content of intact fruits.
#'    Chemometrics and Intelligent Laboratory Systems, 66(2):191-204.
#'
#' @export epo
#'
epo <- function(data, ncomp = 2) {
  if (missing(data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.numeric(data)) {
    stop("The input data must be numeric.")
  }
  if (!is.integer(ncomp) | ncomp <= 0) {
    stop("'ncomp' must be a positive integer greater than 0.")
  }

  if (is.data.frame(data) || tibble::is_tibble(data)) {
    X <- as.matrix(data)
  } else {
    X <- data
  }

  ncomp <- min(ncomp, dim(X)[1], dim(X)[2])

  decomp <- svd(X)
  perturbation_direction <- decomp$v[, 1:ncomp, drop = FALSE]

  I <- diag(1, nrow = dim(X)[2], ncol = dim(X)[2])
  Q <- tcrossprod(parasitic_direction)

  X_corrected <- X %*% (I - Q)
  X_perturbation <- X %*% Q

  result <- list(
    correction = X_corrected %>% tibble::as_tibble(),
    perturbation = X_perturbation %>% tibble::as_tibble(),
    loadings = perturbation_direction %>% tibble::as_tibble()
    )

  return(result)
}


