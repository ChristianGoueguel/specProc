#' @title External Parameter Orthogonalization
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the External Parameter Orthogonalization (EPO)
#' algorithm as proposed by Roger *et al.* (2003). The EPO algorithm aims to
#' remove the influence of independent external factors or interferences from
#' the input data (spectrum), effectively separating the signal of interest from
#' unwanted perturbations.
#'
#' @details
#' The EPO algorithm works by detecting directions (singular vectors) in the input data
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
#'   \item \code{correction}: The orthogonalized data, representing the signal of interest.
#'   \item \code{perturbation}: The perturbation data filtered out.
#'   \item \code{loadings}: The singular vectors (loadings of the input data) used for the orthogonalization.
#' }
#'
#' @references
#'  - Roger, J.-M., Chauchard, F., Bellon-Maurel, V. (2003). EPO-PLS external
#'    parameter orthogonalization of PLS application to temperature-independent
#'    measurement of sugar content of intact fruits.
#'    Chemometrics and Intelligent Laboratory Systems, 66(2):191-204.
#'
#' @export epo
#'
epo <- function(data, ncomp = 2) {
  if (missing(data)) {
    stop("Missing 'data' argument.")
  }

  if (is.data.frame(data) || tibble::is_tibble(data)) {
    X <- as.matrix(data)
  } else {
    X <- data
  }

  if (dim(X)[1] == 1 || dim(X)[2] == 1) {
    stop("The dimensions of the input data must be at least 2 x 2")
  } else {
    ncomp <- min(ncomp, dim(X)[1], dim(X)[2])
  }

  if (ncomp %% 1 != 0 | ncomp <= 0) {
    stop("'ncomp' must be a positive integer greater than 0.")
  }

  result <- epo_cpp(X, ncomp)

  result$correction <- tibble::as_tibble(result$correction)
  colnames(result$correction) <- colnames(X)

  result$perturbation <- tibble::as_tibble(result$perturbation)
  colnames(result$perturbation) <- colnames(X)

  result$loadings <- tibble::as_tibble(result$loadings)

  return(result)
}


