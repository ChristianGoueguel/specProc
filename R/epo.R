#' @title External Parameter Orthogonalization
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the External Parameter Orthogonalization (EPO)
#' algorithm as proposed by Roger *et al.* (2003). The EPO algorithm aims to
#' remove interferences or clutter present in the spectral matrix, effectively
#' separating the signal of interest from unwanted perturbations.
#'
#' @details
#' The EPO algorithm works by detecting directions (singular vectors) in the spectral matrix
#' that are affected by external variations. It then projects the spectral matrix onto
#' the subspace orthogonal to these variations, effectively removing the unwanted
#' perturbations and extracting the signal of interest.
#'
#' Let \eqn{\textbf{x}} be the spectral matrix. The EPO algorithm aims to split \eqn{\textbf{x}} into:
#'
#' \deqn{\textbf{x} = \textbf{xP} + \textbf{xQ} + \textbf{R}}
#'
#' where \eqn{\textbf{P}} and \eqn{\textbf{Q}} are, respectively, the projection
#' matrices of \eqn{\textbf{x}} onto the useful and perturbation (clutter) subspaces.
#' \eqn{\textbf{R}} is the residual matrix.
#'
#' @param x A numeric matrix, data frame or tibble.
#' @param ncomp An integer specifying the number of singular vectors to
#' orthogonalize against. If not provided, the default value is 2.
#'
#' @return The function returns a list of three components:
#' \itemize{
#'   \item \code{correction}: The orthogonalized matrix, representing the signal of interest.
#'   \item \code{clutter}: The clutter data matrix.
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
epo <- function(x, ncomp = 2) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }

  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }

  if (dim(x)[1] == 1 || dim(x)[2] == 1) {
    stop("The dimensions of the input data must be at least 2 x 2")
  } else {
    ncomp <- min(ncomp, dim(x)[1], dim(x)[2])
  }

  if (ncomp %% 1 != 0 | ncomp <= 0) {
    stop("'ncomp' must be a positive integer greater than 0.")
  }

  result <- epo_cpp(x, ncomp)

  result$correction <- tibble::as_tibble(result$correction)
  colnames(result$correction) <- colnames(x)

  result$clutter <- tibble::as_tibble(result$clutter)
  colnames(result$clutter) <- colnames(x)

  result$loadings <- tibble::as_tibble(result$loadings)

  return(result)
}


