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
#' The EPO algorithm works by detecting the dominant directions (right singular
#' vectors) of a clutter matrix \eqn{\textbf{D}} that describe the external
#' variations, e.g. the differences between spectra of the same samples measured
#' under different temperatures or moisture levels. It then projects the spectral
#' matrix onto the subspace orthogonal to these variations, effectively removing
#' the unwanted perturbations and extracting the signal of interest.
#'
#' Let \eqn{\textbf{X}} be the spectral matrix. The EPO algorithm aims to split \eqn{\textbf{X}} into:
#'
#' \deqn{\textbf{X} = \textbf{XP} + \textbf{XQ} + \textbf{R}}
#'
#' where \eqn{\textbf{P}} and \eqn{\textbf{Q}} are, respectively, the projection
#' matrices of \eqn{\textbf{X}} onto the useful and perturbation (clutter) subspaces.
#' \eqn{\textbf{R}} is the residual matrix. With \eqn{\textbf{V}} the first `ncomp`
#' right singular vectors of \eqn{\textbf{D}}, \eqn{\textbf{Q} = \textbf{VV}^T} and
#' the corrected matrix is \eqn{\textbf{X} - \textbf{XVV}^T}.
#'
#' The singular value decomposition is computed in C++ (Eigen), and the
#' \eqn{p \times p} projection matrix is never formed explicitly.
#'
#' @param x A numeric matrix, data frame or tibble.
#' @param ncomp An integer specifying the number of singular vectors to orthogonalize against. Default is 2.
#' @param clutter An optional numeric matrix or data frame, with the same number of
#'   columns as `x`, describing the external (clutter) variation. If `NULL`
#'   (default), the clutter directions are estimated from `x` itself, i.e. the
#'   `ncomp` dominant directions of `x` are removed.
#'
#' @return The function returns a list of four components:
#' \itemize{
#'   \item \code{correction}: The orthogonalized matrix, representing the signal of interest.
#'   \item \code{clutter}: The clutter part of `x`, \eqn{\textbf{XVV}^T}.
#'   \item \code{loadings}: The singular vectors \eqn{\textbf{V}} used for the orthogonalization.
#'   \item \code{singular_values}: The corresponding singular values of the clutter matrix.
#' }
#'
#' @references
#'  - Roger, J.-M., Chauchard, F., Bellon-Maurel, V. (2003).
#'    EPO-PLS external parameter orthogonalization of PLS application to temperature-independent measurement of sugar content of intact fruits.
#'    Chemometrics and Intelligent Laboratory Systems, 66(2):191-204.
#'
#' @export epo
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(20 * 50), 20, 50)
#' # spectra of the same samples measured at a second temperature
#' x_temp <- x + outer(rnorm(20), sin(seq(0, pi, length.out = 50)))
#' res <- epo(x, ncomp = 1, clutter = x_temp - x)
#' dim(res$correction)
#'
epo <- function(x, ncomp = 2, clutter = NULL) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  x <- as_numeric_matrix(x, "x")
  if (nrow(x) < 2 || ncol(x) < 2) {
    stop("The dimensions of the input data must be at least 2 x 2")
  }
  d <- if (is.null(clutter)) x else as_numeric_matrix(clutter, "clutter")
  if (ncol(d) != ncol(x)) {
    stop("'clutter' must have the same number of columns as 'x'.")
  }
  if (anyNA(x) || anyNA(d)) {
    stop("'x' and 'clutter' cannot contain missing values.")
  }
  if (!is.numeric(ncomp) || length(ncomp) != 1 || ncomp %% 1 != 0 || ncomp <= 0) {
    stop("'ncomp' must be a positive integer greater than 0.")
  }
  ncomp <- min(ncomp, dim(d))

  result <- epo_cpp(x, d, as.integer(ncomp))
  comp <- paste0("comp", seq_len(ncomp))

  list(
    correction = as_tbl(result$correction, colnames(x)),
    clutter = as_tbl(result$clutter, colnames(x)),
    loadings = as_tbl(result$loadings, comp),
    singular_values = drop(result$singular_values)
  )
}
