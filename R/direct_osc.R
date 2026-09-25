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
#' (1998), the DOSC algorithm is non-iterative:
#' 1. \eqn{\textbf{Y}} is projected onto the column space of \eqn{\textbf{X}}:
#'    \eqn{\hat{\textbf{Y}} = \textbf{XX}^{+}\textbf{Y}}.
#' 2. \eqn{\textbf{X}} is orthogonalized with respect to \eqn{\hat{\textbf{Y}}}:
#'    \eqn{\textbf{Z} = \textbf{X} - \hat{\textbf{Y}}\hat{\textbf{Y}}^{+}\textbf{X}}.
#' 3. PCA of \eqn{\textbf{Z}} gives the orthogonal scores \eqn{\textbf{T}}.
#' 4. The weights \eqn{\textbf{W} = \textbf{X}^{+}\textbf{T}} express the scores
#'    as a linear combination of \eqn{\textbf{X}}, the loadings are
#'    \eqn{\textbf{P} = \textbf{X}^T\textbf{T}(\textbf{T}^T\textbf{T})^{-1}}, and the
#'    corrected matrix is \eqn{\textbf{X} - \textbf{TP}^T}.
#'
#' New data are corrected with \eqn{\textbf{X}_{new} - \textbf{X}_{new}\textbf{WP}^T}
#' after applying the returned `center` and `scale`.
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
#' @param ncomp An integer specifying the number of orthogonal components to remove. Default is 10; it is reduced if larger than the rank of the orthogonalized matrix.
#' @param center A logical value specifying whether to center the data. Default is `TRUE`.
#' @param scale A logical value specifying whether to scale the data. Default is `FALSE`.
#' @param tol A numeric value giving the relative tolerance used to compute the
#' pseudo-inverse of \eqn{\textbf{X}}; singular values smaller than `tol` times
#' the largest one are discarded, which regularizes the weights. Default is 1e-3.
#'
#' @return A list with the following components:
#'  - `correction`: The corrected matrix.
#'  - `loading`: The loadings matrix \eqn{\textbf{P}}.
#'  - `score`: The scores matrix \eqn{\textbf{T}}.
#'  - `weight`: The weights matrix \eqn{\textbf{W}}.
#'  - `center`, `scale`: The column centers and scales applied to `x`.
#' @export direct_osc
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(20 * 50), 20, 50)
#' y <- x[, 1] + rnorm(20, sd = 0.1)
#' res <- direct_osc(x, y, ncomp = 2)
#' dim(res$correction)
#'
direct_osc <- function(x, y, ncomp = 10, center = TRUE, scale = FALSE, tol = 1e-3) {

  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' must be provided")
  }
  if (!is.logical(center) || !is.logical(scale)) {
    stop("Arguments 'center' and 'scale' must be boolean (TRUE or FALSE)")
  }
  check_number(tol, "tol", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  xy <- prepare_xy(x, y, center, scale)
  x <- xy$x
  y <- xy$y

  x_pinv <- MASS::ginv(x, tol = tol)
  y_hat <- x %*% (x_pinv %*% y)
  z <- x - y_hat %*% (MASS::ginv(y_hat) %*% x)

  ncomp <- clamp_ncomp(ncomp, z)
  sv <- svd(z, nu = ncomp, nv = 0)
  t_mat <- sv$u %*% diag(sv$d[seq_len(ncomp)], ncomp)

  w_mat <- x_pinv %*% t_mat
  t_mat <- x %*% w_mat
  p_mat <- crossprod(x, t_mat) %*% solve(crossprod(t_mat))
  x_dosc <- x - tcrossprod(t_mat, p_mat)

  comp <- paste0("comp", seq_len(ncomp))
  result <- list(
    correction = as_tbl(x_dosc, xy$names),
    loading = as_tbl(p_mat, comp),
    score = as_tbl(t_mat, comp),
    weight = as_tbl(w_mat, comp),
    center = xy$center,
    scale = xy$scale
  )
  return(result)
}
