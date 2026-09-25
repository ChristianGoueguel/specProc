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
#' only the information relevant to the response variable \eqn{\textbf{Y}}. The
#' part of \eqn{\textbf{X}} that cannot be explained by \eqn{\textbf{Y}},
#' \eqn{\textbf{Z} = (\textbf{I} - \textbf{Y}\textbf{Y}^{+})\textbf{X}}, spans the
#' interferent space. Its first `ncomp` principal component loadings
#' \eqn{\textbf{P}} are used to project \eqn{\textbf{X}} onto the subspace
#' orthogonal to the interferents:
#' \eqn{\textbf{X}_{NAS} = \textbf{X}(\textbf{I} - \textbf{PP}^T)}.
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
#' @param ncomp An integer specifying the number of principal components to retain for orthogonal processing. Default is 5; it is reduced if larger than the rank of the interferent space.
#' @param center A logical value specifying whether to center the data. Default is `TRUE`.
#' @param scale A logical value specifying whether to scale the data. Default is `FALSE`.
#'
#' @return A tibble containing the corrected predictor variables. The
#'   interferent loadings are stored in the `"loadings"` attribute.
#' @export nas
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(20 * 50), 20, 50)
#' y <- x[, 1] + rnorm(20, sd = 0.1)
#' x_nas <- nas(x, y, ncomp = 2)
#'
nas <- function(x, y, ncomp = 5, center = TRUE, scale = FALSE) {
  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' must be provided")
  }
  if (!is.logical(center) || !is.logical(scale)) {
    stop("Arguments 'center' and 'scale' must be boolean (TRUE or FALSE)")
  }
  xy <- prepare_xy(x, y, center, scale)
  x <- xy$x
  y <- xy$y

  z <- x - y %*% solve_ls(y, x)
  ncomp <- clamp_ncomp(ncomp, z)
  p_mat <- svd(z, nu = 0, nv = ncomp)$v
  x_nas <- x - (x %*% p_mat) %*% t(p_mat)

  out <- as_tbl(x_nas, xy$names)
  attr(out, "loadings") <- p_mat
  return(out)
}
