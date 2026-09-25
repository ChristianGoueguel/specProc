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
#' Contrary to the Orthogonal Signal Correction (OSC) algorithm, Wold *et al.* (1998), which uses inverse
#' Partial Least Squares (PLS) regression to filter out the orthogonal signal, DO
#' filters out the orthogonal signal directly by orthogonalization of the
#' \eqn{\textbf{X}} matrix:
#' \deqn{\textbf{Z} = \textbf{X} - \textbf{Y}(\textbf{Y}^T\textbf{Y})^{-1}\textbf{Y}^T\textbf{X}}
#' Principal Components Analysis (PCA) is performed on \eqn{\textbf{Z}} to obtain
#' the loadings \eqn{\textbf{P}}, and the corrected matrix is
#' \eqn{\textbf{X}_{DO} = \textbf{X} - \textbf{XPP}^T}. Direct Orthogonalization
#' is typically simpler and faster than OSC.
#'
#' To correct new data, preprocess it with the returned `center` and `scale`
#' vectors and apply \eqn{\textbf{X}_{new} - \textbf{X}_{new}\textbf{PP}^T}.
#'
#' @references
#'    - Andersson, C.A., (1999).
#'      Direct orthogonalization.
#'      Chemometrics Intell. Lab. Syst., 47(1):51-63
#'    - Pierna, J.A.F., Massart, D.L., de Noord, O.E., Ricoux, P., (2001).
#'      Direct orthogonalization: some case studies.
#'      Chemometrics Intell. Lab. Syst., 55(1-2):101-108
#'    - Wold, S., Antti, H., Lindgren, F., Ohman, J. (1998).
#'      Orthogonal signal correction of near-infrared spectra.
#'      Chemometrics Intell. Lab. Syst., 44(1):175-185.
#'
#' @param x A matrix or data frame of the predictor variables
#' @param y A vector, matrix or data frame of the response variable(s)
#' @param ncomp An integer specifying the number of principal components to
#' retain for orthogonal processing. Default is 2. Values larger than the
#' rank of the orthogonalized matrix are reduced accordingly.
#' @param center A logical value specifying whether to center the data. Default is `TRUE`.
#' @param scale A logical value specifying whether to scale the data. Default is `FALSE`.
#'
#' @return A list with the following components:
#'  - `correction`: The corrected matrix.
#'  - `loading`: The loadings matrix \eqn{\textbf{P}}.
#'  - `score`: The scores matrix \eqn{\textbf{XP}}.
#'  - `center`, `scale`: The column centers and scales applied to `x`.
#' @export direct_orthogonal
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(20 * 50), 20, 50)
#' y <- x[, 1] + rnorm(20, sd = 0.1)
#' res <- direct_orthogonal(x, y, ncomp = 2)
#' dim(res$correction)
#'
direct_orthogonal <- function(x, y, ncomp = 2, center = TRUE, scale = FALSE) {

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

  t_mat <- x %*% p_mat
  x_do <- x - tcrossprod(t_mat, p_mat)

  result <- list(
    correction = as_tbl(x_do, xy$names),
    loading = as_tbl(p_mat, paste0("comp", seq_len(ncomp))),
    score = as_tbl(t_mat, paste0("comp", seq_len(ncomp))),
    center = xy$center,
    scale = xy$scale
  )
  return(result)
}

# Shared input handling for the orthogonalization methods: converts x and y to
# numeric matrices, checks dimensions and missing values, and preprocesses.
prepare_xy <- function(x, y, center, scale) {
  check_flag(center, "center")
  check_flag(scale, "scale")
  x <- as_numeric_matrix(x, "x")
  y <- as_response_matrix(y, nrow(x), "y")
  if (anyNA(x) || anyNA(y)) {
    stop("'x' and 'y' cannot contain missing values.", call. = FALSE)
  }
  if (nrow(x) < 3) {
    stop("At least 3 observations are required.", call. = FALSE)
  }
  px <- preprocess(x, center, scale)
  py <- preprocess(y, center, scale)
  list(
    x = px$x, y = py$x, names = colnames(x),
    center = px$center, scale = px$scale
  )
}

# Least-squares coefficients B minimizing ||Y - A B||, via the pseudo-inverse.
solve_ls <- function(a, b) {
  MASS::ginv(a) %*% b
}

# Limits the number of components to the numerical rank of a matrix.
clamp_ncomp <- function(ncomp, m) {
  check_count(ncomp, "ncomp")
  d <- svd(m, nu = 0, nv = 0)$d
  rank <- sum(d > max(dim(m)) * max(d) * .Machine$double.eps)
  if (rank < 1) {
    stop("The orthogonalized matrix has rank 0; nothing to remove.", call. = FALSE)
  }
  if (ncomp > rank) {
    warning("'ncomp' reduced to ", rank, " (the rank of the orthogonal subspace).", call. = FALSE)
    ncomp <- rank
  }
  as.integer(ncomp)
}
