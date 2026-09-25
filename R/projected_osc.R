#' @title Projected Orthogonal Signal Correction
#'
#' @author Christian L. Goueguel
#'
#' @description
#' The projected orthogonal signal correction (POSC) method is a preprocessing
#' technique used to remove systematic variation from predictor variables that
#' is orthogonal to the response variable. This function implements the POSC
#' algorithm for model fitting and prediction.
#'
#' @details
#' POSC obtains OPLS-filtered data directly from an ordinary (non-orthogonalized)
#' PLS1 model (Kemsley and Tapp, 2009):
#' 1. A PLS1 model with `ncomp` components is fitted, giving the score matrix
#'    \eqn{\textbf{T}} and the fitted response \eqn{\hat{\textbf{y}}}.
#' 2. The part of the score space orthogonal to \eqn{\hat{\textbf{y}}},
#'    \eqn{\textbf{T} - \hat{\textbf{y}}(\hat{\textbf{y}}^T\hat{\textbf{y}})^{-1}\hat{\textbf{y}}^T\textbf{T}},
#'    spans `ncomp - 1` orthogonal components with scores \eqn{\textbf{T}_o}.
#' 3. The orthogonal loadings are \eqn{\textbf{P}_o = \textbf{X}^T\textbf{T}_o(\textbf{T}_o^T\textbf{T}_o)^{-1}}
#'    and the filtered data are \eqn{\textbf{X} - \textbf{T}_o\textbf{P}_o^T}.
#'
#' The filtered data are identical to those obtained from an OPLS model with one
#' predictive and `ncomp - 1` orthogonal components.
#'
#' @references
#'  - Kemsley, E.K., Tapp, H.S., (2009).
#'    OPLS filtered data can be obtained directly from non-orthogonalized PLS1.
#'    Journal of Chemometrics, 23(5):263-264.
#'  - Trygg, J., Wold, S., (2002).
#'    Orthogonal projections to latent structures (O-PLS).
#'    Journal of Chemometrics, 16(3):119-128.
#'
#' @param x A matrix or data frame of the predictor variables.
#' @param y A vector of the response variable.
#' @param ncomp An integer specifying the number of PLS components (at least 2). `ncomp - 1` orthogonal components are removed. Default is 5.
#' @param center A logical value indicating whether to mean-center `x` and `y`. Default is `TRUE`.
#' @param scale A logical value indicating whether to scale `x` and `y`. Default is `FALSE`.
#' @param tol A numeric value; orthogonal components whose singular value is smaller than `tol` times the largest one are discarded. The default value is 1e-10.
#' @param newdata An optional matrix or data frame of new predictor variables to be corrected using the POSC model. It is preprocessed with the centers and scales of `x`.
#'
#' @return A list containing the following components:
#'  - `correction`: The corrected `x`.
#'  - `scores`: The orthogonal scores matrix \eqn{\textbf{T}_o}.
#'  - `loadings`: The orthogonal loadings matrix \eqn{\textbf{P}_o}.
#'  - `weights`: The orthogonal weights \eqn{\textbf{W}_o}, such that \eqn{\textbf{T}_o = \textbf{XW}_o}.
#'  - `center`, `scale`: The column centers and scales applied to `x`.
#'  - `newdata`: If `newdata` is provided, a list with the corrected new data (`correction`) and its orthogonal scores (`scores`).
#'
#' @export projected_osc
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(30 * 40), 30, 40)
#' y <- x[, 1] + rnorm(30, sd = 0.1)
#' res <- projected_osc(x[1:20, ], y[1:20], ncomp = 3, newdata = x[21:30, ])
#' dim(res$newdata$correction)
#'
projected_osc <- function(x, y, ncomp = 5, center = TRUE, scale = FALSE, tol = 1e-10, newdata = NULL) {

  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' must be provided.")
  }
  check_count(ncomp, "ncomp", lower = 2)
  check_number(tol, "tol", lower = 0)

  xy <- prepare_xy(x, y, center, scale)
  if (ncol(xy$y) != 1) {
    stop("'y' must be a single response variable.")
  }
  x <- xy$x
  y <- xy$y
  ncomp <- min(ncomp, nrow(x) - 1, ncol(x))
  if (ncomp < 2) {
    stop("At least 2 PLS components are needed; the data have too few observations or variables.")
  }

  fit <- pls::simpls.fit(x, y, ncomp = ncomp, center = FALSE)
  t_mat <- unclass(fit$scores)
  r_mat <- unclass(fit$projection)
  q <- t(unclass(fit$Yloadings))
  y_hat <- t_mat %*% q

  # T_o = T K spans the part of the PLS score space orthogonal to y_hat.
  k_mat <- diag(ncomp) - q %*% solve(crossprod(y_hat)) %*% crossprod(y_hat, t_mat)
  sv <- svd(t_mat %*% k_mat)
  keep <- which(sv$d > tol * max(sv$d))
  keep <- keep[seq_len(min(length(keep), ncomp - 1))]
  if (length(keep) == 0) {
    stop("No orthogonal component found; try a larger 'ncomp' or a smaller 'tol'.")
  }

  w_o <- r_mat %*% k_mat %*% sv$v[, keep, drop = FALSE]
  t_o <- x %*% w_o
  p_o <- crossprod(x, t_o) %*% solve(crossprod(t_o))
  x_posc <- x - tcrossprod(t_o, p_o)

  comp <- paste0("ortho", seq_along(keep))
  res <- list(
    "correction" = as_tbl(x_posc, xy$names),
    "scores" = as_tbl(t_o, comp),
    "loadings" = as_tbl(p_o, comp),
    "weights" = as_tbl(w_o, comp),
    "center" = xy$center,
    "scale" = xy$scale
  )

  if (!is.null(newdata)) {
    newdata <- as_numeric_matrix(newdata, "newdata")
    if (ncol(newdata) != ncol(x)) {
      stop("'newdata' must have the same number of columns as 'x'.")
    }
    xn <- apply_preprocess(newdata, list(center = xy$center, scale = xy$scale))
    t_new <- xn %*% w_o
    res$newdata <- list(
      "correction" = as_tbl(xn - tcrossprod(t_new, p_o), xy$names),
      "scores" = as_tbl(t_new, comp)
    )
  }

  return(res)
}
