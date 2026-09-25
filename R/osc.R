#' @title Orthogonal Signal Correction
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements three orthogonal signal correction (OSC) algorithms, which
#' are a class of preprocessing techniques designed to minimize, in a set of spectral
#' data, the systematic variability or noise not directly related to or correlated
#' with the response vector or property of interest.
#'
#' @details
#' The OSC algorithm identifies and removes the orthogonal variation in the input
#' spectral matrix, \eqn{\textbf{X}}, by iteratively deflating \eqn{\textbf{X}}
#' with respect to the response vector \eqn{\textbf{y}}. The resulting \eqn{\textbf{X}}-matrix
#' contains only the variation that is relevant to the \eqn{\textbf{y}}-vector,
#' which can then be used for further modeling or analysis. This function implements
#' three different methods for OSC:
#'  - `"wold"`: the original method of Wold *et al.* (1998). The first principal
#'    component score is orthogonalized to \eqn{\textbf{y}} and the weights are
#'    obtained by a PLS regression of the orthogonalized score on \eqn{\textbf{X}},
#'    iterating until convergence.
#'  - `"sjoblom"`: the method of Sjöblom *et al.* (1998), where the weights are
#'    obtained by a simple least-squares regression instead of PLS.
#'  - `"fearn"`: the non-iterative method of Fearn (2000), which finds the
#'    directions of maximum variance of \eqn{\textbf{X}} subject to the scores
#'    being exactly orthogonal to \eqn{\textbf{y}}.
#'
#' For the iterative methods, \eqn{\textbf{X}} is deflated after each component.
#' New data can be corrected by applying the returned `center` and `scale`
#' and then, for each component \eqn{i}, computing
#' \eqn{\textbf{t}_i = \textbf{X}_{new}\textbf{w}_i} and
#' \eqn{\textbf{X}_{new} \leftarrow \textbf{X}_{new} - \textbf{t}_i\textbf{p}_i^T}.
#'
#' @references
#'  - Sjöblom, J., Svensson, O., Josefson, M., Kullberg, H., Wold, S., (1998).
#'    An evaluation of orthogonal signal correction applied to calibration transfer of near infrared spectra.
#'    Chemometrics Intell. Lab. Syst., 44(1):229-244.
#'  - Fearn, T., (2000).
#'    On orthogonal signal correction.
#'    Chemometrics Intell. Lab. Syst., 50(1):47-52.
#'  - Wold, S., Antti, H., Lindgren, F., Ohman, J. (1998).
#'    Orthogonal signal correction of near-infrared spectra.
#'    Chemometrics Intell. Lab. Syst., 44(1):175-185.
#'  - Svensson, O., Kourti, T. and MacGregor, J.F., (2002).
#'    An investigation of orthogonal correction algorithms and their characteristics.
#'    Journal of Chemometrics, 16(1):176-188.
#'
#' @param x A matrix or data frame of the predictor variables.
#' @param y A vector (or one-column matrix/data frame) of the response variable.
#' @param method A character string indicating the OSC method to use. Accepted values are `"wold"`, `"sjoblom"` and `"fearn"`. Default is `"sjoblom"`.
#' @param center A logical value indicating whether to mean-centered `x` and `y`. Default is `TRUE`.
#' @param scale A logical value indicating whether to scale `x` and `y`. Default is `FALSE`.
#' @param ncomp An integer representing the number of orthogonal components to remove. Default value is 10; it is reduced to `min(n - 1, p)` if larger.
#' @param tol A numeric value representing the tolerance for convergence. The default value is 1e-3.
#' @param max.iter An integer representing the maximum number of iterations. The default value is 10.
#' @param pls.ncomp An integer giving the number of PLS components used to compute the weights in Wold's method. Default is 5.
#'
#' @return A list containing the following components:
#'  - `correction`: The corrected matrix.
#'  - `scores`: The orthogonal scores matrix.
#'  - `loadings`: The orthogonal loadings matrix.
#'  - `weights`: The orthogonal weights matrix.
#'  - `R2`: The percentage of the (preprocessed) variance of `x` remaining after correction.
#'  - `angle`: The mean angle (in degrees) between the orthogonal scores and `y`; values close to 90 indicate orthogonality.
#'  - `center`, `scale`: The column centers and scales applied to `x`.
#'
#' @export osc
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(20 * 50), 20, 50)
#' y <- x[, 1] + rnorm(20, sd = 0.1)
#' res <- osc(x, y, method = "fearn", ncomp = 2)
#' res$angle
#'
osc <- function(x, y, method = "sjoblom", center = TRUE, scale = FALSE, ncomp = 10, tol = 1e-3, max.iter = 10, pls.ncomp = 5) {

  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' must be provided.")
  }
  method <- match.arg(method, c("wold", "sjoblom", "fearn"))
  check_count(ncomp, "ncomp")
  check_count(max.iter, "max.iter")
  check_count(pls.ncomp, "pls.ncomp")
  check_number(tol, "tol", lower = 0, lower_open = TRUE)

  xy <- prepare_xy(x, y, center, scale)
  if (ncol(xy$y) != 1) {
    stop("'y' must be a single response variable.")
  }
  x <- xy$x
  y <- xy$y
  n <- nrow(x)
  p <- ncol(x)
  if (ncomp > min(n - 1, p)) {
    ncomp <- min(n - 1, p)
  }

  out <- switch(
    method,
    wold = osc_iterative(x, y, ncomp, tol, max.iter, weight_fun = function(x, t) {
      k <- min(pls.ncomp, n - 1, p)
      drop(pls::simpls.fit(x, t, ncomp = k, center = FALSE)$coefficients[, , k])
    }),
    sjoblom = osc_iterative(x, y, ncomp, tol, max.iter, weight_fun = function(x, t) {
      drop(crossprod(x, t))
    }),
    fearn = osc_fearn(x, y, ncomp)
  )

  comp <- paste0("comp", seq_len(ncol(out$t)))
  res <- list(
    "correction" = as_tbl(out$x, xy$names),
    "weights" = as_tbl(out$w, comp),
    "scores" = as_tbl(out$t, comp),
    "loadings" = as_tbl(out$p, comp),
    "angle" = osc_angle(out$t, y),
    "R2" = sum(out$x^2) / sum(x^2) * 100,
    "center" = xy$center,
    "scale" = xy$scale
  )
  return(res)
}

# Orthogonalizes a score vector with respect to y.
orthogonalize <- function(t, y) {
  t - y %*% solve_ls(y, t)
}

# Wold and Sjoblom OSC: iterate between orthogonalizing the score to y and
# re-expressing it as a linear combination of X, then deflate X.
osc_iterative <- function(x, y, ncomp, tol, max.iter, weight_fun) {
  n <- nrow(x)
  ws <- ps <- ts <- vector("list", ncomp)
  for (i in seq_len(ncomp)) {
    sv <- svd(x, nu = 1, nv = 0)
    t <- sv$u * sv$d[1]
    for (iter in seq_len(max.iter)) {
      t_orth <- orthogonalize(t, y)
      w <- weight_fun(x, t_orth)
      w <- w / sqrt(sum(w^2))
      t_new <- x %*% w
      dif <- sqrt(sum((t_new - t)^2) / sum(t_new^2))
      t <- t_new
      if (dif < tol) break
    }
    p <- crossprod(x, t) / sum(t^2)
    x <- x - tcrossprod(t, p)
    ws[[i]] <- w
    ps[[i]] <- p
    ts[[i]] <- t
  }
  list(
    x = x,
    w = do.call(cbind, ws),
    p = do.call(cbind, ps),
    t = do.call(cbind, ts)
  )
}

# Fearn OSC: maximize var(Xw) subject to Y'Xw = 0. With B = X'Y, the
# constrained matrix is Z = X M = X - XB(B'B)^-1 B'; its right singular
# vectors are the weights and T = XW is exactly orthogonal to y.
osc_fearn <- function(x, y, ncomp) {
  b <- crossprod(x, y)
  z <- x - (x %*% b) %*% MASS::ginv(crossprod(b)) %*% t(b)
  ncomp <- min(ncomp, qr(z)$rank)
  w <- svd(z, nu = 0, nv = ncomp)$v
  t <- x %*% w
  p <- crossprod(x, t) %*% solve(crossprod(t))
  list(x = x - tcrossprod(t, p), w = w, p = p, t = t)
}

osc_angle <- function(t, y) {
  cosines <- drop(crossprod(t, y)) / (sqrt(colSums(t^2)) * sqrt(sum(y^2)))
  mean(acos(pmin(pmax(cosines, -1), 1)) * 180 / pi)
}
