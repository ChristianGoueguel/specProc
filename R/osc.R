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
#' three different methods for OSC: the original method proposed by Wold *et al.* (1998),
#' the method proposed by Sjöblom *et al.* (1998), and the method proposed by Fearn (2000).
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
#' @param y A vector of the response variable.
#' @param method A character string indicating the OSC method to use. Accepted values are `"wold"`, `"sjoblom"` and `"fearn"`. Default is `"sjoblom"`.
#' @param center A logical value indicating whether to mean-centered `x` and `y`. Default is `TRUE`.
#' @param scale A logical value indicating whether to scale `x` and `y`. Default is `FALSE`.
#' @param ncomp An integer representing the number of components, which defines how many times the entire process will be performed. Default value is 10.
#' @param tol A numeric value representing the tolerance for convergence. The default value is 1e-3.
#' @param max.iter An integer representing the maximum number of iterations. The default value is 10.
#'
#' @return An list containing the following components:
#'  - `correction`: The corrected matrix.
#'  - `scores`: The orthogonal scores matrix.
#'  - `loadings`: The orthogonal loadings matrix.
#'  - `weights`: The orthogonal weights matrix.
#'  - `R2`: The value of the explained variance.
#'  - `angle`: The value of the orthogonalization angle.
#'
#' @export osc
#'
osc <- function(x, y, method = "sjoblom", center = TRUE, scale = FALSE, ncomp = 10, tol = 1e-3, max.iter = 10) {

  requireNamespace("pls", quietly = TRUE)

  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' must be provided.")
  }
  if (nrow(x) != length(y)) {
    stop("Dimensions of 'x' and 'y' don't match.")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("'x' and 'y' cannot contain missing values.")
  }
  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }
  if (center && scale) {
    x <- scale(x, center = TRUE, scale = TRUE)
    y <- scale(y, center = TRUE, scale = TRUE)
  } else if (center) {
    x <- scale(x, center = TRUE, scale = FALSE)
    y <- scale(y, center = TRUE, scale = FALSE)
  } else if (scale) {
    x <- scale(x, center = FALSE, scale = TRUE)
    y <- scale(y, center = FALSE, scale = TRUE)
  }
  n <- nrow(x)
  p <- ncol(x)
  if (ncomp < 1 || ncomp > min(n - 1, p)) {
    ncomp <- min(n - 1, p)
  }
  method <- match.arg(method, c("wold", "sjoblom", "fearn"))
  osc_method <- switch(
    method,
    wold = wold,
    sjoblom = sjoblom,
    fearn = fearn
  )
  out <- osc_method(
    x = x,
    y = y,
    ncomp = ncomp,
    tol = tol,
    max.iter = max.iter
  )
  return(out)
}


# Wold’s OSC algorithm
wold <- function(x, y, ncomp, tol, max.iter) {
  x_original <- x
  ps <- ws<- ts <- vector("list", ncomp)
  for (i in seq_len(ncomp)) {
    pc <- stats::prcomp(x, center = FALSE)
    t <- pc$x[, 1]
    dif <- 1
    iter <- 0
    while (dif > tol && iter < max.iter) {
      iter <- iter + 1
      t_new <- t - y %*% MASS::ginv(crossprod(y, y)) %*% crossprod(y, t)
      plsFit <- pls::simpls.fit(x, t_new, ncomp, center = FALSE)
      w <- plsFit$coefficients[ , , ncomp]
      w <- w / sqrt(sum(w^2))
      t_new <- x %*% w
      dif <- sqrt(sum((t_new - t)^2) / sum(t_new^2))
      t <- t_new
    }
    p <- crossprod(t, x) %*% MASS::ginv(crossprod(t, t_new))
    x <- x - tcrossprod(t, p)
    ws[[i]] <- w
    ps[[i]] <- p
    ts[[i]] <- t
  }
  w_ortho <- do.call(cbind, ws)
  p_ortho <- do.call(cbind, ps)
  t_ortho <- do.call(cbind, ts)
  x_osc <- x_original - x_original %*% tcrossprod(w_ortho, p_ortho)

  R2 <- sum(x_osc^2) / sum(x_original^2) * 100
  angle <- crossprod(t_ortho, y)
  norm <- MASS::ginv(sqrt(apply(t_ortho^2, 2, sum) * sum(y^2)))
  angle <- t(angle) %*% t(norm)
  angle <- mean(acos(angle) * 180 / pi)

  res <- list(
    "correction" = tibble::as_tibble(x_osc),
    "weights" = tibble::as_tibble(w_ortho),
    "scores" = tibble::as_tibble(t_ortho),
    "loadings" = tibble::as_tibble(p_ortho),
    "angle" = angle,
    "R2" = R2
  )
  return(res)
}


# Sjoblom’s OSC algorithm
sjoblom <- function(x, y, ncomp, tol, max.iter) {
  x_original <- x
  ps <- ws <- ts <- vector("list", ncomp)
  for (i in seq_len(ncomp)) {
    pc <- stats::prcomp(x, center = FALSE)
    t <- pc$x[, 1]
    dif <- 1
    iter <- 0
    while (dif > tol && iter < max.iter) {
      iter <- iter + 1
      t_new <- t - y %*% MASS::ginv(crossprod(y, y)) %*% crossprod(y, t)
      w <- crossprod(x, t_new) %*% MASS::ginv(crossprod(t_new, t_new))
      w <- w / sqrt(sum(w^2))
      t_new <- x %*% w
      dif <- sqrt(sum((t_new - t)^2) / sum(t_new^2))
      t <- t_new
    }
    plsFit <- pls::simpls.fit(x, t, ncomp)
    w <- plsFit$coefficients[ , , ncomp]
    t <- x %*% w
    t <- t - y %*% MASS::ginv(crossprod(y, y)) %*% crossprod(y, t)
    p <- crossprod(x, t) %*% MASS::ginv(crossprod(t, t))
    x <- x - tcrossprod(t, p)
    ws[[i]] <- w
    ps[[i]] <- p
    ts[[i]] <- t
  }
  w_ortho <- do.call(cbind, ws)
  p_ortho <- do.call(cbind, ps)
  t_ortho <- do.call(cbind, ts)
  x_osc <- x_original - x_original %*% tcrossprod(w_ortho, p_ortho)

  R2 <- sum(x_osc^2) / sum(x_original^2) * 100
  angle <- crossprod(t_ortho, y)
  norm <- MASS::ginv(sqrt(apply(t_ortho^2, 2, sum) * sum(y^2)))
  angle <- t(angle) %*% t(norm)
  angle <- mean(acos(angle) * 180 / pi)

  res <- list(
    "correction" = tibble::as_tibble(x_osc),
    "weights" = tibble::as_tibble(w_ortho),
    "scores" = tibble::as_tibble(t_ortho),
    "loadings" = tibble::as_tibble(p_ortho),
    "angle" = angle,
    "R2" = R2
  )
  return(res)
}


# Fearn’s OSC algorithm
fearn <- function(x, y, ncomp, tol, max.iter) {
  x_original <- x
  ps <- ws <- ts <- vector("list", ncomp)
  m <- diag(row(x)) - crossprod(x, y) %*% MASS::ginv(crossprod(y, x) %*% crossprod(x, y)) %*% crossprod(y, x)
  z <- x %*% m
  decomp <- svd(t(z))
  u <- decomp$u
  s <- decomp$d
  v <- decomp$v
  g <- diag(s[1:ncomp])
  c <- v[, 1:ncomp, drop = FALSE]

  for (i in seq_len(ncomp)) {
    w_old <- rep(0, ncol(x))
    w_new <- rep(1, ncol(x))
    dif <- 1
    iter <- 0
    while (dif > tol && iter < max.iter) {
      iter <- iter + 1
      w_old <- w_new
      t_new <- c[, i] %*% g[i, i]
      p_new <- tcrossprod(x, t_new) / tcrossprod(t_new, t_new)
      w_new <- m %*% tcrossprod(x, p_new)
      dif <- sqrt(sum((w_new - w_old)^2) / sum(w_new^2))
    }
    ws[[i]] <- w_new
    ts[[i]] <- c[, i] %*% g[i, i]
    ps[[i]] <- tcrossprod(x, t[[i]]) / tcrossprod(t[[i]], t[[i]])
  }
  w_ortho <- do.call(cbind, ws)
  t_ortho <- do.call(cbind, ts)
  p_ortho <- do.call(cbind, ps)
  x_osc <- x - tcrossprod(t_ortho, p_ortho)

  R2 <- sum(x_osc^2) / sum(x_original^2) * 100
  angle <- crossprod(t_ortho, y)
  norm <- MASS::ginv(sqrt(apply(t_ortho^2, 2, sum) * sum(y^2)))
  angle <- t(angle) %*% t(norm)
  angle <- mean(acos(angle) * 180 / pi)

  res <- list(
    "correction" = tibble::as_tibble(x_osc),
    "weights" = tibble::as_tibble(w_ortho),
    "scores" = tibble::as_tibble(t_ortho),
    "loadings" = tibble::as_tibble(p_ortho),
    "angle" = angle,
    "R2" = R2
  )
  return(res)
}

