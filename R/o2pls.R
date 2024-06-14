#' @title O2PLS
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Implements the O2PLS (Orthogonal to Partial Least Squares) algorithm.
#'
#' @details
#' The O2-PLS method handles situations where systematic X-orthogonal variation
#' in Y exists, and it is predictive in both ways, X → Y and Y → X. The basic
#' idea of O2-PLS is (a) to use multiple linear regression to estimate the pure
#' constituent profiles and (b) to divide the systematic part in X and Y into
#' two parts, one which is related to both X and Y (covarying) and one that is
#' not (orthogonal). For each matrix the latter is computed in a way that makes
#' it orthogonal to the other matrix, i.e. linearly independent. Thus the O2-PLS
#' model can be written as a factor analysis model where some factors (T) are
#' common to both X and Y.
#'
#' @references
#'    - Trygg, J., (2002).
#'      O2-PLS for qualitative and quantitative analysis in multivariate calibration.
#'      J. Chemom. 16(1):283–293.
#'
#' @param x A numeric matrix or data frame representing the predictor variables.
#' @param y A numeric matrix or data frame representing the response variables.
#' @param center A logical value indicating whether to mean-centered `x` and `y`. Default is `TRUE`.
#' @param scale A logical value indicating whether to scale `x` and `y`. Default is `FALSE`.
#' @param ncomp An integer representing the number of components. Default value is 10.
#' @param tol A numeric value representing the tolerance for convergence. The default value is 1e-5.
#'
#' @return An list containing the following components:
#'  - `correction`: The corrected matrix.
#'  - `scores`: The orthogonal scores matrix.
#'  - `loadings`: The orthogonal loadings matrix.
#'  - `weights`: The weights matrix.
#'
#' @export o2pls
#'
o2pls <- function(x, y, ncomp = 10, center = TRUE, scale = FALSE, tol = 1e-5) {

  requireNamespace("pls", quietly = TRUE)

  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' must be provided.")
  }
  if (nrow(x) != nrow(y)) {
    stop("Dimensions of 'x' and 'y' don't match.")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("'x' and 'y' cannot contain missing values.")
  }

  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }
  if (is.data.frame(y) || tibble::is_tibble(y)) {
    x <- as.matrix(y)
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

  plsModel <- pls::plsr(y ~ x, ncomp = ncomp, validation = "CV")
  k <- plsModel$coefficients[, , 1:ncomp]
  pcaModel <- stats::prcomp(k, center = FALSE)
  w <- pcaModel$x
  w <- w / sqrt(sum(w^2))
  t <- x %*% w %*% MASS::ginv(crossprod(w, w))

  ty_ortho <- py_ortho <- wy_ortho <- NULL

  repeat {
    p <- MASS::ginv(crossprod(t, t)) %*% crossprod(t, x)
    p <- t(p)
    z <- p - w %*% MASS::ginv(crossprod(w, w)) %*% crossprod(w, p)
    pcaModel <- stats::prcomp(z, center = FALSE)
    zt <- pcaModel$x
    zp <- pcaModel$rotation
    s <- sum(apply(zt %*% t(zp), 2, function(x) sum(x^2)) > tol)
    if (s == 0) {
      break
    }
    zt_s <- zt[ , 1:s, drop = FALSE]
    zp_s <- zp[ , 1:s, drop = FALSE]
    t_ortho <- x %*% zt_s %*% MASS::ginv(crossprod(zt_s, zt_s))
    p_ortho <- MASS::ginv(crossprod(t_ortho, t_ortho)) %*% crossprod(t_ortho, x)
    p_ortho <- t(p_ortho)
    xe <- x - tcrossprod(t_ortho, p_ortho)
    colnames(xe) <- colnames(x)

    ty_ortho_new <- y %*% zt_s %*% MASS::ginv(crossprod(zt_s, zt_s))
    py_ortho_new <- MASS::ginv(crossprod(ty_ortho_new, ty_ortho_new)) %*% crossprod(ty_ortho_new, y)
    py_ortho_new <- t(py_ortho_new)
    ty_ortho <- cbind(ty_ortho, ty_ortho_new)
    py_ortho <- cbind(py_ortho, py_ortho_new)
    wy_ortho <- cbind(wy_ortho, zt_s)

    t <- xe %*% w %*% MASS::ginv(crossprod(w, w))
    x <- xe
  }

  out <- structure(
    list(
      "correction" = xe,
      "scores" = ty_ortho,
      "loadings" = py_ortho,
      "weights" = list(x = w, y = wy_ortho)
    ),
    class = "o2pls"
  )

  return(out)
}


#' @export
print.o2pls <- function(x, ...) {
  cat("An object of class 'o2pls'\n\n")
  cat("Components:\n")
  cat("- correction: ", dim(x$correction), " matrix\n", sep = "")
  cat("- scores: ", dim(x$scores), " matrix\n", sep = "")
  cat("- loadings: ", dim(x$loadings), " matrix\n", sep = "")
  cat("- X weights: ", dim(x$weights$x), " matrix\n", sep = "")
  cat("- Y weights: ", dim(x$weights$y), " matrix\n", sep = "")
}


