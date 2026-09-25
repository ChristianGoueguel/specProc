#' @title Modified Orthogonal Projections to Latent Structures
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the modified orthogonal projections to latent
#' structures (O2PLS) algorithm as proposed by Trygg (2002). The OPLS and O2PLS
#' methods differ as follows: OPLS is unidirectional \eqn{(X \Rightarrow Y)},
#' meaning that only orthogonal variations in the \eqn{X}-space are filtered out.
#' Whilst O2PLS is bi-directional \eqn{(X \Leftrightarrow Y)}, meaning that
#' orthogonal variations in both the \eqn{X}- and \eqn{Y}-space are filtered out.
#'
#' @details
#' The O2PLS method handles situations where systematic \eqn{X}-orthogonal variation
#' in \eqn{Y} exists, and it is predictive in both ways, \eqn{(X \Rightarrow Y)}
#' and \eqn{(Y \Rightarrow X)}. The systematic part in \eqn{X} and \eqn{Y} is
#' divided into two parts, one which is related to both \eqn{X} and \eqn{Y}
#' (joint/covarying) and one that is not (orthogonal).
#'
#' The algorithm follows Trygg and Wold (2003):
#' 1. The joint weights \eqn{\textbf{W}} and \eqn{\textbf{C}} are the dominant
#'    singular vectors of \eqn{\textbf{Y}^T\textbf{X}}.
#' 2. For each \eqn{Y}-orthogonal component of \eqn{X}, the weight
#'    \eqn{\textbf{w}_{o}} is the dominant singular vector of
#'    \eqn{\textbf{E}_{XY}^T\textbf{T}} (with \eqn{\textbf{T} = \textbf{XW}} and
#'    \eqn{\textbf{E}_{XY} = \textbf{X} - \textbf{TW}^T}), and \eqn{\textbf{X}}
#'    is deflated by \eqn{\textbf{t}_o\textbf{p}_o^T}.
#' 3. The same procedure is applied to \eqn{Y} for the \eqn{X}-orthogonal components.
#' 4. The joint components are recomputed from the filtered data.
#'
#' For a single response variable, `ny` must be 0 and the method is equivalent
#' to OPLS with `nx` orthogonal components.
#'
#' @references
#'    - Trygg, J., (2002).
#'      O2-PLS for qualitative and quantitative analysis in multivariate calibration.
#'      J. Chemom. 16(1):283–293.
#'    - Trygg, J., Wold, S., (2003).
#'      O2-PLS, a two-block (X–Y) latent variable regression (LVR) method with an integral OSC filter.
#'      J. Chemom. 17(1):53–64.
#'
#' @param x A numeric matrix or data frame representing the predictor variables.
#' @param y A numeric vector, matrix or data frame representing the response variables.
#' @param ncomp An integer giving the number of joint (predictive) components. Default is 1.
#' @param nx An integer giving the number of \eqn{Y}-orthogonal components removed from `x`. Default is 1.
#' @param ny An integer giving the number of \eqn{X}-orthogonal components removed from `y`. Default is 0.
#' @param center A logical value indicating whether to mean-centered `x` and `y`. Default is `TRUE`.
#' @param scale A logical value indicating whether to scale `x` and `y`. Default is `FALSE`.
#'
#' @return An object of class `o2pls`, a list containing:
#'  - `correction`: The filtered `x` (\eqn{Y}-orthogonal variation removed).
#'  - `correction_y`: The filtered `y` (\eqn{X}-orthogonal variation removed).
#'  - `scores`: A list with the joint scores `x` (\eqn{\textbf{T}}) and `y` (\eqn{\textbf{U}}), and the orthogonal scores `x_ortho` and `y_ortho`.
#'  - `loadings`: A list with the joint loadings `x` and `y`, and the orthogonal loadings `x_ortho` and `y_ortho`.
#'  - `weights`: A list with the joint weights `x` (\eqn{\textbf{W}}) and `y` (\eqn{\textbf{C}}), and the orthogonal weights `x_ortho` and `y_ortho`.
#'  - `center`, `scale`: The column centers and scales applied to `x`.
#'
#' @export o2pls
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(30 * 40), 30, 40)
#' y <- x[, 1:2] %*% c(1, -1) + rnorm(30, sd = 0.1)
#' fit <- o2pls(x, y, ncomp = 1, nx = 2)
#' fit
#'
o2pls <- function(x, y, ncomp = 1, nx = 1, ny = 0, center = TRUE, scale = FALSE) {

  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' must be provided.")
  }
  check_count(ncomp, "ncomp")
  check_count(nx, "nx", lower = 0)
  check_count(ny, "ny", lower = 0)

  xy <- prepare_xy(x, y, center, scale)
  x <- xy$x
  y <- xy$y
  if (ncomp > min(ncol(x), ncol(y), nrow(x) - 1)) {
    stop("'ncomp' cannot exceed the number of response variables, predictors or observations - 1.")
  }
  if (ncomp + nx > min(nrow(x) - 1, ncol(x))) {
    stop("'ncomp + nx' cannot exceed min(n - 1, number of predictors).")
  }
  if (ncomp + ny > ncol(y)) {
    stop("'ncomp + ny' cannot exceed the number of response variables.")
  }

  joint <- function(x, y) {
    s <- svd(crossprod(y, x), nu = ncomp, nv = ncomp)
    list(w = s$v, c = s$u, t = x %*% s$v, u = y %*% s$u)
  }

  # Removes one block's orthogonal components (Trygg & Wold, 2003).
  remove_ortho <- function(m, w, k) {
    wo <- to <- po <- NULL
    for (i in seq_len(k)) {
      t <- m %*% w
      e <- m - tcrossprod(t, w)
      w_o <- svd(crossprod(e, t), nu = 1, nv = 0)$u
      t_o <- m %*% w_o
      p_o <- crossprod(m, t_o) / sum(t_o^2)
      m <- m - tcrossprod(t_o, p_o)
      wo <- cbind(wo, w_o)
      to <- cbind(to, t_o)
      po <- cbind(po, p_o)
    }
    list(m = m, w = wo, t = to, p = po)
  }

  j0 <- joint(x, y)
  ox <- remove_ortho(x, j0$w, nx)
  oy <- remove_ortho(y, j0$c, ny)
  j <- joint(ox$m, oy$m)

  p_joint <- crossprod(ox$m, j$t) %*% solve(crossprod(j$t))
  q_joint <- crossprod(oy$m, j$u) %*% solve(crossprod(j$u))

  comp <- paste0("comp", seq_len(ncomp))
  named <- function(m, prefix) {
    if (is.null(m)) return(NULL)
    colnames(m) <- paste0(prefix, seq_len(ncol(m)))
    m
  }

  out <- structure(
    list(
      "correction" = as_tbl(ox$m, xy$names),
      "correction_y" = as_tbl(oy$m, colnames(y)),
      "scores" = list(
        x = named(j$t, "comp"), y = named(j$u, "comp"),
        x_ortho = named(ox$t, "ortho"), y_ortho = named(oy$t, "ortho")
      ),
      "loadings" = list(
        x = named(p_joint, "comp"), y = named(q_joint, "comp"),
        x_ortho = named(ox$p, "ortho"), y_ortho = named(oy$p, "ortho")
      ),
      "weights" = list(
        x = named(j$w, "comp"), y = named(j$c, "comp"),
        x_ortho = named(ox$w, "ortho"), y_ortho = named(oy$w, "ortho")
      ),
      "center" = xy$center,
      "scale" = xy$scale
    ),
    class = "o2pls"
  )
  return(out)
}


#' @export
print.o2pls <- function(x, ...) {
  dims <- function(m) if (is.null(m)) "none" else paste(dim(m), collapse = " x ")
  cat("An object of class 'o2pls'\n\n")
  cat("Joint components:        ", ncol(x$scores$x), "\n", sep = "")
  cat("X-orthogonal components: ", NCOL(x$scores$x_ortho) * !is.null(x$scores$x_ortho), "\n", sep = "")
  cat("Y-orthogonal components: ", NCOL(x$scores$y_ortho) * !is.null(x$scores$y_ortho), "\n\n", sep = "")
  cat("- correction:  ", dims(x$correction), "\n", sep = "")
  cat("- X weights:   ", dims(x$weights$x), "\n", sep = "")
  cat("- Y weights:   ", dims(x$weights$y), "\n", sep = "")
  cat("- X scores:    ", dims(x$scores$x), "\n", sep = "")
  cat("- X ortho:     ", dims(x$scores$x_ortho), "\n", sep = "")
  invisible(x)
}
