#' @title Piecewise Direct Standardization
#'
#' @author Christian L. Goueguel
#'
#' @description
#' The `pds` function performs Piecewise Direct Standardization (PDS), a method
#' proposed by Wang *et al.* (1991) to transfer spectra from
#' one instrument to another. Optionally, the local models can be blended with
#' a global PLSR model (direct standardization), which can lead to better
#' transfer performance when the instrumental differences are not purely local.
#'
#' @details
#' For each wavelength \eqn{j} of the standard (master) instrument, a PLSR model
#' is fitted between the response of the master instrument at \eqn{j} and the
#' responses of the slave instrument in the window \eqn{[j - win, j + win]}
#' (truncated at the edges of the spectrum). The regression coefficients are
#' stored in a banded transfer matrix \eqn{\textbf{F}}, and the intercepts in a
#' vector \eqn{\textbf{b}_0}. Spectra measured on the slave instrument are then
#' standardized as
#' \deqn{\textbf{X}_{2,std} = \textbf{X}_2 \textbf{F} + \textbf{1}\textbf{b}_0^T}
#'
#' When `alpha > 0`, a global PLSR model mapping the whole slave spectrum to
#' the whole master spectrum is also fitted, and the transfer matrix and
#' intercept are \eqn{(1 - \alpha)} times the local ones plus \eqn{\alpha}
#' times the global ones.
#'
#' @param x1 A matrix or data frame containing spectra acquired with the standard (master) instrument.
#' @param x2 A matrix or data frame containing spectra of the same samples acquired with the instrument to be standardized (slave).
#' @param win An integer specifying the half size of the moving window used for PLSR. A larger value may improve the transfer but will increase computational time. Default is 5.
#' @param ncomp An integer specifying the number of components to be used in PLSR. Typically, a small number (e.g., 2-5) is sufficient. Default is 2.
#' @param alpha A numeric value between 0 and 1 specifying the weight for the global PLSR model. A value of 0 (default) corresponds to the original PDS method, while a value of 1 corresponds to using only the global PLSR model.
#'
#' @return A list with two components:
#'   \item{`transfer_matrix`}{The \eqn{p \times p} transfer matrix \eqn{\textbf{F}}.}
#'   \item{`intercept`}{A vector of length \eqn{p} containing the intercepts.}
#'
#' @references
#'   - Wang, Y., Veltkamp, D.J., Kowalski, B.R., (1991).
#'     Multivariate instrument standardization.
#'     Analytical Chemistry, 63(23):2750-2756.
#'   - Bouveresse, E., Massart, D.L., (1996).
#'     Improvement of the piecewise direct standardization procedure for the
#'     transfer of NIR spectra for multivariate calibration.
#'     Chemometrics and Intelligent Laboratory Systems, (32)2:201-213.
#'
#' @export pds
#'
#' @examples
#' set.seed(1)
#' wl <- seq(0, 1, length.out = 40)
#' x1 <- t(replicate(15, runif(1) * dnorm(wl, 0.5, 0.1) + runif(1)))
#' x2 <- 1.1 * x1 + 0.05                 # slave instrument: gain and offset
#' model <- pds(x1, x2, win = 3, ncomp = 2)
#' x2_std <- x2 %*% model$transfer_matrix +
#'   matrix(model$intercept, nrow(x2), ncol(x2), byrow = TRUE)
#' max(abs(x2_std - x1))
#'
pds <- function(x1, x2, win = 5, ncomp = 2, alpha = 0) {
  x1 <- as_numeric_matrix(x1, "x1")
  x2 <- as_numeric_matrix(x2, "x2")
  if (ncol(x1) != ncol(x2)) {
    stop("Input matrices x1 and x2 must have the same number of columns.")
  }
  if (nrow(x1) != nrow(x2)) {
    stop("Input matrices x1 and x2 must have the same number of rows (same samples).")
  }
  if (anyNA(x1) || anyNA(x2)) {
    stop("'x1' and 'x2' cannot contain missing values.")
  }
  check_count(win, "win", lower = 0)
  check_count(ncomp, "ncomp")
  check_number(alpha, "alpha", lower = 0, upper = 1)

  n <- nrow(x1)
  p <- ncol(x1)
  if (n < 3) {
    stop("At least 3 transfer samples are required.")
  }

  mu1 <- colMeans(x1)
  mu2 <- colMeans(x2)
  x1c <- sweep(x1, 2, mu1)
  x2c <- sweep(x2, 2, mu2)

  transfer <- matrix(0, p, p)
  if (alpha < 1) {
    for (j in seq_len(p)) {
      idx <- max(1, j - win):min(p, j + win)
      transfer[idx, j] <- pls_coef(x2c[, idx, drop = FALSE], x1c[, j], ncomp)
    }
  }
  if (alpha > 0) {
    global <- pls_coef(x2c, x1c, ncomp)
    transfer <- (1 - alpha) * transfer + alpha * global
  }
  intercept <- mu1 - drop(mu2 %*% transfer)

  dimnames(transfer) <- list(colnames(x2), colnames(x1))
  names(intercept) <- colnames(x1)
  list(transfer_matrix = transfer, intercept = intercept)
}

# Regression coefficients of a PLS model fitted on centered data.
pls_coef <- function(x, y, ncomp) {
  y <- as.matrix(y)
  if (all(abs(y) < .Machine$double.eps) || all(abs(x) < .Machine$double.eps)) {
    return(matrix(0, ncol(x), ncol(y)))
  }
  k <- min(ncomp, ncol(x), nrow(x) - 1, qr(x)$rank)
  fit <- pls::simpls.fit(x, y, ncomp = k, center = FALSE)
  coef <- fit$coefficients[, , k]
  matrix(coef, ncol(x), ncol(y))
}
