#' @title Least-Squares Polynomial
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function performs baseline correction on the spectral matrix by
#' estimating and removing the continuous background emission using least-squares
#' polynomial curve fitting approach.
#'
#' @details
#' This function implements the algorithm described in Lieber and
#' Mahadevan-Jansen (2003), which smoothes the spectrum in such a way that
#' peaks are automatically eliminated, leaving only the baseline to be
#' subtracted from the raw spectrum. The basis for this method is a
#' modified least-squares-based polynomial curve-fitting function, such that all
#' data points in the generated curve that have an intensity value higher than
#' their respective pixel value in the input spectrum are automatically
#' reassigned to the original intensity.
#'
#' @references
#'    - Lieber, C.A., Mahadevan-Jansen, A., (2003). Automated method for
#'      subtraction of fluorescence from biological Raman spectra.
#'      Applied Spectroscopy, 57(11):1363-1367
#'
#' @param x A matrix or data frame, with one spectrum per row.
#' @param degree An integer specifying the degree of the polynomial fitting
#' function. The default value is 4.
#' @param tol A numeric value representing the tolerance for the difference
#' between iterations. The default value is 1e-3.
#' @param max.iter An integer specifying the maximum number of iterations for the
#' algorithm. The default value is 10.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{correction}: The baseline-corrected spectral matrix.
#'   \item \code{background}: The fitted background emission.
#' }
#'
#' @export baseline_lsp
#'
#' @examples
#' wl <- seq(200, 400, length.out = 500)
#' spec <- 0.002 * (wl - 200)^2 + 50 * exp(-(wl - 300)^2 / 2) + rnorm(500, sd = 0.5)
#' res <- baseline_lsp(matrix(spec, nrow = 1), degree = 3, max.iter = 100)
#' plot(wl, spec, type = "l")
#' lines(wl, unlist(res$background), col = "red")
#'
baseline_lsp <- function(x, degree = 4, tol = 1e-3, max.iter = 10) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(degree) || length(degree) != 1) {
    stop("'degree' must be a single numeric value.")
  }
  if (!is.numeric(tol) || length(tol) != 1) {
    stop("'tol must be a single numeric value.")
  }
  if (!is.numeric(max.iter) || length(max.iter) != 1) {
    stop("'max.iter' must be a single numeric value.")
  }
  check_count(degree, "degree")
  check_count(max.iter, "max.iter")

  x <- as_numeric_matrix(x, "x")
  if (anyNA(x)) {
    stop("'x' contains missing values; remove or impute them before baseline correction.")
  }
  m <- ncol(x)
  if (degree >= m) {
    stop("'degree' must be smaller than the number of spectral points.")
  }

  # Orthonormal polynomial basis, shared by all spectra.
  basis <- cbind(1 / sqrt(m), stats::poly(seq_len(m), degree = degree))
  background <- t(apply(x, 1, lsp, basis = basis, tol = tol, max.iter = max.iter))
  if (m == 1) background <- t(background)

  baseline_result(x, background)
}

lsp <- function(x, basis, tol, max.iter) {
  z_d <- x
  for (i in seq_len(max.iter)) {
    z_p <- drop(basis %*% crossprod(basis, z_d))
    z_w <- pmin(x, z_p)
    crit <- sum(abs((z_w - z_d) / z_d), na.rm = TRUE)
    z_d <- z_w
    if (crit < tol) break
  }
  z_p
}
