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
#' Two iterative polynomial fits are available:
#'  - `"modpoly"`: the modified polynomial fit of Lieber and
#'    Mahadevan-Jansen (2003). A polynomial is fitted to the spectrum, the
#'    points of the spectrum above the fit are replaced by the fit, and the
#'    fit is repeated, so that the peaks are progressively removed. The first
#'    fits are pulled up by strong peaks, and the ends of the spectrum, where
#'    nothing pulls the polynomial back, can then fall far below the
#'    background.
#'  - `"imodpoly"` (default): the improved modified polynomial fit of Zhao
#'    *et al.* (2007). The points more than one standard deviation of the
#'    residuals above the first fit (the peaks) are left out of the
#'    following fits, and the remaining points are clipped at the fit plus
#'    the standard deviation of the residuals, rather than at the fit. The
#'    iterations stop when this standard deviation changes by less than
#'    `tol` (relative). The baseline follows the background up to the ends
#'    of the spectrum and passes through its noise.
#'
#' @references
#'    - Lieber, C.A., Mahadevan-Jansen, A., (2003). Automated method for
#'      subtraction of fluorescence from biological Raman spectra.
#'      Applied Spectroscopy, 57(11):1363-1367
#'    - Zhao, J., Lui, H., McLean, D.I., Zeng, H., (2007). Automated
#'      autofluorescence background subtraction algorithm for biomedical
#'      Raman spectroscopy. Applied Spectroscopy, 61(11):1225-1232.
#'
#' @param x A matrix or data frame, with one spectrum per row.
#' @param degree An integer specifying the degree of the polynomial fitting
#' function. The default value is 4.
#' @param tol A numeric value representing the tolerance for the difference
#' between iterations. The default value is 1e-3.
#' @param max.iter An integer specifying the maximum number of iterations for the
#' algorithm. The default value is 100.
#' @param method The algorithm: `"imodpoly"` (default) or `"modpoly"`. See
#' Details.
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
#' data(forageLIBS)
#' spectrum <- forageLIBS[1, -(1:14)]
#' wl <- as.numeric(names(spectrum))
#' in_region <- wl > 240 & wl < 300
#' region <- spectrum[in_region]
#' res <- baseline_lsp(region, degree = 4)
#' oldpar <- par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
#' # the spectrum and its fitted baseline
#' plot(wl[in_region], unlist(region), type = "l", col = "grey40", ylim = c(800, 3000),
#'      xlab = "Wavelength (nm)", ylab = "Counts", main = "Spectrum and baseline")
#' lines(wl[in_region], unlist(res$background), col = "red")
#' # the corrected spectrum: the background is now around zero
#' plot(wl[in_region], unlist(res$correction), type = "l", col = "grey40",
#'      ylim = c(-200, 2000), xlab = "Wavelength (nm)", ylab = "Counts",
#'      main = "Corrected spectrum")
#' abline(h = 0, col = "red", lty = 2)
#' par(oldpar)
baseline_lsp <- function(x, degree = 4, tol = 1e-3, max.iter = 100,
                         method = c("imodpoly", "modpoly")) {
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
  method <- match.arg(method)

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
  fit <- if (method == "imodpoly") imodpoly else modpoly
  background <- t(apply(x, 1, fit, basis = basis, tol = tol, max.iter = max.iter))
  if (m == 1) background <- t(background)

  baseline_result(x, background)
}

# Modified polynomial fit (Lieber and Mahadevan-Jansen, 2003).
modpoly <- function(x, basis, tol, max.iter) {
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

# Improved modified polynomial fit (Zhao et al., 2007). The basis is
# orthonormal, but not on a subset of its rows, hence the least-squares fit.
imodpoly <- function(x, basis, tol, max.iter) {
  fit <- function(y, keep) drop(basis %*% qr.coef(qr(basis[keep, , drop = FALSE]), y[keep]))
  keep <- rep(TRUE, length(x))
  p <- fit(x, keep)
  dev <- stats::sd(x - p)
  # peak removal: the points above the first fit plus one deviation
  keep <- x <= p + dev
  if (sum(keep) <= ncol(basis)) return(p)
  y <- x
  for (i in seq_len(max.iter)) {
    p <- fit(y, keep)
    dev_new <- stats::sd(y[keep] - p[keep])
    y[keep] <- pmin(y[keep], p[keep] + dev_new)
    if (!is.finite(dev_new) || dev_new == 0 || abs(dev_new - dev) / dev_new < tol) break
    dev <- dev_new
  }
  p
}
