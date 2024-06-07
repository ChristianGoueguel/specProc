#' @title Orthogonal Signal Correction
#'
#' @description
#' Orthogonal signal correction (OSC) removes systematic variation in the
#' spectral matrix that is orthogonal to the response vector.
#'
#' @details
#' The OSC algorithm identifies and removes the orthogonal variation in the
#' x-data by iteratively deflating the x-matrix with respect to the y-data.
#' The resulting x-matrix contains only the variation that is relevant to the
#' y-data, which can then be used for further modeling or analysis.
#' This function implements three different methods for OSC: the original
#' method proposed by Wold et al. (1998), the method proposed by Sjöblom et al.
#' (1998), and the method proposed by Wise and Gallagher.
#'
#' @references
#'  - Sjoblom, J., Svensson, O., Josefson, M., Kullberg, H., Wold, S. (1998).
#'    An evaluation of orthogonal signal correction applied to calibration transfer
#'    of near infrared spectra. Chemometrics Intell. Lab. Syst., 44(1):229-244.
#'  - Wise, B. M. and Gallagher, N.B. http://www.eigenvector.com/MATLAB/OSC.html.
#'  - Wold, S., Antti, H., Lindgren, F., Ohman, J. (1998). Orthogonal signal
#'    correction of near-infrared spectra. Chemometrics Intell. Lab. Syst., 44(1):175-185.
#'  - Svensson, O., Kourti, T. and MacGregor, J.F. (2002). An investigation of
#'    orthogonal correction algorithms and their characteristics. Journal of Chemometrics, 16(1):176-188.
#'  - Westerhuis, J. A., de Jong, S., Smilde, A. K. (2001). Direct orthogonal
#'    signal correction. Chemometrics Intell. Lab. Syst., 56(1):13-25.
#'
#' @author Christian L. Goueguel
#'
#' @param x A matrix or data.frame containing the x-data (predictors).
#' @param y A vector containing the y-data (response). The length of `y` must
#' match the number of rows in `x`.
#' @param method A character string indicating the OSC method to use. Accepted
#' values are `"wold"` (Wold's original method), `"sjoblom"` (Sjöblom's method), or
#' `"wise"` (Wise and Gallagher's method).
#' @param center A logical value indicating whether to center the x-data before
#' applying OSC. If `TRUE`, the columns of x will be mean-centered.
#' @param osc.ncomp An integer representing the number of OSC components to be
#' calculated. The default value is 4.
#' @param pls.ncomp An integer representing the number of PLS components to be
#' calculated. The default value is 10.
#' @param tol A numeric value representing the tolerance for convergence of the
#' OSC algorithm. The default value is 1e-3.
#' @param iter An integer representing the maximum number of iterations for the
#' OSC algorithm. The default value is 20.
#'
#' @return An list containing the following components:
#' \describe{
#'   \item{x_corrected}{The corrected x-data after applying OSC.}
#'   \item{scores}{The x-scores after orthogonal variations are removed.}
#'   \item{loadings}{The loadings.}
#'   \item{weights}{The wights.}
#'   \item{R2}{The value of R2}
#' }
#'
#' @export osc
#'
osc <- function(x, y, method = "wold", center = TRUE, osc.ncomp = 4, pls.ncomp = 10, tol = 1e-3, iter = 20) {
  if (missing(x) || missing(y)) {
    stop("data set or response are missing")
  }
  if (nrow(x) != length(y)) {
    stop("x and y don't match.")
  }
  if (is.factor(y) && length(unique(y)) < 2) {
    stop("Classification needs at least two classes.")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("NA is not permitted in data set or response.")
  }

  x <- as.matrix(x)
  y <- as.vector(y)
  n <- nrow(x)
  p <- ncol(x)

  if (pls.ncomp < 1 || pls.ncomp > min(n - 1, p)) {
    pls.ncomp <- min(n - 1, p)
  }

  method <- match.arg(method, c("wold", "sjoblom", "wise"))

  osc_method <- switch(
    method,
    wold = wold,
    sjoblom = sjoblom,
    wise = wise
  )

  out <- osc_method(
    x,
    y,
    center = center,
    osc.ncomp = osc.ncomp,
    pls.ncomp = pls.ncomp,
    tol = tol,
    iter = iter
  )

  res <- list(
    x_corrected = out@x |> tibble::as_tibble(),
    R2 = out@R2,
    weights = out@W |> tibble::as_tibble(),
    loadings = out@p |> tibble::as_tibble(),
    scores = out@t |> tibble::as_tibble()
  )
  return(res)

}
