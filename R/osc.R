#' @title Orthogonal Signal Correction
#'
#' @description
#' This function applies orthogonal signal correction (OSC) to remove systematic
#' variation in the X-data (predictors) that is orthogonal (unrelated) to the
#' Y-data (response). OSC is a preprocessing technique used in chemometrics
#' and multivariate data analysis to improve the interpretability and predictive
#' ability of regression models. It is particularly useful when the X-data
#' contains a large amount of variation that is not relevant to the Y-data,
#' which can mask the underlying signal of interest and lead to poor model performance.
#'
#' @details
#' The OSC algorithm identifies and removes the orthogonal variation in the
#' X-data by iteratively deflating the X-matrix with respect to the Y-data.
#' The resulting X-matrix contains only the variation that is relevant to the
#' Y-data, which can then be used for further modeling or analysis.
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
#' @param X A matrix or data.frame containing the X-data (predictors).
#' @param Y A vector containing the Y-data (response). The length of `Y` must
#' match the number of rows in `X`.
#' @param method A character string indicating the OSC method to use. Accepted
#' values are `"wold"` (Wold's original method), `"sjoblom"` (Sjöblom's method), or
#' `"wise"` (Wise and Gallagher's method).
#' @param center A logical value indicating whether to center the X-data before
#' applying OSC. If `TRUE`, the columns of X will be mean-centered.
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
#'   \item{X_corrected}{The corrected X-data after applying OSC.}
#'   \item{scores}{The X-scores after orthogonal variations are removed.}
#'   \item{loadings}{The loadings.}
#'   \item{weights}{The wights.}
#'   \item{R2}{The value of R2}
#' }
#'
#' @export osc
#' @examples
#' \dontrun{
#' osc_result <- osc(
#'   x_data, y_data,
#'   method = "wold",
#'   center = TRUE,
#'   osc.ncomp = 4,
#'   pls.ncomp = 10
#' )
#' }
osc <- function(X, Y, method = "wold", center = TRUE, osc.ncomp = 4, pls.ncomp = 10, tol = 1e-3, iter = 20) {
  if (missing(X) || missing(Y)) {
    stop("data set or response are missing")
  }
  if (nrow(X) != length(Y)) {
    stop("X and Y don't match.")
  }
  if (is.factor(Y) && length(unique(Y)) < 2) {
    stop("Classification needs at least two classes.")
  }
  if (any(is.na(X)) || any(is.na(Y))) {
    stop("NA is not permitted in data set or response.")
  }

  X <- as.matrix(X)
  Y <- as.vector(Y)
  n <- nrow(X)
  p <- ncol(X)

  if (pls.ncomp < 1 || pls.ncomp > min(n - 1, p)) {
    pls.ncomp <- min(n - 1, p)
  }

  method <- match.arg(method, c("wold", "sjoblom", "wise"))

  osc_method <- switch(
    method,
    wold = mt::osc_wold,
    sjoblom = mt::osc_sjoblom,
    wise = mt::osc_wise
  )

  out <- osc_method(
    X,
    Y,
    center = center,
    osc.ncomp = osc.ncomp,
    pls.ncomp = pls.ncomp,
    tol = tol,
    iter = iter
  )

  res <- list(
    X_corrected = out@X |> tibble::as_tibble(),
    R2 = out@R2,
    weights = out@W |> tibble::as_tibble(),
    loadings = out@p |> tibble::as_tibble(),
    scores = out@t |> tibble::as_tibble()
  )
  return(res)

}
