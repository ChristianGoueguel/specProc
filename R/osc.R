#' Orthogonal Signal Correction (OSC) Function
#'
#' This function applies Orthogonal Signal Correction (OSC) to the provided x and y data using a specified method.
#'
#' @author Christian L. Goueguel
#' @param x A matrix or data.frame containing the X-data (predictors).
#' @param y A factor or vector containing the Y-data (class labels or continuous response).
#' @param method A character string indicating the OSC method to use ("wold", "sjoblom", or "wise").
#' @param center A logical value indicating whether to center the data.
#' @param osc.ncomp An integer representing the number of OSC components.
#' @param pls.ncomp An integer representing the number of PLS components.
#' @param tol A numeric value representing the tolerance for convergence.
#' @param iter An integer representing the maximum number of iterations.
#' @param ... Additional arguments to be passed to the OSC algorithm function.
#'
#' @source \item{(1)}{Sjoblom. J., Svensson, O., Josefson, M., Kullberg, H., Wold, S. (1998). An evaluation of orthogonal signal correction applied to calibration transfer of near infrared spectra. Chemometrics Intell. Lab. Syst.,44: 229-244.}
#' @source \item{(2)}{Wise, B. M. and Gallagher, N.B. http://www.eigenvector.com/MATLAB/OSC.html.}
#' @source \item{(3)}{Wold, S., Antti, H., Lindgren, F., Ohman, J.(1998). Orthogonal signal correction of nearinfrared spectra. Chemometrics Intell. Lab. Syst., 44: 175-185.}
#' @source \item{(4)}{Svensson, O., Kourti, T. and MacGregor, J.F. (2002). An investigation of orthogonal correction algorithms and their characteristics. Journal of Chemometrics, 16:176-188.}
#' @source \item{(5)}{Westerhuis, J. A., de Jong, S., Smilde, A, K. (2001). Direct orthogonal signal correction. Chemo- metrics Intell. Lab. Syst., 56: 13-25.}
#'
#' @return An object of class "osc" containing the results of the OSC algorithm applied to the input data.
#' @export ocs
#'
#' @examples
#' \dontrun{
#' osc_result <- osc(x_data, y_data, method = "wold", center = TRUE, osc.ncomp = 4, pls.ncomp = 10)
#' }
osc <- function(x, y, method = "wold", center = TRUE, osc.ncomp = 4, pls.ncomp = 10, tol = 1e-3, iter = 20, ...) {
  requireNamespace("mt", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)

  #' arguments validity checking
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

  #' initialization
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
    wold = mt::osc_wold,
    sjoblom = mt::osc_sjoblom,
    wise = mt::osc_wise
  )

  #' call OSC algorithm
  out <- osc_method(
    x,
    y,
    center = center,
    osc.ncomp = osc.ncomp,
    pls.ncomp = pls.ncomp,
    tol = tol,
    iter = iter,
    ...
  )

  res <- list(
    X_corrected = out@X |> tibble::as_tibble(),
    R2 = out@R2,
    angle = out@angle,
    weights = out@W |> tibble::as_tibble(),
    loadings = out@p |> tibble::as_tibble(),
    scores = out@t |> tibble::as_tibble(),
    center = out@center
  )
  return(res)

}
