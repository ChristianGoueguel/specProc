#' @title Asymmetric Least Squares
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Baseline correction based on asymmetric least squares (ALS) algorithm
#' as proposed by Eilers *et al.* (2005).
#'
#' @details
#' The function applies Eilers' method based on a Whittaker filter. The algorithm
#' estimates a baseline curve by minimizing the asymmetric least squares criterion,
#' which allows for different weights for positive and negative residuals. The
#' resulting baseline curve is subtracted from the input data, providing a
#' baseline-corrected version.
#'
#' @references
#'  - Eilers, P.H.C., Boelens, H.F.M., (2005).
#'    Baseline correction with asymmetric least squares smoothing.
#'    Leiden University Medical Centre report.
#'
#' @param x A numeric matrix or data frame.
#' @param lambda A numeric value specifying the smoothing parameter, which
#' controls the amount of curvature allowed for the baseline. The smaller the
#' lambda, the more curvature in the baseline fitting. Default is 1000.
#' @param p A numeric value specifying the extent of asymmetry required of the
#' fit. Larger values allow more negative-going regions. Smaller values disallow
#' negative-going regions. `p` must be between 0 and 1. Default is 0.001.
#' @param max.iter Maximum number of iterations for the algorithm. Default is 10.
#'
#' @return A list containing two tibbles:
#' \itemize{
#'   \item \code{correction}: The baseline-corrected spectral matrix.
#'   \item \code{background}: The fitted background emission.
#' }
#'
#' @export whittaker
#'
whittaker <- function(x, lambda = 1e3, p = 0.001, max.iter = 10){

  if(missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(lambda) || length(lambda) != 1) {
    stop("'lambda' must be a single numeric value.")
  }
  if (!is.numeric(p) || length(p) != 1) {
    stop("'p' must be a single numeric value.")
  }
  if (p <= 0 || p >= 1) {
    stop("'p' must be between 0 and 1")
  }
  if (!is.numeric(max.iter) || length(max.iter) != 1) {
    stop("'max.iter' must be a single numeric value.")
  }

  if (is.data.frame(x) || tibble::as_tibble(x)) {
    x <- as.matrix(x)
  }

  n <- nrow(x)
  m <- ncol(x)
  correctedData <- matrix(nrow = n, ncol = m)
  baseline <- matrix(nrow = n, ncol = m)

  for (i in 1:n) {
    rowData <- x[i, ]
    rowBaseline <- als(rowData, lambda, p, max.iter)
    correctedData[i, ] <- rowData - rowBaseline
    baseline[i, ] <- rowBaseline
  }
  wlength <- colnames(x)
  replaceWithZero <- function(x) {
    ifelse(x < 0, 1, x)
  }
  correctedData <- correctedData %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~wlength, dplyr::everything()) %>%
    purrr::map(replaceWithZero)

  baseline <- tibble::as_tibble(baseline) %>%
    dplyr::rename_with(~wlength, dplyr::everything()) %>%
    purrr::map(replaceWithZero)

  res <- list(
    "correction" = correctedData,
    "background" = baseline
    )

  return(res)
}

als <- function(x, lambda, p, max.iter) {
  n <- length(x)
  d <- diff(diag(n), differences = 2)
  w <- rep(1, n)
  for (i in 1:max.iter) {
    wd <- diag(w)
    h <- wd + lambda * d %*% t(d)
    z <- solve(h, w * x)
    w <- p * (x > z) + (1 - p) * (x < z)
  }
  return(z)
}


