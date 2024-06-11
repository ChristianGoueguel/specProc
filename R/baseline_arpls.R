#' @title Asymmetrically Reweighted Penalized Least Squares
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Baseline correction based on asymmetrically reweighted penalized least
#' squares smoothing algorithm as proposed by Baek *et al.* (2015).
#'
#' @details
#' The algorithm estimates a baseline curve by iteratively updating weights based
#' on the residuals and minimizing a penalized least squares criterion. The
#' resulting baseline curve is subtracted from the input data, providing a
#' baseline-corrected version.
#'
#' @references
#'  - Baek, S.-J., Park, A., Ahn, Y.-J., Choo, J., (2015).
#'    Baseline correction using asymmetrically reweighted penalized least
#'    squares smoothing.
#'    Analyst, 140(1):250–257.
#'
#' @param x A numeric matrix or data frame.
#' @param lambda A numeric value specifying the smoothing parameter, which
#' controls the amount of curvature allowed for the baseline. The smaller the
#' lambda, the more curvature in the baseline fitting. Default is 1000.
#' @param ratio A numeric value specifying the convergence ratio for the
#' iterative algorithm. The algorithm stops when the relative change in the
#' weights is less than this ratio. Typical values are between 0.01 and 0.1.
#' Default is 0.05.
#' @param max.iter Maximum number of iterations for the algorithm. Default is 10.
#'
#' @return A list containing two tibbles:
#' \itemize{
#'   \item \code{correction}: The baseline-corrected spectral matrix.
#'   \item \code{background}: The fitted background emission.
#' }
#'
#' @export baseline_arpls
#'
baseline_arpls <- function(x, lambda = 1e3, ratio = 0.05, max.iter = 10) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(lambda) || length(lambda) != 1) {
    stop("'lambda' must be a single numeric value.")
  }
  if (!is.numeric(ratio) || length(ratio) != 1) {
    stop("'ratio' must be a single numeric value.")
  }
  if (!is.numeric(max.iter) || length(max.iter) != 1) {
    stop("'max.iter' must be a single numeric value.")
  }

  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }

  n <- nrow(x)
  m <- ncol(x)
  correctedData <- matrix(nrow = n, ncol = m)
  baseline <- matrix(nrow = n, ncol = m)

  for (i in 1:n) {
    rowData <- x[i, ]
    rowBaseline <- arpls(rowData, lambda, ratio, max.iter)
    correctedData[i, ] <- rowData - rowBaseline
    baseline[i, ] <- rowBaseline
  }
  wlength <- colnames(x)
  replaceWithZero <- function(x) {
    ifelse(x < 0, 0, x)
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

arpls <- function(x, lambda, ratio, max.iter) {
  n <- length(x)
  d <- diff(diag(n), differences = 2)
  h <- lambda * t(d) %*% d
  w <- rep(1, n)
  for (i in 1:max.iter) {
    wd <- diag(w)
    c <- chol(wd + h)
    z <- c %/% (c %/% (wd %*% x))
    d <- x - z
    dn <- d[d < 0]
    m <- mean(dn)
    s <- stats::sd(dn)
    wt <- 1 / (1 + exp(2 * (d - (2 * s - m)) / s))
    if (norm(w - wt, type = "2") / norm(w, type = "2") < ratio) {
      break
    }
    w <- wt
  }
  return(z)
}
