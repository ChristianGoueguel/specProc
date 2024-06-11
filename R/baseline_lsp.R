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
#' Mahadevan-Jansen (2003).
#'
#' @references
#'    - Lieber, C.A., Mahadevan-Jansen, A., (2003). Automated method for
#'      subtraction of fluorescence from biological Raman spectra.
#'      Applied Spectroscopy, 57(11):1363-1367
#'
#' @param x A matrix or data frame.
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

  if (is.data.frame(x) && tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }

  n <- nrow(x)
  m <- ncol(x)
  correctedData <- matrix(nrow = n, ncol = m)
  baseline <- matrix(nrow = n, ncol = m)

  for (i in 1:n) {
    rowData <- x[i, ]
    rowBaseline <- lsp(rowData, degree, tol, max.iter)
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

lsp <- function(x, degree, tol, max.iter) {
  n <- length(x)
  z <- rep(0, n)
  p <- cbind(1 / sqrt(n), stats::poly(1:n, degree = degree))
  z_w <- z_d <- z_g <- x
  i <- 0
  repeat {
    i <- i + 1
    z_p <- p %*% crossprod(p, z_d)
    z_w <- pmin(z_g, z_p)
    crit <- sum(abs((z_w - z_d) / z_d), na.rm = TRUE)
    if (crit < tol || i > max.iter)
      break
    z_d <- z_w
  }
  z <- z_p

  return(z)
}
