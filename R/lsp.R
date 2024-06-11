#' @title Least-Squares Polynomial Smoothing
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function performs baseline correction on the spectral matrix by
#' estimating and removing the continuous background emission using least-squares
#' polynomial curve fitting approach.
#'
#' @details
#' This function is a wrapper around the `baseline.modpolyfit` function from the
#' `baseline` package, which implements the algorithm described in Lieber and
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
#'   \item \code{continuum}: The fitted background emission.
#' }
#'
#' @export lsp
#'
lsp <- function(x, degree = 4, tol = 1e-3, max.iter = 10) {
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
    x <- x %>%
      dplyr::select(dplyr::where(is.numeric)) %>%
      as.matrix()
  }

  degree <- as.numeric(degree)
  tol <- as.numeric(tol)
  rep <- as.numeric(rep)
  wlength <- colnames(x)

  replaceWithZero <- function(x) {
    ifelse(x < 0, 0, x)
  }

  bc_mod <- baseline::baseline.modpolyfit(
    spectra = x,
    degree = degree,
    tol = tol,
    rep = max.iter
  )

  bc_spec <- bc_mod %>%
    purrr::pluck("corrected") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~wlength, dplyr::everything()) %>%
    purrr::map(replaceWithZero)

  background <- bc_mod %>%
    purrr::pluck("baseline") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~wlength, dplyr::everything()) %>%
    purrr::map(replaceWithZero)

  res <- list(
    "correction" = bc_spec,
    "continuum" = background
    )

  return(res)
  }
