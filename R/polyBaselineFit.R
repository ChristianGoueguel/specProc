#' @title Polynomial Baseline Correction
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function performs baseline correction on the spectra matrix by
#' estimating and removing the continuous background emission using a polynomial
#' curve fitting approach.
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
#' @param data A matrix, data frame or tibble.
#' @param degree An integer specifying the degree of the polynomial fitting
#' function. The default value is 4.
#' @param tol A numeric value representing the tolerance for the difference
#' between iterations. The default value is 1e-3.
#' @param rep An integer specifying the maximum number of iterations for the
#' algorithm. The default value is 100.
#' @return A list with two elements:
#' \itemize{
#'   \item \code{correction}: The baseline-corrected spectral matrix.
#'   \item \code{continuum}: The fitted continuum emission.
#' }
#' @export polyBaselineFit
polyBaselineFit <- function(data, degree = 4, tol = 1e-3, rep = 100) {
  if (missing(data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.numeric(degree)) {
    stop("'degree' must be numeric.")
  }
  if (!is.numeric(tol)) {
    stop("'tol' must be numeric.")
  }
  if (!is.numeric(rep)) {
    stop("'rep' must be numeric.")
  }

  if (is.data.frame(data) && tibble::is_tibble(.data)) {
    X_mat <- data %>%
      dplyr::select(dplyr::where(is.numeric)) %>%
      as.matrix()
  } else {
    X_mat <- data
  }

  degree <- as.numeric(degree)
  tol <- as.numeric(tol)
  rep <- as.numeric(rep)
  wlength <- colnames(X_mat)

  replaceWithZero <- function(x) {
    ifelse(x < 0, 0, x)
  }

  bc_mod <- baseline::baseline.modpolyfit(
    spectra = X_mat,
    degree = degree,
    tol = tol,
    rep = rep
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
