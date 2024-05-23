#' @title Polynomial Baseline Correction
#'
#' @description This function performs baseline correction on emission spectra data by estimating and removing the continuous background emission using a polynomial curve fitting approach.
#'
#' @details This function is a wrapper around the `baseline.modpolyfit` function from the `baseline` package, which implements the algorithm described in Chad A. Lieber, Anita Mahadevan-Jansen. (2003). "Automated method for subtraction of fluorescence from biological Raman spectra." Applied Spectroscopy, 57(11), 1363-1367. https://doi.org/10.1366/000370203322554518
#'
#' @author Christian L. Goueguel
#' @param .data A data frame or tibble containing the emission spectra data. Each column should represent a separate emission spectrum.
#' @param degree An integer specifying the degree of the polynomial fitting function. The default value is 4.
#' @param tol A numeric value representing the tolerance for the difference between iterations. The default value is 1e-3.
#' @param rep An integer specifying the maximum number of iterations for the algorithm. The default value is 100.
#' @return A list with two elements:
#' \itemize{
#'   \item \code{spec}: A data frame containing the baseline-corrected emission spectra.
#'   \item \code{bkg}: A data frame containing the estimated background emission.
#' }
#' @export baselineRemoval
baselineRemoval <- function(.data, degree = 4, tol = 1e-3, rep = 100) {
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("'data' must be a data frame or tibble.")
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

  X_mat <- .data %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    as.matrix()

  degree <- as.numeric(degree)
  tol <- as.numeric(tol)
  rep <- as.numeric(rep)
  wlength <- colnames(X_mat)

  replaceWithZero <- function(x) {
    ifelse(x < 0, 0, x)
  }

  # Background fitting
  bc_mod <- baseline::baseline.modpolyfit(
    spectra = X_mat,
    degree = degree,
    tol = tol,
    rep = rep
  )

  # Background subtracted spectra
  bc_spec <- bc_mod %>%
    purrr::pluck("corrected") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~wlength, dplyr::everything()) %>%
    purrr::map(replaceWithZero)

  # Estimated background emission
  background <- bc_mod %>%
    purrr::pluck("baseline") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~wlength, dplyr::everything()) %>%
    purrr::map(replaceWithZero)

  res <- list("spec" = bc_spec, "bkg" = background)
  return(res)
  }
