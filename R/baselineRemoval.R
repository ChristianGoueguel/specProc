#' @title Function for Baseline Correction
#' @description This function perform the estimation and removal of the continuous background emission using a polynomial curve fitting function.
#' @author Christian L. Goueguel
#' @details Wrapper function from the baseline package.
#' @source Chad A. Lieber, Anita Mahadevan-Jansen, Applied Spectroscopy, 2003, Vol. 57, 11, 1363-1367.
#' @param .data Data frame of emission spectra
#' @param degree Degree of the polynomial fitting function (by default 4)
#' @param tol Tolerance of difference between iterations (by default 1e-3)
#' @param rep Maximum number of iterations (by default 100)
#' @return List containing:
#' @return \item{`spec`}{data frame of background subtracted spectra}
#' @return \item{`bkg`}{data frame of the modeled background}
#' @export baselineRemoval
baselineRemoval <- function(.data, degree = 4, tol = 1e-3, rep = 100) {
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("data must be a data frame or tibble.")
  }
  if (!is.numeric(degree)) {
    stop("degree must be numeric.")
  }
  if (!is.numeric(tol)) {
    stop("tol must be numeric.")
  }
  if (!is.numeric(rep)) {
    stop("rep must be numeric.")
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
    purrr::map_dfr(replaceWithZero)

  # Estimated background emission
  background <- bc_mod %>%
    purrr::pluck("baseline") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~wlength, dplyr::everything()) %>%
    purrr::map_dfr(replaceWithZero)

  res <- list("spec" = bc_spec, "bkg" = background)
  return(res)
  }
