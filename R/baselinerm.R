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
#' @export baselinerm
baselinerm <- function(.data, degree = 4, tol = 1e-3, rep = 100) {

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("baseline", quietly = TRUE)

  # Check if the input data is provided and is a data frame or tibble
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !is_tibble(.data)) {
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

  # Select only numeric columns and convert to a matrix
  X_mat <- .data %>%
    select(where(is.numeric)) %>%
    as.matrix()

  # Ensure the input parameters are numeric
  degree <- as.numeric(degree)
  tol <- as.numeric(tol)
  rep <- as.numeric(rep)

  wlength <- colnames(X_mat)

  rreplaceWithZero <- function(x) {
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
    pluck("corrected") %>%
    as_tibble() %>%
    rename_with(~wlength, everything()) %>%
    map_dfr(replaceWithZero)

  # Estimated background emission
  background <- bc_mod %>%
    pluck("baseline") %>%
    as_tibble() %>%
    rename_with(~wlength, everything()) %>%
    map_dfr(replaceWithZero)

  res <- list("spec" = bc_spec, "bkg" = background)
  return(res)
  }
