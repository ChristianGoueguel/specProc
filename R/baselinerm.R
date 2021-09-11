#' @title Baseline Correction
#' @description This function perform the estimation and removal of the continuous background emission using a polynomial curve fitting function.
#' @author Christian L. Goueguel
#' @details This is a wrapper function implemented in the baseline package.
#' @source Chad A. Lieber, Anita Mahadevan-Jansen, Applied Spectroscopy, 2003, Vol. 57, 11, 1363-1367.
#' @param data Data frame of emission spectra
#' @param degree Degree of the polynomial fitting function (by default 4)
#' @param tol Tolerance of difference between iterations (by default 1e-3)
#' @param rep Maximum number of iterations (by default 100)
#' @return List containing a data frame of background subtracted spectra (spec), and a data frame of the modeled background (bkg).
#' @importFrom utils "globalVariables"
#' @export baselinerm
baselinerm <- function(data, degree = 4, tol = 1e-3, rep = 100) {

  utils::globalVariables(names = "where")

  if (length(data) == 0 | is.null(data) == TRUE) {
    stop("Apparently you forgot to provide the spectra.")
  }

  if (is.data.frame(data) == FALSE) {
    stop("Data must be of class tbl_df, tbl or data.frame")
  }

  Xmat <- data %>%
    dplyr::select(tidyselect::vars_select_helpers$where(is.numeric)) %>%
    as.matrix()

  degree <- as.numeric(degree)
  tol <- as.numeric(tol)
  rep <- as.numeric(rep)
  wlength <- colnames(Xmat)

  replaceWithZero <- function(x) {
    ifelse(x < 0, yes = 0, no = x)
  }

  # Background fitting
  bc_mod <- baseline::baseline.modpolyfit(
    spectra = Xmat,
    degree = degree,
    tol = tol,
    rep = rep
  )

  # Background subtracted spectra
  correctedSpec <- bc_mod %>%
    purrr::pluck("corrected") %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(tidyselect::all_of(wlength)) %>%
    purrr::map_dfr(replaceWithZero)

  # Estimated background emission
  background <- bc_mod %>%
    purrr::pluck("baseline") %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(tidyselect::all_of(wlength)) %>%
    purrr::map_dfr(replaceWithZero)

  res <- list(
    "spec" = correctedSpec,
    "bkg" = background
  )

  return(res)

  }
