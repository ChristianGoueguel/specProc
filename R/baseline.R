#' @title Baseline Correction
#' @description This function perform baseline correction using modified polynomial curve fitting.
#' @author Christian L. Goueguel
#' @details This is a wrapper function implemented on the baseline package.
#' @param data Data frame of LIBS emission spectra
#' @param degree Degree of modified polynomial fitting (by default 4)
#' @param tol Tolerance of difference between iterations (by default 1e-3)
#' @param rep Maximum number of iterations (by default 100)
#' @return A list containing a data frame of baseline corrected spectra (spec), and a data frame of the modeled background emission (bkg).
#' @import tidyselect
#' @importFrom tidyselect "vars_select_helpers"
#' @importFrom utils "globalVariables"
#' @export baseline
baseline <- function(data, degree = 4, tol = 1e-3, rep = 100) {

  utils::globalVariables("where")

  if (length(data) == 0) {
     stop("Seems you forgot to provide spectra data.")
   }
  else{
    if (is.data.frame(data) == FALSE) {
      stop("Data must be of class tbl_df, tbl or data.frame")
    }
    else{
      Xmat <- data %>%
        dplyr::select(tidyselect::vars_select_helpers$where(is.numeric)) %>%
        as.matrix()

      degree <- as.numeric(degree)
      tol <- as.numeric(tol)
      rep <- as.numeric(rep)
      wlength <- colnames(Xmat)

      replace_with_zero <- function(x) {
        ifelse(x < 0, yes = 0, no = x)
      }

      # baseline modeling
      bc_mod <- baseline::baseline.modpolyfit(
        spectra = Xmat,
        degree = degree,
        tol = tol,
        rep = rep
      )

      # baseline corrected spectra
      spec <- bc_mod %>%
        purrr::pluck("corrected") %>%
        tibble::as_tibble() %>%
        magrittr::set_colnames(all_of(wlength)) %>%
        purrr::map_dfr(replace_with_zero)

      # background emission
      bkg <- bc_mod %>%
        purrr::pluck("baseline") %>%
        tibble::as_tibble() %>%
        magrittr::set_colnames(all_of(wlength)) %>%
        purrr::map_dfr(replace_with_zero)

      res <- list("spec" = spec, "bkg" = bkg)
      return(res)
    }
  }
}
