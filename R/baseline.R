#' @title Baseline Correction
#' @description This function perform baseline correction using modified polynomial curve fitting.
#' @author Christian L. Goueguel
#' @details This is a wrapper function implemented on the baseline package.
#' @param data Data frame of LIBS emission spectra
#' @param degree Degree of modified polynomial fitting (by default 4)
#' @param tol Tolerance of difference between iterations (by default 0.001)
#' @param rep Maximum number of iterations (by default 100)
#' @return A list containing a data frame of baseline corrected spectra (spec), and a data frame of the modeled background emission (bkg).
#' @export baseline
#'
baseline <- function(data, degree = 4, tol = 0.001, rep = 100) {

  if (length(data) == 0) {
     stop("Seems you forgot to provide spectra data.")
   }
  else{
    if (is.data.frame(data) == FALSE) {
      stop("Data must be of class data.frame, tbl_df, or tbl")
    }
    else{
      if (!require(baseline)) install.packages("baseline")
      if (!require("pacman")) install.packages("pacman")
      pacman::p_load(magrittr, dplyr, purrr, tibble)

      Xmat <- data %>%
        select(where(is.numeric)) %>%
        as.matrix()

      degree %<>% as.numeric()
      tol %<>% as.numeric()
      rep %<>% as.numeric()
      wlength <- Xmat %>% colnames()

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
        pluck("corrected") %>%
        as_tibble() %>%
        set_colnames(all_of(wlength)) %>%
        map_dfr(., replace_with_zero)

      # background emission
      bkg <- bc_mod %>%
        pluck("baseline") %>%
        as_tibble() %>%
        set_colnames(all_of(wlength)) %>%
        map_dfr(., replace_with_zero)

      res <- list("spec" = spec, "bkg" = bkg)
      return(res)
    }
  }
}
