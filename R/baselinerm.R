#' @title Background Correction
#' @description This function perform the estimation and removal of the continuous background emission using polynomial curve fitting.
#' @author Christian L. Goueguel
#' @details This is a wrapper function implemented in the baseline package.
#' @param data Data frame of emission spectra
#' @param degree Degree of modified polynomial fitting (by default 4)
#' @param tol Tolerance of difference between iterations (by default 1e-3)
#' @param rep Maximum number of iterations (by default 100)
#' @return List containing a data frame of background subtracted spectra (spec), and a data frame of the modeled background (bkg).
#' @import tidyselect
#' @import magrittr
#' @importFrom tidyselect "vars_select_helpers"
#' @importFrom utils "globalVariables"
#' @importFrom dplyr "select"
#' @importFrom purrr "pluck"
#' @importFrom purrr "map_dfr"
#' @importFrom tibble "as_tibble"
#' @export baselinerm
baselinerm <- function(data, degree = 4, tol = 1e-3, rep = 100) {

  utils::globalVariables(names = "where")

  if (length(data) == 0 | is.null(data) == TRUE) {
    stop("Apparently you forgot to provide the spectra.")
  }
  else{
    if (is.data.frame(data) == FALSE) {
      stop("Data must be of class tbl_df, tbl or data.frame")
    }
    else{
      Xmat <- data %>%
        select(tidyselect::vars_select_helpers$where(is.numeric)) %>%
        as.matrix()

      degree %<>% as.numeric()
      tol %<>% as.numeric()
      rep %<>% as.numeric()
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
        pluck("corrected") %>%
        as_tibble() %>%
        set_colnames(all_of(wlength)) %>%
        map_dfr(replaceWithZero)

      # Estimated background emission
      background <- bc_mod %>%
        pluck("baseline") %>%
        as_tibble() %>%
        set_colnames(all_of(wlength)) %>%
        map_dfr(replaceWithZero)

      res <- list(
        "spec" = correctedSpec,
        "bkg" = background
        )
      return(res)
    }
  }
}
