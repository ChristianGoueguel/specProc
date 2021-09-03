#' @title Baseline Correction of LIBS Spectra With Polynomial Fitting
#' @author Christian L. Goueguel, September 2021
#' @param data Data frame of LIBS emission spectra
#' @param .degree Degree of modified polynomial fitting
#' @param .tol Tolerance of difference between iterations
#' @param .rep Maximum number of iterations
#'
#' @return List containing (1) spec: data frame of baseline corrected spectra,
#'                         (2) bkg: data frame of modeled background emission
#' @export baseline_rmv
baseline_rmv <- function(data, .degree = 4, .tol = 0.001, .rep = 100) {

  X <- data %>%
    select(where(is.numeric)) %>%
    as.matrix()

  .degree %<>% as.numeric()
  .tol %<>% as.numeric()
  .rep %<>% as.numeric()
  .t <- X %>% names() %>% as.numeric()

  # A function replacing negative values with 0
  # "if the value is less than 0, put 0, else leave it alone"
  replace_with_zero <- function(x) {
    ifelse(x < 0, yes = 0, no = x)
  }

  # baseline modeling
  set.seed(101)
  bc_mod <- baseline::baseline.modpolyfit(
    spectra = X,
    t = .t,
    degree = .degree,
    tol = .tol,
    rep = .rep
  )

  # baseline corrected spectra
  spec <- bc_mod %>%
    pluck("corrected") %>%
    as_tibble() %>%
    map_dfr(., replace_with_zero)

  # background emission
  bkg <- bc_mod %>%
    pluck("baseline") %>%
    as_tibble() %>%
    map_dfr(., replace_with_zero)

  res_list <- list("spec" = spec, "bkg" = bkg)
  return(res_list)

}
