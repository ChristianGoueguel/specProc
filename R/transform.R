#' @title Function for computing the robust Box-Cox and Yeo-Johnson transformations
#' @description Perform transformation to normality, without effects of by any outliers that may be present in the data.
#' @author Christian L. Goueguel
#' @details Wrapper function implemented in the cellWise package.
#' @source Raymaekers, J. and Rousseeuw, P. J. Transforming variables to central normality. Mach Learn 1–23 (2021) doi:10.1007/s10994-021-05960-5.
#' @param data
#' @param var
#' @param quant
#' @param nbsteps
#' @return Data frame of transformed variable(s).
#' @export transform
transform <- function(data, var = NULL, quant = 0.99, nbsteps = 2) {
  data %>%
    select(all_of(var)) %>%
    na_if(0) %>%
    cellWise::transfo(
      type = "bestObj",
      robust = TRUE,
      lambdarange = NULL,
      prestandardize = TRUE,
      prescaleBC = FALSE,
      quant = 0.99,
      nbsteps = 2,
      checkPars = list(silent = TRUE)
    ) %T>%
    {
      tibble(
        mineral = vctrs::vec_c(var),
        lambda_hat = pluck(., "lambdahats"),
        transform = pluck(., "ttypes"),
        objective = pluck(., "objective")
      ) %>%
        print()
    } %>%
    pluck("Xt") %>%
    as_tibble()
}
