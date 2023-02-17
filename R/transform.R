#' @title Robust Box-Cox and Yeo-Johnson Transformation
#' @description The function transforms each variable in a dataset toward central normality using re-weighted maximum likelihood to robustly fit the Box-Cox or Yeo-Johnson transformation.
#' @details Wrapper of the `transfo` function implemented in the cellWise package.
#' @source J. Raymaekers and P.J. Rousseeuw, Transforming variables to central normality. Machine Learning, 1–23, 2021, doi:10.1007/s10994-021-05960-5.
#' @param data Data frame or tibble. Columns are the numeric variables.
#' @param var Numeric. Selected variable(s).
#' @param quant Numeric. Quantile for determining the weights in the re-weighting step.
#' @param nbsteps Integer. Number of re-weighting steps.
#' @return Tibble of transformed variable(s).
#' @export transform
transform <- function(data, var = NULL, quant = 0.99, nbsteps = 2) {
  if (is.null(data) == TRUE) {
    stop("Data must be provided")
  }
  if (is.data.frame(data) == FALSE & tibble::is_tibble(data) == FALSE) {
    stop("Data must be of class data.frame, tbl_df, or tbl")
  }

  if (is.null(var) == FALSE) {
    data %>%
      select(where(is.numeric)) %>%
      select(all_of(var)) %>%
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
  } else {
    data %>%
      select(where(is.numeric)) %>%
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
}
