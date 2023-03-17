#' @title Robust Box-Cox and Yeo-Johnson Transformation
#' @author Christian L. Goueguel
#' @description The function transforms each variable in a dataset toward central normality using re-weighted maximum likelihood to robustly fit the Box-Cox or Yeo-Johnson transformation.
#' @details Wrapper of the `transfo` function from the cellWise package.
#' @source J. Raymaekers and P.J. Rousseeuw, Transforming variables to central normality, Machine Learning, 1–23 (2021).
#' @param .data A data frame or tibble containing numeric columns.
#' @param var A vector of character or numeric specifying the selected variable(s). Default is NULL (all columns).
#' @param quant Numeric value for the quantile used in determining the weights in the re-weighting step. Default is 0.99.
#' @param nbsteps Integer value for the number of re-weighting steps. Default is 2.
#' @return A tibble of transformed variable(s), method used ('BC' for Box-Cox and 'YJ' for Yeo-Johnson), objective and lambda.
#' @export robTransform
robTransform <- function(.data, var = NULL, quant = 0.99, nbsteps = 2) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("cellWise", quietly = TRUE)

  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }

  selected_data <- .data %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    { if (!is.null(var)) dplyr::select(., dplyr::all_of(var)) else . }

  transformation_result <- cellWise::transfo(
    selected_data,
    type = "bestObj",
    robust = TRUE,
    lambdarange = NULL,
    prestandardize = TRUE,
    prescaleBC = FALSE,
    quant = quant,
    nbsteps = nbsteps,
    checkPars = list(silent = TRUE)
  )

  summary_tbl <- tibble::tibble(
    variable = colnames(selected_data),
    lambda_hat = transformation_result$lambdahats,
    transform = transformation_result$ttypes,
    objective = transformation_result$objective
  )

  transformed_data <- transformation_result$Xt %>% dplyr::as_tibble()
  res <- list(
    summary = summary_tbl,
    data = transformed_data
  )
  return(res)
}
