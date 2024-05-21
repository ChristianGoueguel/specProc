#' @title Robust Box-Cox and Yeo-Johnson Transformation
#' @author Christian L. Goueguel
#' @description Transforms each variable in a dataset toward central normality using re-weighted maximum likelihood to robustly fit the Box-Cox or Yeo-Johnson transformation.
#' @details Wrapper of the `transfo` function from the cellWise package.
#' @source Raymaekers and Rousseeuw (2021), Machine Learning, https://doi.org/10.1007/s10994-021-05960-5.
#' @param .data data frame or tibble.
#' @param var vector of selected character or numeric variable(s). Default is `NULL` (all columns are selected).
#' @param type transformation method to use. Available methods are "BC" (Box-Cox power transformation, which is for strictly positive values.), "YJ" ( Yeo-Johnson power transformation, which works for positive and negative values.) or "bestObj" (both BC and YJ are run for strictly positive variables, and the solution with lowest objective is kept. Whereas if a variable has negative values YJ is run.). Default is "bestObj".
#' @param quant numeric value for the quantile used in determining the weights in the re-weighting step (default is 0.99).
#' @param nbsteps number of re-weighting steps (2 by default).
#' @return tibble of transformed variable(s), method used ('BC' for Box-Cox and 'YJ' for Yeo-Johnson), objective and lambda.
#' @export robTransform
robTransform <- function(.data, var = NULL, type = "bestObj", quant = 0.99, nbsteps = 2) {
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (!is.null(var)) {
    if (is.character(var)) {
      not_found <- var[!var %in% names(.data)]
      if (length(not_found) > 0) {
        stop("The following variable(s) are not present in the data: ", paste(not_found, collapse = ", "))
      }
    } else {
      stop("The 'var' argument must be a character vector or NULL.")
    }
  }
  if (!type %in% c("BC", "YJ", "bestObj")) {
    stop("Invalid type of transformation. Available method types are: BC, YJ and bestObj.")
  }
  if (!is.character(type)) {
    stop("The argument 'type' must be a character.")
  }
  if (quant < 0 || quant > 1) {
    stop("'quant' must be a numeric value between 0 and 1.")
  }
  if (nbsteps <= 0) {
    stop("'nbsteps' must be a positive integer")
  }
  . <- NULL
  s_tbl <- .data %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    { if (!is.null(var)) dplyr::select(., dplyr::all_of(var)) else . }

  tf_out <- s_tbl %>%
    as.matrix() %>%
    cellWise::transfo(
      type = type,
      robust = TRUE,
      standardize = TRUE,
      quant = quant,
      nbsteps = nbsteps,
      checkPars = list(silent = TRUE)
    )
  summary_tbl <- tibble::tibble(
    variable = colnames(s_tbl),
    lambda_hat = tf_out$lambdahats,
    transform = tf_out$ttypes,
    objective = tf_out$objective
  )
  transfo_tbl <- s_tbl %>%
    as.matrix() %>%
    cellWise::transfo_newdata(tf_out) %>%
    tibble::as_tibble()

  result <- list(
    summary = summary_tbl,
    transformation = transfo_tbl
  )
  return(result)
}
