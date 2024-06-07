#' @title Robust Box-Cox and Yeo-Johnson Transformation
#'
#' @description
#' Transforms each variable in a dataset toward central normality using
#' re-weighted maximum likelihood to robustly fit the Box-Cox or Yeo-Johnson
#' transformation.
#'
#' @details
#' The Box-Cox and Yeo-Johnson transformations are power transformations
#' aimed at making the data distribution more normal-like. The Box-Cox
#' transformation is suitable for strictly positive values, while the
#' Yeo-Johnson transformation can handle both positive and negative values.
#' The function is a wrapper around the `transfo` function
#' from the `cellWise` package, which applies a robust version of these
#' transformations by using re-weighted maximum likelihood estimation.
#' This approach downweights outlying observations to make the transformation
#' more robust to their influence.
#'
#'
#' The `type` parameter controls which transformation method(s) to use:
#'  - "BC": Only applies the Box-Cox transformation to strictly positive variables.
#'  - "YJ": Only applies the Yeo-Johnson transformation to all variables.
#'  - "bestObj" (default): For strictly positive variables, both BC and YJ are
#'    applied, and the solution with the lowest objective function value is kept.
#'    For variables with negative values, only YJ is applied.
#'
#' @param x A data frame or tibble containing the variables to be transformed.
#' @param var A vector of character or numeric variable names to be transformed.
#'   If `NULL` (default), all columns are selected.
#' @param type A character string specifying the transformation method(s) to use.
#'   Allowed values are "BC", "YJ", or "bestObj" (default).
#' @param quantile A numeric value between 0 and 1 specifying the quantile to use
#'   for determining the weights in the re-weighting step. Default is 0.99.
#' @param nbsteps An integer specifying the number of re-weighting steps to perform.
#'   Default is 2.
#'
#' @return A list containing two data frames:
#'  - `summary`:
#'    - `variable`: the variable(s) name
#'    - `lambda`: the estimated lambda parameter
#'    - `method`: the method used ('BC' for Box-Cox or 'YJ' for Yeo-Johnson)
#'    - `objective`: the objective function value
#'  - `transformation`:
#'    - the transformed variable(s)
#'
#' @references
#'  - Raymaekers, J., Rousseeuw, P.J., (2021). Transforming variables to central normality.
#'   Machine Learning, https://doi.org/10.1007/s10994-021-05960-5.
#'  - Box, G. E. P., Cox, D. R. (1964). An analysis of transformations.
#'   Journal of the Royal Statistical Society, Series B, 26:211–252.
#'
#' @author Christian L. Goueguel
#'
#' @export robustBCYJ
#'
robustBCYJ <- function(x, var = NULL, type = "bestObj", quantile = 0.99, nbsteps = 2) {
  if (missing(x)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(x) && !tibble::is_tibble(x)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (!is.null(var)) {
    if (is.character(var)) {
      not_found <- var[!var %in% names(x)]
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
  if (quantile < 0 || quantile > 1) {
    stop("'quantile' must be a numeric value between 0 and 1.")
  }
  if (nbsteps <= 0) {
    stop("'nbsteps' must be a positive integer")
  }
  . <- NULL
  s_tbl <- x %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    { if (!is.null(var)) dplyr::select(., dplyr::all_of(var)) else . }

  tf_out <- s_tbl %>%
    as.matrix() %>%
    cellWise::transfo(
      type = type,
      robust = TRUE,
      standardize = TRUE,
      quant = quantile,
      nbsteps = nbsteps,
      checkPars = list(silent = TRUE)
    )
  summary_tbl <- tibble::tibble(
    variable = colnames(s_tbl),
    lambda = tf_out$lambdahats,
    method = tf_out$ttypes,
    objective = tf_out$objective
  )
  transfo_tbl <- s_tbl %>%
    as.matrix() %>%
    cellWise::transfo_newdata(tf_out) %>%
    tibble::as_tibble()

  res <- list(
    summary = summary_tbl,
    transformation = transfo_tbl
  )
  return(res)
}
