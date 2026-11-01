#' @title Robust Box-Cox and Yeo-Johnson Transformation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `robust_bcyj()` was deprecated in specProc 0.9.0: use [robust_boxcox()]
#' or [robust_yeojohnson()] instead, which return a fitted object that
#' [predict()][predict.specproc_transformation] applies to new data. It
#' still works, with a warning.
#'
#' @param x A data frame or tibble containing the variables to be transformed.
#' @param var A vector of character or numeric variable names to be transformed.
#'   If `NULL` (default), all columns are selected.
#' @param type A character string specifying the transformation method(s) to use.
#'   Allowed values are "BC", "YJ", or "bestObj" (default: Box-Cox or
#'   Yeo-Johnson, whichever fits best, for strictly positive variables).
#' @param quantile A numeric value between 0 and 1 specifying the quantile to use
#'   for determining the weights in the re-weighting step. Default is 0.99.
#' @param nbsteps An integer specifying the number of re-weighting steps to perform.
#'   Default is 2.
#'
#' @return A list containing two data frames:
#'  - `summary`:
#'    - `variable`: the variable(s) name
#'    - `lambda`: the estimated lambda parameter
#'    - `method`: the method used ('BC' for Box-Cox, 'YJ' for Yeo-Johnson, or
#'      'none')
#'    - `objective`: the objective function value
#'  - `transformation`:
#'    - the transformed variable(s)
#'
#' @author Christian L. Goueguel
#'
#' @keywords internal
#' @export robust_bcyj
robust_bcyj <- function(x, var = NULL, type = "bestObj", quantile = 0.99, nbsteps = 2) {
  lifecycle::deprecate_warn("0.9.0", "robust_bcyj()",
                            details = "Use `robust_boxcox()` or `robust_yeojohnson()` instead.")
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

  xmat <- as.matrix(s_tbl)
  fit <- robust_transformation(xmat, type = type, quantile = quantile, nbsteps = nbsteps)
  summary_tbl <- tibble::tibble(
    variable = colnames(s_tbl),
    lambda = unname(vapply(fit$fits, function(f) if (f$type == "none") NA_real_ else f$lambda,
                           numeric(1))),
    method = unname(vapply(fit$fits, `[[`, character(1), "type")),
    objective = unname(vapply(fit$fits, `[[`, numeric(1), "objective"))
  )
  transfo_tbl <- tibble::as_tibble(apply_transformation(xmat, fit))

  res <- list(
    summary = summary_tbl,
    transformation = transfo_tbl
  )
  return(res)
}
