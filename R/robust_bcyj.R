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
#' The transformations are fitted robustly (Raymaekers and Rousseeuw, 2021),
#' so that outlying observations do not drive the estimate of \eqn{\lambda}:
#'  1. Each variable is pre-standardized: divided by its median for Box-Cox,
#'     which needs strictly positive values, and centered by its median and
#'     divided by its MAD for Yeo-Johnson.
#'  2. An initial \eqn{\lambda} (between -4 and 6) minimizes a robust
#'     distance (Tukey's biweight) between the sorted, robustly standardized
#'     transformed values and the quantiles of the normal distribution. For
#'     this step, the transformation is continued linearly on the side that
#'     it compresses (the upper side for \eqn{\lambda < 1}, the lower side
#'     for \eqn{\lambda > 1}), beyond the point where the transformed value
#'     is 1.5 times that of the quartile, so that outliers on that side
#'     cannot dictate \eqn{\lambda}.
#'  3. The values whose standardized transformed value exceeds the
#'     `quantile` of the normal distribution are given weight zero (first
#'     with the rectified transformation), and
#'     \eqn{\lambda} is re-estimated by maximum likelihood on the others,
#'     `nbsteps` times.
#'  4. The transformed variable is standardized by the mean and standard
#'     deviation of these inliers.
#'
#' Variables with fewer than 5 values or no spread (zero MAD) are left
#' unchanged (method `"none"`), as are variables with non-positive values
#' when `type = "BC"`.
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
#'    - `method`: the method used ('BC' for Box-Cox, 'YJ' for Yeo-Johnson, or
#'      'none')
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
#' @export robust_bcyj
#'
robust_bcyj <- function(x, var = NULL, type = "bestObj", quantile = 0.99, nbsteps = 2) {
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
