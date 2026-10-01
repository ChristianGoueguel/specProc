#' @title Biweight Midcovariance
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function computes the biweight midcovariance, a robust measure of
#' covariance between two numerical vectors. The biweight midcovariance is
#' less sensitive to outliers than the traditional covariance.
#'
#' @references
#'    - Wilcox, R., (1997).
#'      Introduction to Robust Estimation and Hypothesis Testing.
#'      Academic Press
#'
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as `x`.
#' @return The biweight midcovariance between `x` and `y`.
#'
#' @examples
#' data(forageLIBS)
#' # iron and manganese contents (mg/kg), with a few iron-rich samples
#' ok <- !is.na(forageLIBS$Fe)
#' c(covariance = stats::cov(forageLIBS$Fe[ok], forageLIBS$Mn[ok]),
#'   biweight = biweight_midcovariance(forageLIBS$Fe[ok], forageLIBS$Mn[ok]))
#'
#' @export biweight_midcovariance
#'
biweight_midcovariance <- function(x, y) {
  check_bivariate(x, y)
  if (anyNA(x) || anyNA(y)) {
    return(NA_real_)
  }
  wx <- biweight_terms(x)
  wy <- biweight_terms(y)
  if (is.null(wx) || is.null(wy)) {
    return(0)
  }
  return(length(x) * sum(wx$a * wy$a) / (sum(wx$d) * sum(wy$d)))
}

check_bivariate <- function(x, y) {
  if (missing(x) || missing(y)) {
    stop("Inputs 'x' and 'y' must be provided.", call. = FALSE)
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both 'x' and 'y' must be numeric vectors.", call. = FALSE)
  }
  if (length(x) != length(y)) {
    stop("'x' and 'y' must have the same length.", call. = FALSE)
  }
  if (length(x) < 2) {
    stop("'x' and 'y' must have at least two elements.", call. = FALSE)
  }
  if (length(unique(x)) == 1 || length(unique(y)) == 1) {
    stop("'x' and 'y' cannot be constant vectors.", call. = FALSE)
  }
  invisible(TRUE)
}
