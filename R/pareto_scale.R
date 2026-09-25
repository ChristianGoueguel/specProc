#' @title Pareto Scaling
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function performs Pareto scaling on a numeric matrix or data frame.
#' Pareto scaling scales each variable (column) by dividing it by the square
#' root of its standard deviation.
#'
#' @param x A numeric matrix or data frame to be scaled.
#' @param drop.na A logical value indicating whether to ignore missing values
#' (NA) when computing the standard deviations. Default is `FALSE`.
#'
#' @return A numeric matrix (or a tibble if `x` is a data frame) with the same
#' dimensions as `x`, but with each variable scaled by the square root of its
#' standard deviation.
#'
#' @export pareto_scale
#'
#' @examples
#' pareto_scale(matrix(c(1, 2, 3, 10, 20, 30), ncol = 2))
#'
pareto_scale <- function(x, drop.na = FALSE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x) && !is.data.frame(x)) {
    stop("'x' must be a numeric matrix or data frame.")
  }
  if (!is.logical(drop.na)) {
    stop("'drop.na' must be a logical value (TRUE or FALSE).")
  }

  is_df <- is.data.frame(x)
  x <- as_numeric_matrix(x, "x")

  std_devs <- apply(x, 2, stats::sd, na.rm = drop.na)

  if (any(std_devs == 0, na.rm = TRUE)) {
    warning("Some variables have zero standard deviation and will not be scaled.")
    std_devs[!is.na(std_devs) & std_devs == 0] <- 1
  }

  x_scaled <- sweep(x, 2, sqrt(std_devs), "/")

  if (is_df) {
    x_scaled <- as_tbl(x_scaled, colnames(x))
  }

  return(x_scaled)
}
