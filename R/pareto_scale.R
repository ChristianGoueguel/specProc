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
#' @param drop_na A logical value indicating whether to ignore missing values
#' (NA) when computing the standard deviations. Default is `FALSE`.
#' @param drop.na `r lifecycle::badge("deprecated")` Use `drop_na`.
#'
#' @return A numeric matrix (or a tibble if `x` is a data frame) with the same
#' dimensions as `x`, but with each variable scaled by the square root of its
#' standard deviation.
#'
#' @export pareto_scale
#'
#' @examples
#' data(forageLIBS)
#' spectra <- forageLIBS[-(1:14)]  # the spectral channels
#' pareto_scale(spectra)[1:3, 1:4]
pareto_scale <- function(x, drop_na = FALSE,
                         drop.na = deprecated()) {
  drop_na <- renamed_arg(drop.na, drop_na, "pareto_scale", "drop.na", "drop_na")

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x) && !is.data.frame(x)) {
    stop("'x' must be a numeric matrix or data frame.")
  }
  if (!is.logical(drop_na)) {
    stop("'drop_na' must be a logical value (TRUE or FALSE).")
  }

  is_df <- is.data.frame(x)
  x <- as_numeric_matrix(x, "x")

  std_devs <- apply(x, 2, stats::sd, na.rm = drop_na)

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
