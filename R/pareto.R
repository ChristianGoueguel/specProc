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
#' @param drop.na A logical value indicating whether to remove missing values
#' (NA) from the calculations. If `TRUE` (the default), missing values will be
#' removed. If `FALSE`, missing values will be included.
#'
#' @return A numeric matrix or data frame with the same dimensions as `x`,
#' but with each variable scaled by the square root of its standard deviation.
#'
#' @export pareto
#'
pareto <- function(x, drop.na = FALSE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x) && !is.data.frame(x)) {
    stop("'x' must be a numeric matrix or data frame.")
  }
  if (!is.logical(drop.na)) {
    stop("'drop.na' must be a logical value (TRUE or FALSE).")
  }

  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }

  std_devs <- apply(x, 2, stats::sd, na.rm = drop.na)

  if (any(std_devs == 0)) {
    warning("Some variables have zero standard deviation and will not be scaled.")
    std_devs[std_devs == 0] <- 1  # Set zero standard deviations to 1 to avoid division by zero
  }

  x_scaled <- sweep(x, 2, sqrt(std_devs), "/")

  if (is.data.frame(x)) {
    x_scaled <- tibble::as_tibble(x_scaled)
  }

  return(x_scaled)
}
