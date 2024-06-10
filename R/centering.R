#' @title Data Centering
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Function to perform mean-centering or median-centering on a numeric matrix or
#' data frame.
#'
#' @details
#' Mean-centering calculates the mean of each column and subtracts this from
#' the column. Median-centering is very similar to
#' mean-centering except that the reference point is the median of each column
#' rather than the mean.
#'
#' @param x A numeric matrix or data frame.
#' @param method A character string specifying the centering method, either
#' "mean" or "median".
#' @param drop.na A logical value indicating whether to remove missing values
#' (NA) before centering.
#'
#' @return A data frame with the same dimensions as the input,
#' but with columns centered according to the specified method.
#'
#' @export centering
#'
centering <- function(x, method = "mean", drop.na = FALSE) {

  if (!is.numeric(x) && !is.data.frame(x)) {
    stop("Input must be a numeric matrix or data frame")
  }

  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }

  if (drop.na) {
    x <- na.omit(x)
  }

  if (method == "mean") {
    xc <- sweep(x, 2, colMeans(x))
  } else if (method == "median") {
    xc <- sweep(x, 2, apply(x, 2, stats::median))
  } else {
    stop("Invalid method. Must be either 'mean' or 'median'")
  }

  return(xc)
}
