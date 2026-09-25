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
#' @param drop.na A logical value indicating whether to ignore missing values
#' (NA) when computing the column means or medians. Missing values are kept
#' in the output.
#'
#' @return A numeric matrix with the same dimensions as the input, with
#' columns centered according to the specified method. The column centers
#' are stored in the `"center"` attribute.
#'
#' @export center
#'
#' @examples
#' m <- matrix(c(1, 2, 3, 10, 20, 30), ncol = 2)
#' center(m)
#' center(m, method = "median")
#'
center <- function(x, method = "mean", drop.na = FALSE) {

  if (!is.numeric(x) && !is.data.frame(x)) {
    stop("Input must be a numeric matrix or data frame")
  }
  check_flag(drop.na, "drop.na")
  x <- as_numeric_matrix(x, "x")

  centers <- switch(
    method,
    mean = colMeans(x, na.rm = drop.na),
    median = apply(x, 2, stats::median, na.rm = drop.na),
    stop("Invalid method. Must be either 'mean' or 'median'")
  )

  xc <- sweep(x, 2, centers)
  attr(xc, "center") <- centers
  return(xc)
}
