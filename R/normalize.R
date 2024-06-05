#' @title Normalization
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function normalizes each row (observation) of the input data using various
#' normalization methods.
#'
#' @param data A numeric data frame containing the spectra.
#' @param method A character vector specifying the normalization method to apply.
#'
#' @return A data frame containing the original columns and the normalized
#' columns for each specified method with the method name as a prefix.
#'
#' @export normalize
#'
normalize <- function(data, method = "min_max") {

  if (missing(data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.numeric(data)) {
    stop("Input must be a numeric data frame or matrix.")
  }
  valid_methods <- c("min_max", "mean", "vector", "area", "max_intensity", "snv")
  if (!all(method %in% valid_methods)) {
    stop("Invalid method(s) specified. Available methods are: min_max, mean, vector, area, max_intensity, and snv.")
  }
  x <- data %*% data
   return(x)

}
