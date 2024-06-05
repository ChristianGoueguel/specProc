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
#'   Available methods are:
#'   \itemize{
#'     \item{"min_max"}{Min-max normalization: (x - min(x)) / (max(x) - min(x))}
#'     \item{"mean"}{Mean normalization: (x - mean(x)) / (max(x) - min(x))}
#'     \item{"vector"}{Vector normalization: x / sqrt(sum(x^2))}
#'     \item{"area"}{Area normalization: x / sum(x)}
#'     \item{"max_intensity"}{Max intensity normalization: x / max(x)}
#'     \item{"snv"}{Standard Normal Variate (SNV) normalization: (x - mean(x)) / sd(x)}
#'   }
#'   Default is "min_max".
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
  normalized_data <- data
  for (m in method) {
    switch(m,
           "min_max" = {
             normalized_data <- cbind(
               normalized_data,
               `min_max` = (data - min(data)) / (max(data) - min(data))
               )
           },
           "mean" = {
             normalized_data <- cbind(
               normalized_data,
               `mean` = (data - mean(data)) / (max(data) - min(data))
               )
           },
           "vector" = {
             normalized_data <- cbind(normalized_data, `vector` = data / sqrt(rowSums(data^2)))
           },
           "area" = {
             normalized_data <- cbind(normalized_data, `area` = data / rowSums(data))
           },
           "max_intensity" = {
             normalized_data <- cbind(normalized_data, `max_intensity` = data / max(data))
           },
           "snv" = {
             normalized_data <- cbind(normalized_data, `snv` = (data - rowMeans(data)) / rowSds(data))
           }
    )
  }
  return(normalized_data)
}
