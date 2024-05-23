#' @title Spectra Normalization
#' @author Christian L. Goueguel
#' @description This function takes a data frame (spectra) as input and normalizes it using one or more specified normalization methods.
#' @param .data A numeric data frame containing the spectra.
#' @param method A character vector specifying the normalization method(s) to apply. Available methods are "min_max", "mean", "vector", "area", "max_intensity", and "snv". Default is "min_max".
#' @return A data frame containing the original columns and the normalized columns for each specified method with the method name as a prefix.
#' @export normSpec
normSpec <- function(.data, method = "min_max") {
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!all(.data %>% purrr::map_lgl(is.numeric))) {
    stop("Input must be a numeric data frame.")
  }
  valid_methods <- c("min_max", "mean", "vector", "area", "max_intensity", "snv")
  if (!all(method %in% valid_methods)) {
    stop("Invalid method specified. Available methods are: min_max, mean, vector, area, max_intensity, and snv.")
  }

  if (method == "min_max") {
    res <- (.data - min(.data)) / (max(.data) - min(.data))
  }
  if (method == "mean") {
    res <- (.data - mean(.data)) / (max(.data) - min(.data))
  }
  if (method == "vector") {
    res <- .data / sqrt(sum(.data^2))
  }
  if (method == "area") {
    res <- .data / sum(.data)
  }
  if (method == "max_intensity") {
    res <- .data / max(.data)
  }
  if (method == "snv") {
    res <- (.data - mean(.data)) / stats::sd(.data)
  }

  return(res)

}
