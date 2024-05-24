#' @title Classical or Robust Descriptive Statistics
#' @description This function calculates various descriptive statistics (robust and non-robust)
#'   for a specified variable or all variables in a given data frame or tibble.
#' @param data A data frame or tibble containing the variable(s) of interest.
#' @param var A character vector specifying the variable(s) for which to calculate the
#'   summary statistics. If left as \code{NULL} (the default), summary statistics will
#'   be calculated for all variables in the data frame/tibble.
#' @param drop.na A logical value indicating whether to remove missing values (\code{NA})
#'   from the calculations. If \code{TRUE} (the default), missing values will be removed.
#'   If \code{FALSE}, missing values will be included in the calculations.
#' @param digits An integer specifying the number of significant digits to display after
#'   the decimal point in the output.
#' @param robust A logical value indicating whether to compute robust descriptive statistics. If \code{FALSE} (the default), computes the classical descriptive statistics for describing the distribution of a univariate variable.
#' @return A data frame containing the summary statistics for the specified variable(s).
#' @export summaryStats
#' @examples
#' # Load the iris dataset
#' data(iris)
#'
#' # Calculate summary statistics for all variables in the iris dataset
#' summary_stats <- summaryStats(iris)
#'
#' # Calculate summary statistics for the 'Sepal.Length' and 'Petal.Length' variables
#' sepal_length_stats <- summaryStats(iris, var = c("Sepal.Length", "Petal.Length"), robust = TRUE)
summaryStats <- function(data, var = NULL, drop.na = TRUE, digits = 2, robust = FALSE) {
  if (is.null(data) == TRUE) {
    stop("Data must be provided")
  }
  if (is.data.frame(data) == FALSE & tibble::is_tibble(data) == FALSE) {
    stop("Data must be of class data.frame, tbl_df, or tbl")
  }
  if (!is.null(var)) {
    if (!is.character(var) && !is.numeric(var)) {
      stop("'var' must be either a character vector or a numeric vector")
    }
    if (is.character(var) && !all(var %in% names(data))) {
      stop("One or more variables specified in 'var' are not present in the data")
    }
  }
  if (!is.logical(drop.na)) {
    stop("'drop.na' must be a logical value (TRUE or FALSE)")
  }
  if (!is.numeric(digits) || digits < 0 || digits %% 1 != 0) {
    stop("'digits' must be a non-negative integer")
  }
  if (!is.logical(robust)) {
    stop("'robust' must be a logical value (TRUE or FALSE)")
  }

  variable <- NULL
  value <- NULL
  sd <- NULL
  mad <- NULL
  median <- NULL
  rsd <- NULL

  getmode <- function(vec) {
    unique_x <- unique(vec)
    unique_x[which.max(tabulate(match(vec, unique_x)))]
  }

  if (robust == FALSE) {
    fct_summary <- function(x) {
      x %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(
          mean = round(mean(value), digits),
          mode = round(getmode(value), digits),
          median = round(stats::median(value), digits),
          sd = round(stats::sd(value), digits),
          variance = stats::var(value),
          cv = round((sd / mean) * 100, digits),
          min = min(value),
          max = max(value),
          range = max - min,
          skewness = round(moments::skewness(value), digits),
          kurtosis = round(moments::kurtosis(value), digits),
          count = dplyr::n()
        ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()
    }
  } else {
    fct_summary <- function(x) {
      x %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(
          median = round(stats::median(value), digits),
          mad = round(stats::mad(value), digits),
          Qn = round(robustbase::Qn(value), digits),
          Sn = round(robustbase::Sn(value), digits),
          rsd = 1.4826 * mad,
          bivar = round(biweight_midvariance(value), digits),
          rcv = round((rsd / median) * 100, digits),
          IQR = round(stats::IQR(value, na.rm = TRUE), digits),
          min = min(value),
          max = max(value),
          medcouple = round(robustbase::mc(value), digits),
          count = dplyr::n()
        ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()
    }
  }

  if (is.null(var) == FALSE) {
    if (drop.na == TRUE) {
      data %>%
        dplyr::select(tidyselect::where(is.numeric)) %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(var),
          names_to = "variable",
          values_to = "value"
          ) %>%
        tidyr::drop_na(value) %>%
        fct_summary()
    } else {
      data %>%
        dplyr::select(tidyselect::where(is.numeric)) %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(var),
          names_to = "variable",
          values_to = "value"
        ) %>%
        fct_summary()
    }
  } else {
    if (drop.na == TRUE) {
      data %>%
        dplyr::select(dplyr::where(is.numeric)) %>%
        tidyr::pivot_longer(
          cols = tidyselect::everything(),
          names_to = "variable",
          values_to = "value"
        ) %>%
        tidyr::drop_na(value) %>%
        fct_summary()
    } else {
      data %>%
        dplyr::select(tidyselect::where(is.numeric)) %>%
        tidyr::pivot_longer(
          cols = tidyselect::everything(),
          names_to = "variable",
          values_to = "value"
        ) %>%
        fct_summary()
    }
  }
}