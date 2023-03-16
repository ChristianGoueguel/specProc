#' @title Compute Correlation for Each Column with Respect to the Response Variable
#' @author Christian L. Goueguel
#' @description This function takes a data frame as input and computes the Pearson, Kendall, or Spearman correlation
#' for each column with respect to the response variable. The function returns a tibble with the respective
#' correlation for each column.
#' @param df A numeric data frame containing the data.
#' @param response_var A character string specifying the name of the response variable column in the data frame.
#' @param method A character string specifying the correlation method to use. Available methods are "pearson",
#'               "kendall", and "spearman". Default is "pearson".
#' @return A tibble containing the variable name, correlation value, and method used.
#' @export corr
corr <- function(.data, response_var, method = "pearson") {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("corrr", quietly = TRUE)

  if (!is.data.frame(.data) || !all(.data %>% map_lgl(is.numeric))) {
    stop("Input must be a numeric data frame.")
  }

  if (!response_var %in% colnames(.data)) {
    stop("Response variable not found in the data frame.")
  }

  valid_methods <- c("pearson", "kendall", "spearman")
  if (!method %in% valid_methods) {
    stop("Invalid method specified. Available methods are: pearson, kendall, and spearman.")
  }

  correlation <- df %>%
    correlate(method = ifelse(method == "kendall", "kendall", "pearson")) %>%
    rownames_to_column("variable") %>%
    select(variable, !!response_var) %>%
    rename(correlation = !!response_var) %>%
    mutate(method = method)

  if (method == "spearman") {
    correlation <- correlation %>%
      mutate(correlation = as.numeric(correlation)) %>%
      mutate(correlation = 2 * asin(correlation / 2)) %>%
      mutate(correlation = round(correlation, 2))
  }

  correlation <- .data %>%
    correlate(method = method) %>%
    rownames_to_column("variable") %>%
    select(variable, !!response_var) %>%
    rename(correlation = !!response_var) %>%
    mutate(method = method)

  return(correlation)
}
