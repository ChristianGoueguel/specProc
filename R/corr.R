#' @title Compute Correlation for Each Column with Respect to the Response Variable
#' @author Christian L. Goueguel
#' @description This function takes a data frame as input and computes the Pearson or Spearman correlation
#' for each column with respect to the response variable. The function returns a tibble with the respective
#' correlation for each column.
#' @param .data A numeric data frame containing the data.
#' @param response_var A character string specifying the name of the response variable column in the data frame.
#' @param method A character string specifying the correlation method to use. Available methods are "pearson" and "spearman". Default is "pearson".
#' @return A tibble containing the variable name, correlation value, and method used.
#' @export corr
corr <- function(.data, response_var, method = "pearson") {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("corrr", quietly = TRUE)

  if (!is.data.frame(.data) || !all(.data %>% purrr::map_lgl(is.numeric))) {
    stop("Input must be a numeric data frame.")
  }
  if (!response_var %in% colnames(.data)) {
    stop("Response variable not found in the data frame.")
  }
  valid_methods <- c("pearson", "spearman")
  if (!method %in% valid_methods) {
    stop("Invalid method specified. Available methods are: pearson and spearman.")
  }

  correlation <- .data %>%
    corrr::correlate(
      method = ifelse(method == "spearman", "spearman", "pearson"),
      use = "pairwise.complete.obs",
      quiet = TRUE
      ) %>%
    tibble::rownames_to_column("variable") %>%
    dplyr::select(variable, !!response_var) %>%
    dplyr::rename(correlation = !!response_var) %>%
    dplyr::mutate(method = method)

  correlation$variable <- names(.data)
  correlation <- correlation %>%
    tidyr::drop_na() %>%
    dplyr::arrange(desc(correlation))

  return(correlation)
}
