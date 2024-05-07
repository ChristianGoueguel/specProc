#' @title Compute Correlation for Each Column with Respect to a Response Variable
#' @author Christian L. Goueguel
#' @description This function takes a data frame as input and computes the Pearson, Spearman or Kendall correlation
#' for each column with respect to the response variable. The function returns a tibble with the respective
#' correlation for each column.
#' @param .data A numeric data frame containing the data.
#' @param response_var A character string specifying the name of the response variable column in the data frame.
#' @param method A character string specifying the correlation method to use. Available methods are "pearson", "spearman", "kendall" and "chatterjee". Default is "pearson".
#' @param .plot for a visual representation of the results (FALSE by default).
#' @return List containing:
#' @return \item{`correlation`}{data frame of the correlation values}
#' @return \item{`plot`}{ggplot2 object (if `.plot = TRUE`)}
#' @export corr
corr <- function(.data, response_var, method = "pearson", .plot = FALSE) {
  if (!is.data.frame(.data) || !all(.data %>% purrr::map_lgl(is.numeric))) {
    stop("Input must be a numeric data frame")
  }
  if (!rlang::quo_name(rlang::enquo(response_var)) %in% colnames(.data)) {
    stop("Response variable not found in the data frame")
  }
  valid_methods <- c("pearson", "spearman", "kendall", "chatterjee")
  if (!method %in% valid_methods) {
    stop("Invalid method specified. Available methods are: pearson, spearman, kendall and chatterjee")
  }
  if (!is.logical(.plot)) {
    stop("'.plot' must be of type boolean (TRUE or FALSE)")
  }

  if (method != "kendall") {
    tbl_corr <- .data %>%
      corrr::correlate(
        method = ifelse(method == "spearman", "spearman", "pearson"),
        use = "pairwise.complete.obs",
        quiet = TRUE
      ) %>%
      tibble::rownames_to_column("variable") %>%
      dplyr::select(variable, {{response_var}}) %>%
      dplyr::rename(correlation = {{response_var}}) %>%
      dplyr::mutate(method = method)
  } else {
    tbl_corr <- .data %>%
      corrr::correlate(
        method = "kendall",
        use = "pairwise.complete.obs",
        quiet = TRUE
      ) %>%
      tibble::rownames_to_column("variable") %>%
      dplyr::select(variable, {{response_var}}) %>%
      dplyr::rename(correlation = {{response_var}}) %>%
      dplyr::mutate(method = method)
  }
  if (method == "chatterjee") {
    tbl_corr <- .data %>%
      as.matrix() %>%
      XICOR::xicor(pvalue = FALSE, ties = TRUE) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("variable") %>%
      tibble::as_tibble() %>%
      dplyr::select(variable, {{response_var}}) %>%
      dplyr::rename(correlation = {{response_var}}) %>%
      dplyr::mutate(method = method)
  }

  if (method != "chatterjee") {
    tbl_corr$variable <- names(.data)
    tbl_corr <- tbl_corr %>%
      tidyr::drop_na() %>%
      dplyr::arrange(desc(correlation))
  } else {
    tbl_corr$variable <- names(.data)
    tbl_corr <- tbl_corr %>%
      dplyr::filter(variable != rlang::quo_name(rlang::enquo(response_var))) %>%
      tidyr::drop_na() %>%
      dplyr::arrange(desc(correlation))
  }

  p <- tbl_corr %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = reorder(variable, -correlation), y = correlation, fill = method) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_vline(xintercept = 0, colour = "white", linewidth = 1) +
    ggplot2::scale_fill_manual(values = "#111D71") +
    ggplot2::scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1), limits = c(-1, 1)) +
    ggplot2::labs(x = " ", y = paste0(rlang::quo_name(rlang::enquo(response_var)), " ", "correlation")) +
    ggplot2::coord_flip() +
    cowplot::theme_minimal_vgrid() +
    ggplot2::theme(legend.position = "top")

  if (.plot == FALSE) {
    return(tbl_corr)
  } else {
    return(list(correlation = tbl_corr, plot = p))
  }
}
