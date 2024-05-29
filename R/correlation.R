#' @title Correlation Coefficients: Pearson, Spearman, Kendall, Chatterjee, and Biweight Midcorrelation
#'
#' @description
#' Computes various correlation coefficients between a specified response variable
#' and each of the remaining variables in a given data frame or tibble. The available correlation
#' methods are Pearson's product-moment correlation (parametric), Spearman's rank correlation,
#' Kendall's tau correlation (non-parametric), Chatterjee's new correlation coefficient, and
#' the biweight midcorrelation (a robust correlation measure). The Pearson's, Spearman's,
#' and Kendall's coefficients are a wrapper around the `corrr` package, whilst
#' the Chatterjee's coefficient is a wrapper around the `XICOR` package.
#'
#' @details
#' The Pearson correlation coefficient measures the linear relationship between two continuous
#' variables and is suitable when the data follows a bivariate normal distribution. The Spearman
#' and Kendall correlations are non-parametric measures of monotonic association, making them
#' suitable for non-linear relationships and when the data deviates from normality.
#' The Chatterjee correlation coefficient is a recently proposed measure that aims to address
#' some limitations of existing correlation coefficients, particularly for heavy-tailed
#' distributions and in the presence of outliers. The biweight midcorrelation is
#' a robust correlation measure that downweights the influence of outliers and is
#' recommended when the data contains extreme values or deviates significantly
#' from normality.
#'
#' @param .data A data frame or tibble containing the variables of interest.
#' @param response_var A character string specifying the name of the response variable.
#' @param method A character string indicating the correlation method to use. Allowed values are
#'   "pearson", "spearman", "kendall", "chatterjee", or "bicor" (for biweight midcorrelation).
#'   The default is "pearson".
#' @param .plot A logical value indicating whether to produce a visualization of the correlations.
#'   Default is FALSE (no plot).
#' @param .color A character string specifying the color to use for the plot. Default is "#111D71".
#' @param .interactive A logical value indicating whether to create an interactive plot using
#'   plotly. Default is FALSE (static ggplot2 plot).
#'
#' @return A list containing:
#' - `$.correlation`: A tibble with columns for the variable name, correlation value, and method used.
#' - `$.plot`: If `.plot = TRUE`, a `ggplot2` object (or a `plotly` object if `.interactive = TRUE`).
#'
#' @references
#' - Chatterjee, S. (2021). A new coefficient of correlation.
#'   Journal of the American Statistical Association, 116(536):2009-2022.
#' - Wilcox, R. (2012). Introduction to robust estimation and hypothesis testing (3rd ed.).
#'   Academic Press. (ISBN 978-0123869838).
#'
#' @author Christian L. Goueguel
#'
#' @export correlation
correlation <- function(.data, response_var, method = "pearson", .plot = FALSE, .color = "#111D71", .interactive = FALSE) {
  if (missing(.data)) {
    stop("Missing '.data' argument.")
  }
  if (!is.data.frame(.data) || !all(.data %>% purrr::map_lgl(is.numeric))) {
    stop("Input '.data' must be a numeric data frame")
  }
  if (!rlang::quo_name(rlang::enquo(response_var)) %in% colnames(.data)) {
    stop("'response_var' not found in the data frame")
  }
  valid_methods <- c("pearson", "spearman", "kendall", "chatterjee", "bicor")
  if (!method %in% valid_methods) {
    stop("Invalid method specified.")
  }
  if (!is.logical(.plot)) {
    stop("'.plot' must be of type boolean (TRUE or FALSE)")
  }

  variable <- NULL
  desc <- NULL
  .correlation <- NULL
  reorder <- NULL

  if (method != "kendall") {
    tbl_corr <- .data %>%
      corrr::correlate(
        method = ifelse(method == "spearman", "spearman", "pearson"),
        use = "pairwise.complete.obs",
        quiet = TRUE
      ) %>%
      tibble::rownames_to_column("variable") %>%
      dplyr::select(variable, {{response_var}}) %>%
      dplyr::rename(.correlation = {{response_var}}) %>%
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
      dplyr::rename(.correlation = {{response_var}}) %>%
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
      dplyr::rename(.correlation = {{response_var}}) %>%
      dplyr::mutate(method = method)
  }
  if (method == "bicor") {
    tbl_corr <- .data %>%
      dplyr::select(-{{response_var}}) %>%
      purrr::map_dbl(~ biweight_midcorrelation(X = ., Y = .data[[ rlang::ensym(response_var) ]])) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("variable") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(method = method)
    colnames(tbl_corr) <- c("variable", "correlation", "method")
  }

  if (method != "chatterjee" && method != "bicor") {
    tbl_corr$variable <- names(.data)
    tbl_corr <- tbl_corr %>%
      tidyr::drop_na() %>%
      dplyr::arrange(desc(.correlation))
  } else if (method == "chatterjee") {
    tbl_corr$variable <- names(.data)
    tbl_corr <- tbl_corr %>%
      dplyr::filter(variable != rlang::quo_name(rlang::enquo(response_var))) %>%
      tidyr::drop_na() %>%
      dplyr::arrange(desc(.correlation))
  } else {
    tbl_corr <- tbl_corr %>%
      tidyr::drop_na() %>%
      dplyr::arrange(desc(.correlation))
  }

  p <- tbl_corr %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = reorder(variable, -.correlation), y = .correlation, fill = method) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_vline(xintercept = 0, colour = "white", linewidth = 1) +
    ggplot2::scale_fill_manual(values = .color) +
    ggplot2::scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1), limits = c(-1, 1)) +
    ggplot2::labs(x = " ", y = paste0(rlang::quo_name(rlang::enquo(response_var)), " ", "correlation")) +
    ggplot2::coord_flip() +
    cowplot::theme_minimal_vgrid() +
    ggplot2::theme(legend.position = "top")

  if (.plot == FALSE) {
    return(tbl_corr)
  } else {
    if (.interactive == FALSE) {
      return(list(.correlation = tbl_corr, .plot = p))
    } else {
      return(plotly::ggplotly(p, tooltip = "y"))
    }
  }
}
