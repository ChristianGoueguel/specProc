#' @title Correlation Coefficients: Pearson, Spearman, Kendall, Chatterjee, and Biweight Midcorrelation
#'
#' @description
#' Computes various correlation coefficients between a specified response variable
#' and each of the remaining variables in a given data frame or tibble. The available correlation
#' methods are Pearson's product-moment correlation (parametric), Spearman's rank correlation,
#' Kendall's tau correlation (non-parametric), Chatterjee's new correlation coefficient, and
#' the biweight midcorrelation (a robust correlation measure).
#'
#' @details
#' The Pearson correlation coefficient measures the linear relationship between two continuous
#' variables and is suitable when the data follows a bivariate normal distribution. The Spearman
#' and Kendall correlations are non-parametric measures of monotonic association, making them
#' suitable for non-linear relationships and when the data deviates from normality.
#' The Chatterjee correlation coefficient \eqn{\xi_n(X, Y)} measures how much the response
#' `var` is a (possibly non-monotonic) function of each other variable; it lies between
#' 0 and 1 (asymptotically) and is not symmetric. The biweight midcorrelation is
#' a robust correlation measure that downweights the influence of outliers and is
#' recommended when the data contains extreme values or deviates significantly
#' from normality.
#'
#' Missing values are handled pairwise: each coefficient uses the observations where both
#' the response and the other variable are available.
#'
#' @param x A data frame or tibble containing the variables of interest.
#' @param var The response variable, given unquoted or as a string.
#' @param method A character string indicating the correlation method to use. Allowed values are "pearson", "spearman", "kendall", "chatterjee", or "bicor" (for biweight midcorrelation). The default is "pearson".
#' @param plot A logical value indicating whether to produce a visualization of the correlations. Default is FALSE (no plot).
#' @param color A character string specifying the color to use for the plot. Default is "#111D71".
#' @param interactive A logical value indicating whether to create an interactive plot using plotly. Default is FALSE (static ggplot2 plot).
#'
#' @return
#' - If `plot = FALSE`, a tibble with columns `variable`, `.correlation` and `method`, sorted by decreasing correlation.
#' - If `plot = TRUE`, a list containing the tibble (`correlation`) and a `ggplot2` object (`plot`).
#' - If `plot = TRUE` and `interactive = TRUE`, a `plotly` object.
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
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(50))
#' df$a <- 2 * df$y + rnorm(50, sd = 0.5)
#' df$b <- -df$y + rnorm(50)
#' df$c <- rnorm(50)
#' correlation(df, y)
#' correlation(df, "y", method = "bicor")
#'
correlation <- function(x, var, method = "pearson", plot = FALSE, color = "#111D71", interactive = FALSE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.data.frame(x) || !all(vapply(x, is.numeric, logical(1)))) {
    stop("Input 'x' must be a numeric data frame")
  }
  var_name <- rlang::as_name(rlang::enquo(var))
  if (!var_name %in% colnames(x)) {
    stop("'var' not found in the data frame")
  }
  valid_methods <- c("pearson", "spearman", "kendall", "chatterjee", "bicor")
  if (!is.character(method) || length(method) != 1 || !method %in% valid_methods) {
    stop("Invalid method specified.")
  }
  if (!is.logical(plot)) {
    stop("'plot' must be of type boolean (TRUE or FALSE)")
  }
  check_flag(interactive, "interactive")

  response <- x[[var_name]]
  others <- setdiff(names(x), var_name)

  coef_fun <- switch(
    method,
    pearson = function(a, b) stats::cor(a, b, method = "pearson"),
    spearman = function(a, b) stats::cor(a, b, method = "spearman"),
    kendall = function(a, b) stats::cor(a, b, method = "kendall"),
    chatterjee = function(a, b) XICOR::calculateXI(a, b),
    bicor = function(a, b) tryCatch(biweight_midcorrelation(a, b), error = function(e) NA_real_)
  )

  values <- vapply(others, function(nm) {
    v <- x[[nm]]
    ok <- stats::complete.cases(v, response)
    if (sum(ok) < 3 || length(unique(v[ok])) < 2 || length(unique(response[ok])) < 2) {
      return(NA_real_)
    }
    as.numeric(coef_fun(v[ok], response[ok]))
  }, numeric(1))

  .correlation <- variable <- NULL

  tbl_corr <- tibble::tibble(
    variable = others,
    .correlation = unname(values),
    method = method
  )
  tbl_corr <- tbl_corr[!is.na(tbl_corr$.correlation), ]
  tbl_corr <- tbl_corr[order(tbl_corr$.correlation, decreasing = TRUE), ]

  if (!plot) {
    return(tbl_corr)
  }

  p <- ggplot2::ggplot(tbl_corr) +
    ggplot2::aes(x = stats::reorder(variable, .correlation), y = .correlation, fill = method) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = color) +
    ggplot2::scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1), limits = c(-1, 1)) +
    ggplot2::labs(x = NULL, y = paste(var_name, "correlation")) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (!interactive) {
    return(list(correlation = tbl_corr, plot = p))
  }
  rlang::check_installed("plotly", reason = "to create interactive plots.")
  return(plotly::ggplotly(p, tooltip = "y"))
}
