#' @title Function to Compute Adjusted Boxplot for Skewed Distribution
#' @author Christian L. Goueguel
#' @source M. Hubert and E. Vandervieren, An adjusted boxplot for skewed distributions, Comput. Stat. Data Anal., Vol.52, 12, 2008
#' @param .data data frame or tibble (must contain numeric columns).
#' @return ggplot2 object.
#' @export adj_boxplot
adj_boxplot <- function(.data) {
  requireNamespace("robustbase", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)

  # Check input validity
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!all(.data %>% purrr::map_lgl(is.numeric))) {
    stop("Input must be a numeric data frame.")
  }

  # Calculate adjusted boxplot values for each column using tidyverse syntax
  adj_boxplot_vals <- .data %>%
    purrr::map_dfr(
      function(col) {
        adj_box <- robustbase::adjboxStats(col)
        tibble::tibble(
          lower = adj_box$stats[1],
          q1 = adj_box$stats[2],
          median = adj_box$stats[3],
          q3 = adj_box$stats[4],
          upper = adj_box$stats[5]
        )
      },
      .id = "variable"
    ) %>%
    purrr::modify_at("variable", forcats::as_factor)

  # Create ggplot2 object
  plot <- adj_boxplot_vals %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = variable,
      ymin = lower,
      lower = q1,
      middle = median,
      upper = q3,
      ymax = upper,
      group = variable,
      fill = variable
    ) +
    ggplot2::geom_boxplot(stat = "identity", width = 0.1, colour = "black") +
    #ggplot2::facet_wrap(vars(variable), scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none", panel.grid = element_blank()) +
    ggplot2::labs(
      title = "Adjusted Boxplots",
      x = " ",
      y = " "
    )
  res <- list(adj_box_stat = adj_boxplot_vals, plot = plot)
  return(res)
}
