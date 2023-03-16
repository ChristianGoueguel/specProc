#' @title Function to Compute Adjusted Boxplot for Skewed Distribution
#' @author Christian L. Goueguel
#' @source M. Hubert and E. Vandervieren, An adjusted boxplot for skewed distributions, Computational Statistics & Data Analysis, Vol.52, 12, 2008
#' @param .data data frame or tibble (must contain numeric columns).
#' @return ggplot2 object.
#' @export adjusted_boxplot
adjusted_boxplot <- function(.data) {

  require(robustbase)
  require(ggplot2)
  require(dplyr)
  require(purrr)

  # check input validity
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!all(.data %>% map_lgl(is.numeric))) {
    stop("Input must be a numeric data frame.")
  }

  # Calculate adjusted boxplot values for each column using tidyverse syntax
  adj_boxplot_vals <- .data %>%
    map(
      function(col) {
        adj_box <- robustbase::adjboxStats(col)
        tibble(
          lower = adj_box$stats[1],
          q1 = adj_box$stats[2],
          median = adj_box$stats[3],
          q3 = adj_box$stats[4],
          upper = adj_box$stats[5]
        )
        }
      )

  # Convert list to data frame
  adj_boxplot_tbl <- bind_rows(adj_boxplot_vals, .id = "variable")
  adj_boxplot_tbl$variable <- factor(adj_boxplot_tbl$variable, levels = names(.data))

  # Create ggplot2 object
  plot <- ggplot(
    adj_boxplot_tbl,
    aes(
      x = variable,
      ymin = lower,
      lower = q1,
      middle = median,
      upper = q3,
      ymax = upper
      )
    ) +
    geom_boxplot(stat = "identity") +
    theme_bw() +
    labs(
      title = "Adjusted Boxplots for Skewed Distributions",
      x = "Variables",
      y = "Values"
      )

  return(plot)
}
