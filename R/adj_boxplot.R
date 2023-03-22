#' @title Function to Compute Adjusted Boxplot for Skewed Distribution
#' @author Christian L. Goueguel
#' @source G. Brys, M. Hubert and A. Struyf, A Robust Measure of Skewness. J. Comput. Graph. Stat. 13 (2004); M. Hubert and E. Vandervieren, An adjusted boxplot for skewed distributions, Comput. Stat. Data Anal., 52 (2008
#' @param .data data frame or tibble (must contain numeric columns).
#' @param .plot plot the adjusted boxplot (TRUE by default).
#' @return A list that contains:
#' @return \item{`adj_box_stat`}{data frame of the adjusted boxplot statistics}
#' @return \item{`plot`}{ggplot2 object (if `.plot = TRUE`)}
#' @export adj_boxplot
adj_boxplot <- function(.data, .plot = TRUE) {
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
  if(!is.logical(.plot)) {
    stop("Argument '.plot' must be of type boolean (TRUE or FALSE).")
  }

  # Calculate adjusted boxplot values for each column
  adj_boxplot_vals <- .data %>%
    purrr::map_dfr(
      function(.x) {
        adj_box <- robustbase::adjboxStats(.x)
        tibble::tibble(
          lower = adj_box$stats[1],
          q1 = adj_box$stats[2],
          median = adj_box$stats[3],
          q3 = adj_box$stats[4],
          upper = adj_box$stats[5],
          medcouple = robustbase::mc(.x)
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
    ggplot2::geom_boxplot(
      stat = "identity",
      width = 0.2,
      colour = "black",
      outlier.colour = "black",
      outlier.shape = 21,
      outlier.size = 1.5
      ) +
    ggplot2::stat_boxplot(geom = "errorbar", width = 0.2) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none", panel.grid = element_blank()) +
    ggplot2::labs(
      title = "Adjusted Boxplots",
      x = " ",
      y = " "
    )

  if (.plot == TRUE) {
    res <- list(adj_box_stat = adj_boxplot_vals, plot = plot)
  } else{
    res <- list(adj_box_stat = adj_boxplot_vals)
  }
  return(res)
}
