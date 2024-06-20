#' @title Adjusted Boxplot
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function generates the adjusted boxplot, which is a robust graphical method
#' for visualizing skewed data distributions. It provides a more accurate representation
#' of the data's spread and skewness compared to standard boxplot, especially
#' in the presence of outliers.
#'
#' @details
#' The function is based on the medcouple (MC) measure computed on the data and which
#' robustly measures skewness. This measure is bounded between −1 and 1. The
#' medcouple is equal to zero when the observed distribution is symmetric,
#' whereas a positive (resp. negative) value of MC corresponds to a right
#' (resp. left) tailed distribution. It worth noting that this method is more appropriate for distributions
#' that are not excessively skewed i.e., for \eqn{|\text{MC}| \leq 0.6}.
#'
#' @references
#' The adjusted boxplot is based on the methodology described in:
#' - Brys, G., Hubert, M., Struyf, A., (2004). A Robust Measure of Skewness.
#'   Journal of Computational and Graphical Statistics, 13(4):996-1017
#' - Hubert, M., Vandervieren, E., (2008). An adjusted boxplot for skewed distributions.
#'   Computational Statistics and Data Analysis, 52(12):5186-5201
#'
#' @param x A numeric data frame or tibble.
#' @param plot A logical value indicating whether to plot the adjusted boxplot (default is `TRUE`).
#' @param xlabels.angle A numeric value specifying the angle (in degrees) for x-axis labels (default is 90).
#' @param xlabels.vjust A numeric value specifying the vertical justification of x-axis labels (default is 1).
#' @param xlabels.hjust A numeric value specifying the horizontal justification of x-axis labels (default is 1).
#' @param box.width A numeric value specifying the width of the boxplot (default is 0.5).
#' @param notch A logical value indicating whether to display a notched boxplot (default is `FALSE`).
#' @param notchwidth A numeric value specifying the width of the notch relative to the body of the boxplot (default is 0.5).
#' @param staplewidth A numeric value specifying the width of staples at the ends of the whiskers.
#'
#' @return
#'    - If `plot = TRUE`, returns a `ggplot2` object containing the adjusted boxplot.
#'    - If `plot = FALSE`, returns a list of tibbles with the adjusted boxplot statistics and potantial outliers.
#'
#' @export adjusted_boxplot
#'
#' @examples
#' set.seed(123)
#' data <- data.frame(
#'   normal = rnorm(100),
#'   skewed = rexp(100, rate = 0.5),
#'   heavy_tailed = rt(100, df = 3)
#' )
#'
#' # Plot the adjusted boxplot
#' adjusted_boxplot(data)
#'
#' # Retrieve the adjusted boxplot statistics
#' adjusted_boxplot(data, plot = FALSE)
#'
adjusted_boxplot <- function(x, plot = TRUE, xlabels.angle = 90, xlabels.vjust = 1, xlabels.hjust = 1, box.width = .5, notch = FALSE, notchwidth = 0.5, staplewidth = 0.5) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!all(x %>% purrr::map_lgl(is.numeric))) {
    stop("Input 'x' must be a numeric data frame.")
  }
  if(!is.logical(plot)) {
    stop("Argument 'plot' must be of type boolean (TRUE or FALSE).")
  }
  if (!is.logical(notch)) {
    stop("Argument 'notch' must be of type boolean (TRUE or FALSE).")
  }
  if (!is.numeric(xlabels.angle) || xlabels.angle < 0 || xlabels.angle > 360) {
    stop("Argument 'x_axis_angle' must be a numeric value between 0 and 360.")
  }
  if (!is.numeric(xlabels.vjust) || xlabels.vjust < 0 || xlabels.vjust > 1) {
    stop("Argument 'xlabels.vjust' must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(xlabels.hjust) || xlabels.hjust < 0 || xlabels.hjust > 1) {
    stop("Argument 'xlabels.hjust' must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(box.width) || box.width <= 0) {
    stop("Argument 'box.width' must be a positive numeric value.")
  }
  if (!is.logical(notch)) {
    stop("Argument 'notch' must be of type boolean (TRUE or FALSE).")
  }
  if (!is.numeric(notchwidth) || notchwidth < 0 || notchwidth > 1) {
    stop("Argument 'notchwidth' must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(staplewidth) || staplewidth < 0) {
    stop("Argument 'staplewidth' must be a positive numeric value.")
  }

  adjBoxplot_stats <- x %>%
    purrr::map(
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
      }
    ) %>%
    dplyr::bind_rows(.id = "variable") %>%
    purrr::modify_at("variable", forcats::as_factor)

  outlier_tbl <- x %>%
    purrr::map(
      function(.x) {
        out_tbl <- robustbase::adjboxStats(.x)
        tibble::tibble(value = out_tbl$out)
      }
    ) %>%
    dplyr::bind_rows(.id = "variable") %>%
    purrr::modify_at("variable", forcats::as_factor)

  variable <- NULL
  lower <- NULL
  q1 <- NULL
  median <- NULL
  q3 <- NULL
  upper <- NULL
  value <- NULL

  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = adjBoxplot_stats,
      ggplot2::aes(
        x = variable,
        ymin = lower,
        lower = q1,
        middle = median,
        upper = q3,
        ymax = upper,
        group = variable,
        fill = variable),
      stat = "identity",
      width = box.width,
      colour = "black",
      outlier.colour = NA,
      outlier.shape = NA,
      notch = notch,
      notchwidth = notchwidth,
      staplewidth = staplewidth) +
    ggplot2::geom_point(
      data = outlier_tbl,
      ggplot2::aes(
        x = variable,
        y = value,
        fill = variable,
        group = variable),
      shape = 21,
      size = 2,
      alpha = 1/3) +
    ggplot2::geom_jitter(size = 1.5) +
    ggsci::scale_fill_d3(palette = "category20") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = xlabels.angle, vjust = xlabels.vjust, hjust = xlabels.hjust)) +
    ggplot2::labs(x = " ", y = " ")

  if (plot == TRUE) {
    return(p)
  } else{
    r <- list("stats" = adjBoxplot_stats, "outliers" = outlier_tbl)
    return(r)
  }
}
