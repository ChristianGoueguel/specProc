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
#'    - If `plot = FALSE`, returns a list of tibbles with the adjusted boxplot statistics and potential outliers.
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
  if (is.matrix(x)) {
    x <- as.data.frame(x)
  }
  if (!is.data.frame(x) || !all(vapply(x, is.numeric, logical(1)))) {
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

  adjBoxplot_stats <- list()
  outlier_tbl <- list()
  for (nm in names(x)) {
    v <- x[[nm]]
    v <- v[!is.na(v)]
    adj_box <- robustbase::adjboxStats(v, doScale = FALSE)
    adjBoxplot_stats[[nm]] <- tibble::tibble(
      lower = adj_box$stats[1],
      q1 = adj_box$stats[2],
      median = adj_box$stats[3],
      q3 = adj_box$stats[4],
      upper = adj_box$stats[5],
      medcouple = medcouple(v)
    )
    outlier_tbl[[nm]] <- tibble::tibble(value = adj_box$out)
  }
  adjBoxplot_stats <- dplyr::bind_rows(adjBoxplot_stats, .id = "variable")
  adjBoxplot_stats$variable <- factor(adjBoxplot_stats$variable, levels = names(x))
  outlier_tbl <- dplyr::bind_rows(outlier_tbl, .id = "variable")
  if (nrow(outlier_tbl) == 0) {
    outlier_tbl <- tibble::tibble(variable = character(), value = numeric())
  }
  outlier_tbl$variable <- factor(outlier_tbl$variable, levels = names(x))

  if (!plot) {
    return(list("stats" = adjBoxplot_stats, "outliers" = outlier_tbl))
  }
  boxplot_stats_plot(adjBoxplot_stats, outlier_tbl, xlabels.angle, xlabels.vjust,
                     xlabels.hjust, box.width, notch, notchwidth, staplewidth)
}
