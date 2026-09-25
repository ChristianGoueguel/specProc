#' @title Generalized Boxplot
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the generalized boxplot, a robust data visualization
#' technique designed to effectively represent skewed and heavy-tailed distributions,
#' as proposed by Bruffaerts *et al*. (2014).
#'
#' @details
#' This method extends the adjusted boxplot method by leveraging the flexible Tukey's
#' g-and-h parametric distribution to model the underlying data structure,
#' particularly for asymmetric or long-tailed datasets, providing a more nuanced
#' and informative summary of the data's central tendency, spread, and potential
#' outliers.
#'
#' @references
#'  - Bruffaerts, C., Verardi, V., Vermandele, C. (2014). A generalized boxplot for
#'    skewed and heavy-tailed distributions. Statistics and Probability Letters 95(C):110–117
#'
#' @param x A numeric data frame or tibble.
#' @param alpha A scalar, between 0 and 1 that specifies the desired detection rate of atypical values.
#' @param p A scalar, between 0.5 and 1 that specifies the quantile order for estimating g and h.
#' @param plot Logical value indicating whether to plot the boxplot or return the boxplot statistics.
#' @param xlabels.angle A numeric value specifying the angle (in degrees) for x-axis labels (default is 90).
#' @param xlabels.vjust A numeric value specifying the vertical justification of x-axis labels (default is 1).
#' @param xlabels.hjust A numeric value specifying the horizontal justification of x-axis labels (default is 1).
#' @param box.width A numeric value specifying the width of the boxplot (default is 0.5).
#' @param notch A logical value indicating whether to display a notched boxplot (default is `FALSE`).
#' @param notchwidth A numeric value specifying the width of the notch relative to the body of the boxplot (default is 0.5).
#' @param staplewidth A numeric value specifying the width of staples at the ends of the whiskers.
#'
#' @return
#'    - If `plot = TRUE`, returns a `ggplot2` object containing the generalized boxplot.
#'    - If `plot = FALSE`, returns a list of tibbles: `stats`, with the whisker
#'      ends (`lower`, `upper`: the most extreme observations within the fences),
#'      quartiles, median, fences and the estimated g and h parameters of each variable,
#'      and `outliers`, with the potential outliers (`out` gives the tail).
#'
#' @export generalized_boxplot
#'
#' @examples
#' set.seed(123)
#' data <- data.frame(
#'   normal = rnorm(100),
#'   skewed = rexp(100, rate = 0.5),
#'   heavy_tailed = rt(100, df = 3)
#' )
#'
#' # Plot the generalized boxplot
#' generalized_boxplot(data)
#'
#' # Retrieve the generalized boxplot statistics
#' generalized_boxplot(data, plot = FALSE)
#'
generalized_boxplot <- function(x, alpha = 0.05, p = 0.9, plot = TRUE, xlabels.angle = 90, xlabels.vjust = 1, xlabels.hjust = 1, box.width = .5, notch = FALSE, notchwidth = 0.5, staplewidth = 0.5) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (is.matrix(x)) {
    x <- as.data.frame(x)
  }
  if (!is.data.frame(x) || !all(vapply(x, is.numeric, logical(1)))) {
    stop("Input 'x' must be a numeric data frame.")
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("Argument 'alpha' must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(p) || p <= 0.5 || p >= 1) {
    stop("Argument 'p' must be a numeric value between 0.5 and 1.")
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

  genBoxplot_stats <- list()
  genBoxplot_out <- list()
  for (nm in names(x)) {
    st <- genboxStats(x[[nm]], alpha, p)
    genBoxplot_stats[[nm]] <- tibble::tibble(
      lower = st$stats$lower_whisker,
      q1 = st$stats$lower_quantile,
      median = st$stats$median,
      q3 = st$stats$upper_quantile,
      upper = st$stats$upper_whisker,
      lower_fence = st$stats$lower_fence,
      upper_fence = st$stats$upper_fence,
      g = st$stats$g,
      h = st$stats$h
    )
    genBoxplot_out[[nm]] <- st$outliers
  }
  genBoxplot_stats <- dplyr::bind_rows(genBoxplot_stats, .id = "variable")
  genBoxplot_stats$variable <- factor(genBoxplot_stats$variable, levels = names(x))
  genBoxplot_out <- dplyr::bind_rows(genBoxplot_out, .id = "variable")
  if (nrow(genBoxplot_out) == 0) {
    genBoxplot_out <- tibble::tibble(variable = character(), out = character(), value = numeric())
  }
  genBoxplot_out$variable <- factor(genBoxplot_out$variable, levels = names(x))

  if (!plot) {
    return(list("stats" = genBoxplot_stats, "outliers" = genBoxplot_out))
  }
  boxplot_stats_plot(genBoxplot_stats, genBoxplot_out, xlabels.angle, xlabels.vjust,
                     xlabels.hjust, box.width, notch, notchwidth, staplewidth)
}

# Draws a boxplot from precomputed statistics (lower, q1, median, q3, upper)
# and outliers (variable, value). Shared by the adjusted and generalized boxplots.
boxplot_stats_plot <- function(stats_tbl, outlier_tbl, xlabels.angle, xlabels.vjust,
                               xlabels.hjust, box.width, notch, notchwidth, staplewidth) {
  variable <- lower <- q1 <- median <- q3 <- upper <- value <- NULL

  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = stats_tbl,
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
    ggsci::scale_fill_d3(palette = "category20") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = xlabels.angle, vjust = xlabels.vjust, hjust = xlabels.hjust)) +
    ggplot2::labs(x = " ", y = " ")
}


# Generalized boxplot statistics for one variable (Bruffaerts et al., 2014).
genboxStats <- function(x, alpha, p) {
  x <- x[!is.na(x)]
  if (length(x) < 5 || stats::IQR(x) == 0) {
    stop("Each variable must have at least 5 non-missing values and a non-zero IQR.", call. = FALSE)
  }
  med <- stats::median(x)
  iqr <- stats::IQR(x)

  # 1. Map the data into (0, 1) and then onto the real line.
  x_star <- (x - med) / iqr
  r <- x_star - min(x_star) + 0.1
  s <- min(r) + max(r)
  w <- stats::qnorm(r / s)
  w_med <- stats::median(w)
  w_scale <- stats::IQR(w) / 1.3426
  w_star <- (w - w_med) / w_scale

  # 2. Quantile-based estimates of the Tukey g-and-h parameters.
  z <- stats::qnorm(p)
  Qp <- unname(stats::quantile(w_star, p))
  Q1p <- unname(stats::quantile(w_star, 1 - p))
  ratio <- -Qp / Q1p
  if (is.finite(ratio) && ratio > 0 && abs(log(ratio)) > 1e-8) {
    g <- log(ratio) / z
    h <- 2 * log(-g * Qp * Q1p / (Qp + Q1p)) / z^2
  } else {
    g <- 0
    h <- 2 * log((Qp - Q1p) / (2 * z)) / z^2
  }
  if (!is.finite(h) || h < 0) h <- 0

  # 3. Fences as g-and-h quantiles, transformed back to the original scale.
  xi <- tukey_gh(c(alpha / 2, 1 - alpha / 2), type = "q", location = 0, scale = 1, g = g, h = h)
  back <- function(q) {
    r_q <- stats::pnorm(w_med + w_scale * q) * s
    (r_q + min(x_star) - 0.1) * iqr + med
  }
  fences <- back(xi)

  inside <- x[x >= fences[1] & x <= fences[2]]
  stats_tbl <- tibble::tibble(
    lower_whisker = min(inside),
    upper_whisker = max(inside),
    lower_fence = fences[1],
    lower_quantile = unname(stats::quantile(x, 0.25)),
    median = med,
    upper_quantile = unname(stats::quantile(x, 0.75)),
    upper_fence = fences[2],
    g = g,
    h = h
  )

  order_x <- sort(x)
  low <- order_x[order_x < fences[1]]
  high <- order_x[order_x > fences[2]]
  out <- tibble::tibble(
    out = c(rep("lower", length(low)), rep("upper", length(high))),
    value = c(low, high)
  )

  list("stats" = stats_tbl, "outliers" = out)
}
