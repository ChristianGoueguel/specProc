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
#'    - If `plot = FALSE`, returns a list of tibbles with the generalized boxplot statistics and potantial outliers.
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
  if (!all(x %>% purrr::map_lgl(is.numeric))) {
    stop("Input 'x' must be a numeric data frame.")
  }
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("Argument 'alpha' must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(p) || p < 0.5 || p > 1) {
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

  genBoxplot_stats <- x %>%
    purrr::map(
      function(.x) {
        stats_tbl <- genboxStats(.x, alpha, p)
        tibble::tibble(
          lower = stats_tbl$stats$lower_fence,
          q1 = stats_tbl$stats$lower_quantile,
          median = stats_tbl$stats$median,
          q3 = stats_tbl$stats$upper_quantile,
          upper = stats_tbl$stats$upper_fence
        )
      }
    ) %>%
    dplyr::bind_rows(.id = "variable") %>%
    purrr::modify_at("variable", forcats::as_factor)

  genBoxplot_out <- x %>%
    purrr::map(
      function(.x) {
        out_tbl <- genboxStats(.x, alpha, p)
        tibble::tibble(
          lower = out_tbl$outliers$lower,
          upper = out_tbl$outliers$upper
        )
      }
    ) %>%
    dplyr::bind_rows(.id = "variable") %>%
    purrr::modify_at("variable", forcats::as_factor) %>%
    tidyr::pivot_longer(!variable, names_to = "out", values_to = "value")

  variable <- NULL
  lower <- NULL
  q1 <- NULL
  median <- NULL
  q3 <- NULL
  upper <- NULL
  value <- NULL

  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = genBoxplot_stats,
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
       data = genBoxplot_out,
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
    r <- list("stats" = genBoxplot_stats, "outliers" = genBoxplot_out)
    return(r)
  }
}


genboxStats <- function(x, alpha, p) {
  n <- length(x)
  order_x <- sort(x)
  x_star <- (x - stats::median(x)) / stats::IQR(x)
  r <- x_star - min(x_star) + 0.1
  r_tilde <- r / (min(r) + max(r))
  w <- stats::qnorm((rank(r_tilde, na.last = "keep") - 0.5) / sum(!is.na(r_tilde)))
  w_star <- (w - stats::median(w)) / (stats::IQR(w) / 1.3426)

  A <- stats::quantile(w_star, p) / stats::quantile(w_star, 1 - p)
  B <- (stats::quantile(w_star, p) * stats::quantile(w_star, 1 - p)) / (stats::quantile(w_star, p) + stats::quantile(w_star, 1 - p))
  z <- stats::qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  g <- (1 / z) * log(-A)
  h <- (2 / z^2) * log(-g * B)

  xi_alpha_2 <- tukeyGH(alpha / 2, type = "q", location = 0, scale = 1, g = g, h = h)
  xi_1_alpha_2 <- tukeyGH(1 - alpha / 2, type = "q", location = 0, scale = 1, g = g, h = h)

  atypical_indices <- which(w_star < xi_alpha_2 | w_star > xi_1_alpha_2)

  L_star_minus <- xi_alpha_2
  L_star_plus <- xi_1_alpha_2
  C_low <- stats::median(w) + (stats::IQR(w) / 1.3426) * L_star_minus
  C_up <- stats::median(w) + (stats::IQR(w) / 1.3426) * L_star_plus
  f_low <- stats::pnorm(C_low, mean = 0, sd = 1) * (min(r) + max(r)) + min(x_star) - 0.1
  f_up <- stats::pnorm(C_up, mean = 0, sd = 1) * (min(r) + max(r)) + min(x_star) - 0.1

  q1 <- stats::quantile(x, 0.25)
  q3 <- stats::quantile(x, 0.75)

  s <- tibble::tibble(
    lower_fence = f_low * stats::IQR(x) + stats::median(x),
    lower_quantile = q1,
    median = stats::median(x),
    upper_quantile = q3,
    upper_fence = f_up * stats::IQR(x) + stats::median(x)
  )

  out <- tibble::tibble(
    lower = order_x[order_x < s$lower_fence],
    upper = order_x[order_x > s$upper_fence]
  )

  results <- list("stats" = s, "outliers" = out)
  return(results)
}

