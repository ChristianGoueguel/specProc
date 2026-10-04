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
#' The data \eqn{x_i} are first mapped, preserving their ranks, into (0, 1)
#' by \eqn{r_i = (\tilde{x}_i - \min_j \tilde{x}_j + 0.1) /
#' (\max_j \tilde{x}_j - \min_j \tilde{x}_j + 0.2)}, with
#' \eqn{\tilde{x}_i = (x_i - \text{median}) / \text{IQR}}, and then onto the
#' real line by \eqn{w_i = \Phi^{-1}(r_i)}. The \eqn{w_i}, standardized by
#' their median and their interquartile range divided by
#' \eqn{z_{0.75} - z_{0.25} = 1.349}, are fitted by a Tukey g-and-h
#' distribution, whose skewness \eqn{g} and tail heaviness \eqn{h} are
#' estimated from the quantiles of orders \eqn{p} and \eqn{1 - p}. The fences
#' are the quantiles of orders \eqn{\alpha/2} and \eqn{1 - \alpha/2} of the
#' fitted distribution, mapped back to the scale of the data: a proportion
#' \eqn{\alpha} of the observations of a clean distribution, of whatever
#' skewness and tails, is expected outside. The default \eqn{\alpha =
#' 2\,\Phi(-4 z_{0.75}) \approx 0.7\%}, the rate of Tukey's boxplot for normal
#' data, is that of the authors' implementation (the Stata command `robbox`
#' of Jann, Verardi and Vermandele). The fences are not inside the box, and
#' the whiskers end at the most extreme observations within the fences.
#'
#' The outlying observations are therefore *atypical at the rate*
#' \eqn{\alpha}: about \eqn{\alpha n} of them are expected in clean data, so
#' that with a large \eqn{\alpha} (such as 5%) many flagged observations are
#' not errors. As \eqn{h} may be negative (tails lighter than normal), where
#' the g-and-h transform turns back before the \eqn{\alpha/2} quantile the
#' fence is its extreme value.
#'
#' The layout, the points and the options for publication figures are those
#' of [adjusted_boxplot()]: see its details.
#'
#' @references
#'  - Bruffaerts, C., Verardi, V., Vermandele, C. (2014). A generalized boxplot for
#'    skewed and heavy-tailed distributions. Statistics and Probability Letters 95(C):110–117
#'  - Verardi, V., Vermandele, C. (2016). Outlier identification for skewed and/or
#'    heavy-tailed unimodal multivariate distributions. Journal de la Société
#'    Française de Statistique, 157(2):90–114
#'
#' @inheritParams adjusted_boxplot
#' @param alpha The expected proportion of observations outside the fences in
#'   clean data, between 0 and 1. Default is \eqn{2\,\Phi(-4 z_{0.75}) \approx
#'   0.007}, as Tukey's boxplot for normal data.
#' @param p The quantile order, between 0.5 and 1, of the estimation of g and
#'   h. Default is 0.9 (a breakdown point of 10%).
#'
#' @return
#'    - If `plot = TRUE`, a `ggplot2` object.
#'    - If `plot = FALSE`, a list of two tibbles: `stats`, with one row per
#'      variable (and group): the number of values `n`, the whisker ends
#'      `lower` and `upper` (the most extreme values within the fences), the
#'      quartiles `q1` and `q3`, the `median`, the fences, the notch limits,
#'      the estimated `g` and `h`, the `mean` and the number of outlying values
#'      `n_outliers`; and `outliers`, with the outlying values, their `row` in
#'      `x`, their `id` and their tail `out` (`"lower"` or `"upper"`).
#'
#' @seealso [adjusted_boxplot()]
#'
#' @export generalized_boxplot
#'
#' @examples
#' data(forageLIBS)
#' minerals <- forageLIBS[c("Measurement", "Ca", "Mg", "P", "K", "S")]
#'
#' # mineral contents, each on its own axis, the outlying samples named: a
#' # figure of a journal page width
#' p <- generalized_boxplot(minerals, id = Measurement, label_outliers = TRUE,
#'                          ylab = "Content (%)", base_size = 8)
#' p
#' # ggplot2::ggsave("minerals.pdf", p, width = 175, height = 70, units = "mm")
#'
#' # the statistics and the outlying samples
#' res <- generalized_boxplot(minerals, id = Measurement, plot = FALSE)
#' res$stats
#' res$outliers
#'
#' # with a detection rate of 5%, about 18 of the 368 samples would be
#' # flagged per mineral even in clean data
#' generalized_boxplot(minerals, id = Measurement, alpha = 0.05, plot = FALSE)$stats
#'
#' # potassium and phosphorus by calcium level, with notches and means: a
#' # figure of a journal column
#' minerals$Ca_level <- cut(minerals$Ca, quantile(minerals$Ca, 0:3 / 3),
#'                          labels = c("Low Ca", "Mid Ca", "High Ca"),
#'                          include.lowest = TRUE)
#' p <- generalized_boxplot(minerals[c("K", "P", "Ca_level")], group = Ca_level,
#'                          notch = TRUE, show_mean = TRUE, ylab = "Content (%)",
#'                          title = "Potassium and phosphorus by calcium level",
#'                          base_size = 8)
#' p
#' # ggplot2::ggsave("by_calcium.pdf", p, width = 85, height = 75, units = "mm")
#'
#' # trace elements on a logarithmic axis, horizontal, with all the samples
#' traces <- forageLIBS[c("Fe", "Mn", "Zn")]
#' generalized_boxplot(traces, log = TRUE, horizontal = TRUE, points = "all",
#'                     ylab = "Content (mg/kg)")
generalized_boxplot <- function(x, alpha = 2 * stats::pnorm(-4 * stats::qnorm(0.75)), p = 0.9,
                                plot = TRUE, id = NULL, group = NULL,
                                scales = c("free_y", "fixed"),
                                points = c("outliers", "all", "none"), label_outliers = FALSE,
                                show_n = TRUE, show_mean = FALSE, horizontal = FALSE,
                                log = FALSE, fill = "grey85", xlab = NULL, ylab = NULL,
                                title = NULL, caption = TRUE, base_size = 11, x_labels_angle = 0,
                                box_width = 0.5, notch = FALSE, notch_width = 0.5,
                                staple_width = 0.5, xlabels.angle = deprecated(),
                                xlabels.vjust = deprecated(), xlabels.hjust = deprecated(),
                                box.width = deprecated(), notchwidth = deprecated(),
                                staplewidth = deprecated()) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("Argument 'alpha' must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(p) || length(p) != 1 || is.na(p) || p <= 0.5 || p >= 1) {
    stop("Argument 'p' must be a numeric value between 0.5 and 1.")
  }
  if (!is.logical(plot) || length(plot) != 1 || is.na(plot)) {
    stop("Argument 'plot' must be of type boolean (TRUE or FALSE).")
  }
  args <- boxplot_args(
    "generalized_boxplot", rlang::caller_env(), scales = scales, points = points,
    label_outliers = label_outliers, show_n = show_n, show_mean = show_mean,
    horizontal = horizontal, log = log, fill = fill, xlab = xlab, ylab = ylab, title = title,
    caption = caption, base_size = base_size, x_labels_angle = x_labels_angle,
    box_width = box_width, notch = notch, notch_width = notch_width, staple_width = staple_width,
    xlabels.angle = xlabels.angle, xlabels.vjust = xlabels.vjust, xlabels.hjust = xlabels.hjust,
    box.width = box.width, notchwidth = notchwidth, staplewidth = staplewidth
  )
  input <- boxplot_input(x, rlang::enquo(id), rlang::enquo(group))
  res <- robust_boxplot_data(input$x, input$vars, input$id, input$group,
                             function(v) generalized_stats(v, alpha, p))
  if (!plot) {
    return(res[c("stats", "outliers")])
  }
  robust_boxplot_plot(res, args, boxplot_caption("generalized", alpha))
}

# Generalized boxplot statistics of one variable (Bruffaerts et al., 2014),
# as in the Stata command robbox of Jann, Verardi and Vermandele, with the
# interquartile range as the scale of the first step, as in the paper.
generalized_stats <- function(x, alpha, p) {
  if (length(x) < 5 || stats::IQR(x) == 0) {
    stop("Each variable must have at least 5 non-missing values and a non-zero IQR.", call. = FALSE)
  }
  med <- stats::median(x)
  iqr <- stats::IQR(x)
  quartiles <- unname(stats::quantile(x, c(0.25, 0.75)))

  # 1. Map the data into (0, 1), preserving their ranks, then onto the real
  # line, and standardize them (normal-consistent IQR).
  x_star <- (x - med) / iqr
  r <- x_star - min(x_star) + 0.1
  s <- min(r) + max(r)
  w <- stats::qnorm(r / s)
  w_med <- stats::median(w)
  w_scale <- stats::IQR(w) / diff(stats::qnorm(c(0.25, 0.75)))
  w_star <- (w - w_med) / w_scale

  # 2. Quantile-based estimates of the Tukey g-and-h parameters; h may be
  # negative (tails lighter than normal).
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
  if (!is.finite(h)) h <- 0

  # 3. Fences: the alpha/2 and 1 - alpha/2 quantiles of the fitted g-and-h,
  # mapped back to the scale of the data, and not inside the box. Where the
  # transform turns back (h < 0), the fence is its extreme value.
  tau <- function(u) {
    if (g == 0) u * exp(h * u^2 / 2) else (exp(g * u) - 1) / g * exp(h * u^2 / 2)
  }
  u <- seq(0, stats::qnorm(1 - alpha / 2), length.out = 1001)
  xi <- c(min(tau(-u)), max(tau(u)))
  back <- function(q) {
    r_q <- stats::pnorm(w_med + w_scale * q) * s
    (r_q + min(x_star) - 0.1) * iqr + med
  }
  fences <- back(xi)
  fences <- c(min(fences[1], quartiles[1]), max(fences[2], quartiles[2]))

  inside <- x[x >= fences[1] & x <= fences[2]]
  notch <- med + c(-1, 1) * 1.58 * iqr / sqrt(length(x))
  stats <- data.frame(
    lower = min(inside), q1 = quartiles[1], median = med, q3 = quartiles[2],
    upper = max(inside), lower_fence = fences[1], upper_fence = fences[2],
    notch_lower = notch[1], notch_upper = notch[2], g = g, h = h
  )
  tail <- ifelse(x < fences[1], "lower", ifelse(x > fences[2], "upper", NA_character_))
  list(stats = stats, tail = tail)
}
