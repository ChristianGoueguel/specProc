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
#' robustly measures skewness. This measure is bounded between -1 and 1. The
#' medcouple is equal to zero when the observed distribution is symmetric,
#' whereas a positive (resp. negative) value of MC corresponds to a right
#' (resp. left) tailed distribution. The fences are, for \eqn{\text{MC} \geq 0},
#' \deqn{[Q_1 - 1.5 e^{-4\,\text{MC}}\,\text{IQR};\; Q_3 + 1.5 e^{3\,\text{MC}}\,\text{IQR}]}
#' and, for \eqn{\text{MC} < 0},
#' \deqn{[Q_1 - 1.5 e^{-3\,\text{MC}}\,\text{IQR};\; Q_3 + 1.5 e^{4\,\text{MC}}\,\text{IQR}],}
#' calibrated by Hubert and Vandervieren (2008) so that about 0.7% of the
#' observations of a clean, moderately skewed distribution are flagged, as
#' with Tukey's boxplot for normal data. The whiskers end at the most extreme
#' observations within the fences, and the observations outside are drawn as
#' points. The statistics are computed by [robustbase::adjboxStats()], whose
#' quartiles are Tukey's hinges, as in [boxplot()]. The method suits
#' distributions that are not excessively skewed, i.e. with
#' \eqn{|\text{MC}| \leq 0.6}.
#'
#' **Layout.** Each variable is drawn in its own panel, with its own value
#' axis (`scales = "free_y"`), so that variables of different scales or units
#' stay readable; `scales = "fixed"` draws them in one panel, on a common
#' axis. With `group`, the boxes of the groups are side by side in the panel
#' of each variable. Below each box, `n` is the number of non-missing values.
#'
#' **Points.** By default, the outlying observations are drawn, slightly
#' spread sideways so that equal values do not hide each other.
#' `points = "all"` draws every observation, the outlying ones in black,
#' as some journals require; `label_outliers = TRUE` names the outlying
#' observations by `id` (else by row number), and the `outliers` table of
#' `plot = FALSE` gives them all.
#'
#' **Annotations.** `annotate` adds, for each box:
#' - `"shape"`: the medcouple (`MC`, marked `(> 0.6)` beyond the range of
#'   the method), or the `g` and `h` of the generalized boxplot, which
#'   explain the asymmetry of the whiskers;
#' - `"outliers"`: the number of flagged values, with the number expected in
#'   clean data (about 0.7% for the adjusted boxplot, which Hubert and
#'   Vandervieren calibrated as Tukey's boxplot for normal data, and `alpha`
#'   for the generalized one): many more flagged values than expected point
#'   to real outliers;
#' - `"fences"`: the fences beyond which values are flagged, as dashed marks
#'   (on the sides with outlying values), the whiskers ending at the last
#'   values inside;
#' - `"median"`: the median with its distribution-free 95% confidence
#'   interval, from the order statistics (`Md`); the notches are made to
#'   compare two medians, and are not a confidence interval of one;
#' - `"spread"`: the interquartile range and the robust coefficient of
#'   variation \eqn{\text{rCV} = \text{IQR} / (1.349\,\text{median})}, in
#'   percent, read as a relative standard deviation;
#' - `"location"`: the biweight location ([biweight_location()]), a robust
#'   mean, marked by a cross (the mean, with `show_mean`, is pulled by the
#'   outlying values);
#' - `"test"`: with `group`, the p-value of the Kruskal-Wallis test between
#'   the groups, below the variable name. With several variables, adjust the
#'   p-values for multiple testing (see [p.adjust()] and the `tests` table);
#' - `"missing"`: the number of missing values, next to `n`.
#'
#' `annotate = "all"` adds them all (the test only with `group`). The
#' caption gives their key. With several boxes in a panel, each statistic
#' takes two short lines: for many groups, make the figure wider or choose
#' fewer annotations. All the values are in the `stats` and `tests` tables
#' of `plot = FALSE`.
#'
#' **Publication figures.** Give the units of the variables in their names
#' (such as `"Ca (%)"`, with `check.names = FALSE` in [data.frame()]) or in
#' `ylab`. Make the plot at its printed size, with `base_size` set to the
#' text size of the journal (often 7 to 9 points), and save it with
#' [ggplot2::ggsave()], for example
#' `ggsave("figure.pdf", p, width = 85, height = 70, units = "mm")` for a
#' single column. The caption says which boxplot is drawn, because readers
#' assume Tukey's whiskers (1.5 IQR) otherwise; use `caption = FALSE` and
#' state it in the figure legend instead, for example: "Adjusted boxplots
#' (Hubert and Vandervieren, 2008): the boxes show the quartiles and the
#' median, the whiskers extend to the most extreme values within the
#' medcouple-adjusted fences, and the points are the values outside."
#'
#' @references
#' The adjusted boxplot is based on the methodology described in:
#' - Brys, G., Hubert, M., Struyf, A., (2004). A Robust Measure of Skewness.
#'   Journal of Computational and Graphical Statistics, 13(4):996-1017
#' - Hubert, M., Vandervieren, E., (2008). An adjusted boxplot for skewed distributions.
#'   Computational Statistics and Data Analysis, 52(12):5186-5201
#' - McGill, R., Tukey, J. W., Larsen, W. A. (1978). Variations of box plots.
#'   The American Statistician, 32(1):12-16 (notches)
#'
#' @param x A data frame (or matrix) of numeric variables, with optionally
#'   the columns `id` and `group`.
#' @param plot A logical value indicating whether to plot the boxplot (default
#'   is `TRUE`) or to return its statistics.
#' @param id Optional column of `x` (unquoted or as a string) identifying the
#'   observations, such as sample names: it names the outlying observations
#'   in the `outliers` table and with `label_outliers`.
#' @param group Optional column of `x` (unquoted or as a string) of groups,
#'   such as sites or treatments: the boxes of each group are drawn side by
#'   side, with their own statistics. Rows with a missing group are left out.
#' @param scales `"free_y"` (default), one panel per variable with its own
#'   value axis, or `"fixed"`, a common value axis.
#' @param points The observations drawn: `"outliers"` (default), the
#'   outlying ones; `"all"`; or `"none"`.
#' @param label_outliers A logical: label the outlying observations with
#'   their `id`, else their row number (`FALSE`, default).
#' @param show_n A logical: write the number of values below each box
#'   (`TRUE`, default).
#' @param show_mean A logical: mark the mean of each box with a diamond
#'   (`FALSE`, default).
#' @param annotate Statistics added to each box (none by default): some of
#'   `"shape"`, `"outliers"`, `"fences"`, `"median"`, `"spread"`,
#'   `"location"`, `"test"` and `"missing"`, or `"all"` (see Details).
#' @param horizontal A logical: horizontal boxes (`FALSE`, default), suited
#'   to many variables or long names.
#' @param log A logical: a logarithmic value axis (`FALSE`, default), for
#'   variables spanning orders of magnitude. The statistics are those of the
#'   data, not of their logarithm.
#' @param fill The fill color of the boxes (default `"grey85"`), or one color
#'   per group (or per variable without `group`).
#' @param xlab,ylab The titles of the axis of the variables (or groups) and of
#'   the value axis. Default is none.
#' @param title The plot title. A long title is split into a title and a
#'   subtitle.
#' @param caption `TRUE` (default), a caption naming the boxplot and its
#'   whiskers; `FALSE`, no caption; or a caption of your own.
#' @param base_size The size of the text, in points. Default is 11.
#' @param x_labels_angle The angle (in degrees) of the labels below the
#'   boxes. Default is 0 (horizontal).
#' @param box_width The width of the boxes (default 0.5).
#' @param notch A logical value indicating whether to draw notches (default
#'   is `FALSE`): the notch spans the median \eqn{\pm 1.58\,\text{IQR}/\sqrt{n}}
#'   (McGill et al., 1978); two medians whose notches do not overlap differ
#'   roughly at the 5% level.
#' @param notch_width The width of the notch relative to the box (default
#'   0.5).
#' @param staple_width The width of the staples at the ends of the whiskers,
#'   relative to the box (default 0.5).
#' @param xlabels.angle,xlabels.vjust,xlabels.hjust,box.width,notchwidth,staplewidth
#'   `r lifecycle::badge("deprecated")` Use `x_labels_angle`, `box_width`,
#'   `notch_width` and `staple_width`; the justification of the labels now
#'   follows their angle.
#'
#' @return
#'    - If `plot = TRUE`, a `ggplot2` object.
#'    - If `plot = FALSE`, a list of tibbles: `stats`, with one row per
#'      variable (and group): the numbers of values `n` and of missing values
#'      `n_missing`, the whisker ends `lower` and `upper` (the most extreme
#'      values within the fences), the quartiles `q1` and `q3`, the `median`,
#'      the fences, the notch limits, the `medcouple`, the 95% confidence
#'      interval of the median (`median_lower`, `median_upper`), the `iqr`,
#'      the robust coefficient of variation `rcv` (%), the `biweight`
#'      location, the `mean`, and the numbers of outlying values `n_outliers`
#'      and of expected ones in clean data `expected_outliers`; `outliers`,
#'      with the outlying values, their `row` in `x`, their `id` and their
#'      tail `out` (`"lower"` or `"upper"`); and, with `group`, `tests`, the
#'      Kruskal-Wallis test between the groups of each variable (`statistic`,
#'      `df`, `p_value`).
#'
#' @seealso [generalized_boxplot()] for skewed and heavy-tailed distributions.
#'
#' @export adjusted_boxplot
#'
#' @examples
#' data(forageLIBS)
#' minerals <- forageLIBS[c("Measurement", "Ca", "Mg", "P", "K", "S")]
#'
#' # mineral contents, each on its own axis, the outlying samples named: a
#' # figure of a journal page width
#' p <- adjusted_boxplot(minerals, id = Measurement, label_outliers = TRUE,
#'                       ylab = "Content (%)", base_size = 8)
#' p
#' # ggplot2::ggsave("minerals.pdf", p, width = 175, height = 70, units = "mm")
#'
#' # the statistics and the outlying samples
#' res <- adjusted_boxplot(minerals, id = Measurement, plot = FALSE)
#' res$stats
#' res$outliers
#'
#' # potassium and phosphorus by calcium level, with notches and means: a
#' # figure of a journal column
#' minerals$Ca_level <- cut(minerals$Ca, quantile(minerals$Ca, 0:3 / 3),
#'                          labels = c("Low Ca", "Mid Ca", "High Ca"),
#'                          include.lowest = TRUE)
#' p <- adjusted_boxplot(minerals[c("K", "P", "Ca_level")], group = Ca_level,
#'                       notch = TRUE, show_mean = TRUE, ylab = "Content (%)",
#'                       title = "Potassium and phosphorus by calcium level",
#'                       base_size = 8)
#' p
#' # ggplot2::ggsave("by_calcium.pdf", p, width = 85, height = 75, units = "mm")
#'
#' # trace elements on a logarithmic axis, horizontal, with all the samples
#' traces <- forageLIBS[c("Fe", "Mn", "Zn")]
#' adjusted_boxplot(traces, log = TRUE, horizontal = TRUE, points = "all",
#'                  ylab = "Content (mg/kg)")
adjusted_boxplot <- function(x, plot = TRUE, id = NULL, group = NULL,
                             scales = c("free_y", "fixed"),
                             points = c("outliers", "all", "none"), label_outliers = FALSE,
                             show_n = TRUE, show_mean = FALSE, annotate = NULL,
                             horizontal = FALSE, log = FALSE, fill = "grey85", xlab = NULL,
                             ylab = NULL, title = NULL, caption = TRUE, base_size = 11,
                             x_labels_angle = 0, box_width = 0.5, notch = FALSE,
                             notch_width = 0.5, staple_width = 0.5,
                             xlabels.angle = deprecated(), xlabels.vjust = deprecated(),
                             xlabels.hjust = deprecated(), box.width = deprecated(),
                             notchwidth = deprecated(), staplewidth = deprecated()) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.logical(plot) || length(plot) != 1 || is.na(plot)) {
    stop("Argument 'plot' must be of type boolean (TRUE or FALSE).")
  }
  args <- boxplot_args(
    "adjusted_boxplot", rlang::caller_env(), scales = scales, points = points,
    label_outliers = label_outliers, show_n = show_n, show_mean = show_mean,
    annotate = annotate, horizontal = horizontal, log = log, fill = fill, xlab = xlab,
    ylab = ylab, title = title, caption = caption, base_size = base_size,
    x_labels_angle = x_labels_angle, box_width = box_width, notch = notch,
    notch_width = notch_width, staple_width = staple_width,
    xlabels.angle = xlabels.angle, xlabels.vjust = xlabels.vjust, xlabels.hjust = xlabels.hjust,
    box.width = box.width, notchwidth = notchwidth, staplewidth = staplewidth
  )
  input <- boxplot_input(x, rlang::enquo(id), rlang::enquo(group))
  rate <- 2 * stats::pnorm(-4 * stats::qnorm(0.75))
  res <- robust_boxplot_data(input$x, input$vars, input$id, input$group, adjusted_stats, rate)
  if (!plot) {
    return(res[c("stats", "outliers", if (!is.null(res$tests)) "tests")])
  }
  robust_boxplot_plot(res, args, "adjusted", rate)
}

# Adjusted boxplot statistics of one variable (Hubert and Vandervieren, 2008).
adjusted_stats <- function(v) {
  a <- robustbase::adjboxStats(v, doScale = FALSE)
  tail <- ifelse(v < a$fence[1], "lower", ifelse(v > a$fence[2], "upper", NA_character_))
  stats <- data.frame(
    lower = a$stats[1], q1 = a$stats[2], median = a$stats[3], q3 = a$stats[4],
    upper = a$stats[5], lower_fence = a$fence[1], upper_fence = a$fence[2],
    notch_lower = a$conf[1], notch_upper = a$conf[2], medcouple = medcouple(v)
  )
  list(stats = stats, tail = tail)
}
