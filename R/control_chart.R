#' @title Control Chart for Instrument Drift
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Builds a control chart of a quantity that should stay constant over time,
#' such as the intensity of an internal standard line, a line ratio of a
#' check sample or a plasma temperature, to detect drifts and shifts of the
#' instrument: a Shewhart chart of individual values, an exponentially
#' weighted moving average (EWMA) or a cumulative sum (CUSUM).
#'
#' @details
#' The center line and the standard deviation \eqn{\sigma} are estimated
#' from the `baseline` measurements (by default, all of them), unless given:
#' the mean, and the mean moving range of consecutive values divided by
#' 1.128, which, unlike the standard deviation, is little inflated by a
#' drift.
#'  - `"shewhart"`: the values themselves, with limits at `width`
#'    \eqn{\sigma} from the center. With `rules = TRUE`, the Western
#'    Electric rules also flag (1) a value beyond the limits, (2) two of
#'    three consecutive values beyond \eqn{2\sigma} on the same side, (3)
#'    four of five beyond \eqn{1\sigma} on the same side and (4) eight
#'    consecutive values on the same side of the center. It detects large
#'    shifts quickly.
#'  - `"ewma"`: \eqn{z_t = \lambda x_t + (1 - \lambda) z_{t-1}}, starting at
#'    the center, with limits at `width`
#'    \eqn{\sigma \sqrt{\lambda / (2 - \lambda) (1 - (1 - \lambda)^{2t})}}.
#'    Small values of \eqn{\lambda} detect small, slow drifts.
#'  - `"cusum"`: the tabular CUSUM, the sums of the deviations beyond
#'    `allowance` \eqn{\sigma} above and below the center,
#'    \eqn{C^+_t = \max(0, x_t - \mu - k\sigma + C^+_{t-1})} and likewise
#'    below, with a signal when a sum exceeds `decision` \eqn{\sigma}. It
#'    detects small persistent shifts.
#'
#' @param x A numeric vector of the monitored quantity.
#' @param order The time or acquisition order of the values (numbers, dates
#'   or date-times), by which they are sorted. By default, their position.
#' @param method `"shewhart"` (default), `"ewma"` or `"cusum"`.
#' @param baseline The measurements that set the center and the standard
#'   deviation: the number of first values, or a logical or index vector.
#'   By default, all values.
#' @param center,sigma The center and standard deviation of the
#'   process, if known.
#' @param width The width of the limits, in standard deviations, for the
#'   Shewhart and EWMA charts. Default is 3.
#' @param rules A logical: apply the Western Electric rules to the Shewhart
#'   chart (`TRUE`, default).
#' @param lambda The smoothing constant of the EWMA chart. Default is 0.2.
#' @param allowance,decision The allowance \eqn{k} and decision interval
#'   \eqn{h} of the CUSUM chart, in standard deviations. Default is 0.5 and
#'   5.
#'
#' @return A tibble of class `specproc_control_chart`, one row per value in
#'   time order: `order`, `value`, the plotted `statistic` (the value, the
#'   EWMA, or for the CUSUM the upper sum, with the lower sum in
#'   `lower_sum`), the `center`, the `lower` and `upper` limits, whether the
#'   point is a `signal`, and, for the Shewhart chart, the first Western
#'   Electric `rule` broken. The method, center, standard deviation and
#'   baseline are in the attributes. Draw it with [plot_control_chart()].
#'
#' @references
#'  - Montgomery, D.C. (2019). Introduction to Statistical Quality Control,
#'    8th ed. Wiley, Hoboken.
#'  - Western Electric Company (1956). Statistical Quality Control Handbook.
#'    Western Electric, Indianapolis.
#'
#' @seealso [plot_control_chart()], [line_intensities()]
#' @export control_chart
#'
#' @examples
#' # a check-sample ratio that drifts after the 60th measurement
#' set.seed(1)
#' ratio <- 1 + c(rep(0, 60), seq(0, 0.06, length.out = 40)) + rnorm(100, sd = 0.02)
#' shewhart <- control_chart(ratio, baseline = 40)
#' shewhart[shewhart$signal, ]
#' plot_control_chart(shewhart)
#'
#' # the EWMA detects the slow drift earlier
#' plot_control_chart(control_chart(ratio, method = "ewma", baseline = 40))
control_chart <- function(x, order = NULL, method = "shewhart", baseline = NULL, center = NULL,
                          sigma = NULL, width = 3, rules = TRUE, lambda = 0.2, allowance = 0.5,
                          decision = 5) {
  method <- match.arg(method, c("shewhart", "ewma", "cusum"))
  if (!is.numeric(x) || length(x) < 2 || anyNA(x)) {
    stop("'x' must be a numeric vector of at least 2 values, without missing values.",
         call. = FALSE)
  }
  n <- length(x)
  if (is.null(order)) order <- seq_len(n)
  if (length(order) != n || anyNA(order)) {
    stop("'order' must give one time or position per value.", call. = FALSE)
  }
  check_number(width, "width", lower = 0, lower_open = TRUE)
  check_flag(rules, "rules")
  check_number(lambda, "lambda", lower = 0, upper = 1, lower_open = TRUE)
  check_number(allowance, "allowance", lower = 0)
  check_number(decision, "decision", lower = 0, lower_open = TRUE)
  sorted <- base::order(order)
  x <- x[sorted]
  order <- order[sorted]
  in_baseline <- control_baseline(baseline, n, sorted)
  if (sum(in_baseline) < 2) {
    stop("The baseline needs at least 2 values.", call. = FALSE)
  }
  base <- x[in_baseline]
  if (is.null(center)) center <- mean(base)
  if (is.null(sigma)) {
    # mean moving range of consecutive baseline values / d2 (1.128)
    sigma <- mean(abs(diff(base))) / 1.128
  }
  check_number(center, "center")
  check_number(sigma, "sigma", lower = 0, lower_open = TRUE)

  out <- tibble::tibble(order = order, value = x)
  if (method == "shewhart") {
    out$statistic <- x
    out$center <- center
    out$lower <- center - width * sigma
    out$upper <- center + width * sigma
    rule <- ifelse(x < out$lower | x > out$upper, 1L, NA_integer_)
    if (rules) {
      z <- (x - center) / sigma
      rule <- western_electric(z, rule)
    }
    out$signal <- !is.na(rule)
    out$rule <- rule
  } else if (method == "ewma") {
    z <- stats::filter(lambda * x, 1 - lambda, method = "recursive", init = center)
    t <- seq_len(n)
    spread <- width * sigma * sqrt(lambda / (2 - lambda) * (1 - (1 - lambda)^(2 * t)))
    out$statistic <- as.numeric(z)
    out$center <- center
    out$lower <- center - spread
    out$upper <- center + spread
    out$signal <- out$statistic < out$lower | out$statistic > out$upper
  } else {
    upper_sum <- lower_sum <- numeric(n)
    k <- allowance * sigma
    for (i in seq_len(n)) {
      previous <- if (i == 1) c(0, 0) else c(upper_sum[i - 1], lower_sum[i - 1])
      upper_sum[i] <- max(0, x[i] - center - k + previous[1])
      lower_sum[i] <- max(0, center - k - x[i] + previous[2])
    }
    out$statistic <- upper_sum
    out$lower_sum <- lower_sum
    out$center <- 0
    out$lower <- -decision * sigma
    out$upper <- decision * sigma
    out$signal <- upper_sum > decision * sigma | lower_sum > decision * sigma
  }
  structure(out, class = c("specproc_control_chart", class(out)), method = method,
            center = center, sigma = sigma, baseline = which(in_baseline), n = n,
            settings = list(width = width, lambda = lambda, allowance = allowance,
                            decision = decision))
}

#' @title Plot a Control Chart
#'
#' @description
#' Draws a control chart returned by [control_chart()]: the monitored
#' statistic in time order, the center line, the control limits and the
#' signals, with the end of the baseline marked.
#'
#' @param object The result of [control_chart()].
#' @param title The plot title. By default, the method and the number of
#'   signals.
#' @param xlab,ylab The axis labels.
#'
#' @return A ggplot object.
#' @seealso [control_chart()]
#' @export plot_control_chart
#'
#' @examples
#' set.seed(1)
#' ratio <- 1 + c(rep(0, 60), seq(0, 0.06, length.out = 40)) + rnorm(100, sd = 0.02)
#' plot_control_chart(control_chart(ratio, method = "cusum", baseline = 40))
plot_control_chart <- function(object, title = NULL, xlab = "Order", ylab = NULL) {
  if (!inherits(object, "specproc_control_chart")) {
    stop("'object' must be returned by control_chart().", call. = FALSE)
  }
  method <- attr(object, "method")
  d <- as.data.frame(object)
  label <- c(shewhart = "Shewhart chart", ewma = "EWMA chart", cusum = "CUSUM chart")[[method]]
  if (is.null(title)) {
    title <- sprintf("%s: %d signal%s of %d values", label, sum(d$signal),
                     if (sum(d$signal) == 1) "" else "s", nrow(d))
  }
  if (is.null(ylab)) {
    ylab <- c(shewhart = "Value", ewma = "EWMA", cusum = "Cumulative sum")[[method]]
  }
  baseline_end <- d$order[max(attr(object, "baseline"))]
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$order))
  if (max(attr(object, "baseline")) < nrow(d)) {
    p <- p + ggplot2::geom_vline(xintercept = baseline_end, colour = "grey60", linetype = "dotted")
  }
  p <- p +
    ggplot2::geom_line(ggplot2::aes(y = .data$center), colour = "grey40") +
    ggplot2::geom_line(ggplot2::aes(y = .data$lower), colour = "#c0392b", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$upper), colour = "#c0392b", linetype = "dashed")
  # each point colored by its own limit (the two sums of the CUSUM separately)
  d$beyond <- d$statistic < d$lower | d$statistic > d$upper
  if (method == "shewhart") d$beyond <- d$signal
  if (method == "cusum") {
    d$lower_beyond <- -d$lower_sum < d$lower
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = -.data$lower_sum), colour = "#1f4e79", alpha = 0.6) +
      ggplot2::geom_point(data = d, ggplot2::aes(y = -.data$lower_sum, colour = .data$lower_beyond),
                          size = 1.3)
  }
  p <- p +
    ggplot2::geom_line(ggplot2::aes(y = .data$statistic), colour = "#1f4e79", alpha = 0.6) +
    ggplot2::geom_point(data = d, ggplot2::aes(y = .data$statistic, colour = .data$beyond),
                        size = 1.3) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "#1f4e79", `TRUE` = "#c0392b"),
                                 labels = c(`FALSE` = "in control", `TRUE` = "signal"),
                                 name = NULL) +
    ggplot2::labs(x = xlab, y = ylab, title = title,
                  subtitle = if (method == "cusum") "Upper sum above zero, lower sum below") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  finish_title(p)
}

#' @export
print.specproc_control_chart <- function(x, ...) {
  method <- attr(x, "method")
  if (is.null(method) || !identical(attr(x, "n"), nrow(x))) {   # a subset of the chart
    print(tibble::as_tibble(unclass_tibble(x)), ...)
    return(invisible(x))
  }
  cat(c(shewhart = "Shewhart", ewma = "EWMA", cusum = "CUSUM")[[method]], " control chart (",
      nrow(x), " values, baseline of ", length(attr(x, "baseline")), ")\n", sep = "")
  cat("Center: ", format(signif(attr(x, "center"), 5)), ", sigma: ",
      format(signif(attr(x, "sigma"), 4)), "\n", sep = "")
  signals <- which(x$signal)
  cat("Signals: ", length(signals), if (length(signals) > 0)
    paste0(" (first at ", format(x$order[signals[1]]), ")"), "\n\n", sep = "")
  print(tibble::as_tibble(unclass_tibble(x)), ...)
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

unclass_tibble <- function(x) {
  class(x) <- setdiff(class(x), "specproc_control_chart")
  attr(x, "method") <- attr(x, "center") <- attr(x, "sigma") <- attr(x, "baseline") <- NULL
  attr(x, "settings") <- attr(x, "n") <- NULL
  x
}

# The baseline as a logical vector in time order.
control_baseline <- function(baseline, n, sorted) {
  if (is.null(baseline)) return(rep(TRUE, n))
  if (is.logical(baseline)) {
    if (length(baseline) != n || anyNA(baseline)) {
      stop("A logical 'baseline' needs one value per measurement.", call. = FALSE)
    }
    return(baseline[sorted])
  }
  if (!is.numeric(baseline) || anyNA(baseline)) {
    stop("'baseline' must be a number of first values, or a logical or index vector.",
         call. = FALSE)
  }
  if (length(baseline) == 1) {
    check_count(baseline, "baseline", lower = 2)
    return(seq_len(n) <= baseline)
  }
  if (any(baseline < 1 | baseline > n | baseline %% 1 != 0)) {
    stop("'baseline' indices must lie between 1 and ", n, ".", call. = FALSE)
  }
  (seq_len(n) %in% baseline)[sorted]
}

# The first Western Electric rule broken at each point (standardized
# values z), from rule 1 (already in `rule`).
western_electric <- function(z, rule) {
  n <- length(z)
  side <- sign(z)
  count_beyond <- function(i, m, limit) {
    idx <- max(1, i - m + 1):i
    c(sum(z[idx] > limit), sum(z[idx] < -limit))
  }
  for (i in seq_len(n)) {
    if (!is.na(rule[i])) next
    if (i >= 3) {
      beyond <- count_beyond(i, 3, 2)
      if ((beyond[1] >= 2 && z[i] > 2) || (beyond[2] >= 2 && z[i] < -2)) {
        rule[i] <- 2L
        next
      }
    }
    if (i >= 5) {
      beyond <- count_beyond(i, 5, 1)
      if ((beyond[1] >= 4 && z[i] > 1) || (beyond[2] >= 4 && z[i] < -1)) {
        rule[i] <- 3L
        next
      }
    }
    if (i >= 8 && side[i] != 0 && all(side[(i - 7):i] == side[i])) {
      rule[i] <- 4L
    }
  }
  rule
}
