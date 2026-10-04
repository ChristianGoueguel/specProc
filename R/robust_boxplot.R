# Internals shared by adjusted_boxplot() and generalized_boxplot(): the
# statistics of each variable (and group), and the plot.

# The boxplot statistics of each variable of `x`, and of each group when
# `group_name` is given. `stats_fun(v)` returns, for the non-missing values
# `v` of one variable (and group), a list: `stats`, a one-row data frame
# with lower, q1, median, q3, upper, lower_fence, upper_fence, notch_lower,
# notch_upper and the statistics of the method; and `tail`, "lower" or
# "upper" for the values outside the fences, NA for the others. `rate` is
# the expected proportion of outlying values in clean data. Returns `stats`,
# `outliers`, `data` (all values, with their tail) and, with groups,
# `tests` (Kruskal-Wallis tests between the groups of each variable).
robust_boxplot_data <- function(x, vars, id_name, group_name, stats_fun, rate) {
  group <- if (is.null(group_name)) {
    NULL
  } else {
    g <- x[[group_name]]
    if (is.factor(g)) droplevels(g) else factor(g)
  }
  levels <- if (is.null(group)) list(NULL) else as.list(levels(group))
  stats <- list()
  data <- list()
  for (variable in vars) {
    for (level in levels) {
      in_group <- if (is.null(level)) rep(TRUE, nrow(x)) else group %in% level
      rows <- which(!is.na(x[[variable]]) & in_group)
      if (length(rows) == 0) next
      v <- x[[variable]][rows]
      res <- stats_fun(v)
      key <- data.frame(variable = variable, stringsAsFactors = FALSE)
      if (!is.null(level)) key$group <- level
      stats[[length(stats) + 1]] <- cbind(
        key, n = length(v), n_missing = sum(is.na(x[[variable]]) & in_group), res$stats,
        boxplot_summaries(v, res$stats), mean = mean(v), n_outliers = sum(!is.na(res$tail)),
        expected_outliers = rate * length(v)
      )
      values <- data.frame(variable = rep(variable, length(v)), stringsAsFactors = FALSE)
      if (!is.null(level)) values$group <- level
      values$row <- rows
      if (!is.null(id_name)) values$id <- x[[id_name]][rows]
      values$value <- v
      values$out <- res$tail
      data[[length(data) + 1]] <- values
    }
  }
  if (length(stats) == 0) {
    stop("No non-missing values in 'x'.", call. = FALSE)
  }
  stats <- tibble::as_tibble(do.call(rbind, stats))
  data <- tibble::as_tibble(do.call(rbind, data))
  stats$variable <- factor(stats$variable, levels = vars)
  data$variable <- factor(data$variable, levels = vars)
  if (!is.null(group)) {
    stats$group <- factor(stats$group, levels = levels(group))
    data$group <- factor(data$group, levels = levels(group))
  }
  outliers <- data[!is.na(data$out), ]
  tests <- NULL
  if (!is.null(group)) {
    tests <- lapply(vars, function(variable) {
      d <- data[data$variable == variable, ]
      if (length(unique(d$group)) < 2) return(NULL)
      kw <- stats::kruskal.test(d$value, droplevels(d$group))
      data.frame(variable = variable, statistic = unname(kw$statistic),
                 df = unname(kw$parameter), p_value = kw$p.value)
    })
    tests <- tibble::as_tibble(do.call(rbind, tests))
    if (nrow(tests) > 0) tests$variable <- factor(tests$variable, levels = vars)
  }
  list(stats = stats, outliers = outliers, data = data, tests = tests)
}

# Robust summaries of the values `v` of a box: the distribution-free 95%
# confidence interval of the median (from the order statistics, unlike the
# notch, which compares two medians), the interquartile range of the box,
# the robust coefficient of variation (IQR / 1.349 over the median, in
# percent) and the biweight location.
boxplot_summaries <- function(v, stats) {
  n <- length(v)
  j <- stats::qbinom(0.025, n, 0.5)
  ci <- if (j >= 1) sort(v)[c(j, n - j + 1)] else c(NA_real_, NA_real_)
  iqr <- stats$q3 - stats$q1
  rcv <- if (stats$median != 0) {
    100 * iqr / diff(stats::qnorm(c(0.25, 0.75))) / abs(stats$median)
  } else {
    NA_real_
  }
  biweight <- tryCatch(biweight_location(v), error = function(e) NA_real_)
  data.frame(median_lower = ci[1], median_upper = ci[2], iqr = iqr, rcv = rcv,
             biweight = biweight)
}

boxplot_annotations <- c("shape", "outliers", "fences", "median", "spread", "location", "test",
                         "missing")

# The column name selected by a quosure (unquoted name or string), or NULL.
boxplot_column <- function(quo, x, arg) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  name <- rlang::as_name(quo)
  if (!name %in% names(x)) {
    stop("The '", arg, "' column does not exist in 'x'.", call. = FALSE)
  }
  name
}

# The data of the boxplot: `x` as a data frame, its numeric variables, and
# the `id` and `group` columns.
boxplot_input <- function(x, id_quo, group_quo) {
  if (is.matrix(x)) {
    x <- as.data.frame(x)
  }
  if (!is.data.frame(x)) {
    stop("Input 'x' must be a numeric data frame.")
  }
  id <- boxplot_column(id_quo, x, "id")
  group <- boxplot_column(group_quo, x, "group")
  vars <- setdiff(names(x), c(id, group))
  if (length(vars) == 0 || !all(vapply(x[vars], is.numeric, logical(1)))) {
    stop("Input 'x' must be a numeric data frame.")
  }
  list(x = x, vars = vars, id = id, group = group)
}

# The plot options, with the renamed arguments resolved and checked.
boxplot_args <- function(fn, user_env, scales, points, label_outliers, show_n, show_mean,
                         annotate, horizontal, log, fill, xlab, ylab, title, caption, base_size,
                         x_labels_angle, box_width, notch, notch_width, staple_width,
                         xlabels.angle, xlabels.vjust, xlabels.hjust, box.width, notchwidth,
                         staplewidth) {
  renamed <- function(old_value, new_value, old, new, details = NULL) {
    if (!lifecycle::is_present(old_value)) {
      return(new_value)
    }
    lifecycle::deprecate_warn("0.9.0", sprintf("%s(%s)", fn, old), sprintf("%s(%s)", fn, new),
                              details = details, user_env = user_env)
    old_value
  }
  name <- function(old_value, old, new) if (lifecycle::is_present(old_value)) old else new
  justification <- "The justification of the labels now follows their angle."
  args <- list(
    scales = match.arg(scales, c("free_y", "fixed")),
    points = match.arg(points, c("outliers", "all", "none")),
    label_outliers = label_outliers, show_n = show_n, show_mean = show_mean,
    annotate = if (identical(annotate, "all")) boxplot_annotations else annotate,
    annotate_all = identical(annotate, "all"),
    horizontal = horizontal, log = log, fill = fill, xlab = xlab, ylab = ylab, title = title,
    caption = caption, base_size = base_size, notch = notch,
    x_labels_angle = renamed(xlabels.angle, x_labels_angle, "xlabels.angle", "x_labels_angle"),
    x_labels_hjust = renamed(xlabels.hjust, NULL, "xlabels.hjust", "x_labels_angle", justification),
    x_labels_vjust = renamed(xlabels.vjust, NULL, "xlabels.vjust", "x_labels_angle", justification),
    box_width = renamed(box.width, box_width, "box.width", "box_width"),
    notch_width = renamed(notchwidth, notch_width, "notchwidth", "notch_width"),
    staple_width = renamed(staplewidth, staple_width, "staplewidth", "staple_width"),
    angle_arg = name(xlabels.angle, "xlabels.angle", "x_labels_angle"),
    width_arg = name(box.width, "box.width", "box_width"),
    notch_arg = name(notchwidth, "notchwidth", "notch_width"),
    staple_arg = name(staplewidth, "staplewidth", "staple_width")
  )
  check_boxplot_args(args)
}

check_boxplot_args <- function(args) {
  if (!is.logical(args$notch) || length(args$notch) != 1 || is.na(args$notch)) {
    stop("Argument 'notch' must be of type boolean (TRUE or FALSE).", call. = FALSE)
  }
  angle <- args$x_labels_angle
  if (!is.numeric(angle) || length(angle) != 1 || is.na(angle) || angle < 0 || angle > 360) {
    stop("Argument '", args$angle_arg, "' must be a numeric value between 0 and 360.",
         call. = FALSE)
  }
  for (just in c("hjust", "vjust")) {
    value <- args[[paste0("x_labels_", just)]]
    if (!is.null(value) && (!is.numeric(value) || length(value) != 1 || is.na(value) ||
                            value < 0 || value > 1)) {
      stop("Argument 'xlabels.", just, "' must be a numeric value between 0 and 1.", call. = FALSE)
    }
  }
  positive <- function(value) is.numeric(value) && length(value) == 1 && !is.na(value)
  if (!positive(args$box_width) || args$box_width <= 0) {
    stop("Argument '", args$width_arg, "' must be a positive numeric value.", call. = FALSE)
  }
  if (!positive(args$notch_width) || args$notch_width < 0 || args$notch_width > 1) {
    stop("Argument '", args$notch_arg, "' must be a numeric value between 0 and 1.", call. = FALSE)
  }
  if (!positive(args$staple_width) || args$staple_width < 0) {
    stop("Argument '", args$staple_arg, "' must be a positive numeric value.", call. = FALSE)
  }
  for (flag in c("label_outliers", "show_n", "show_mean", "horizontal", "log")) {
    check_flag(args[[flag]], flag)
  }
  check_number(args$base_size, "base_size", lower = 0, lower_open = TRUE)
  fill_ok <- is.character(args$fill) && length(args$fill) >= 1 && !anyNA(args$fill) &&
    all(vapply(args$fill, function(col) !inherits(try(grDevices::col2rgb(col), silent = TRUE),
                                                  "try-error"), logical(1)))
  if (!fill_ok) {
    stop("'fill' must be one or several colors.", call. = FALSE)
  }
  annotate <- args$annotate
  if (!is.null(annotate) && (!is.character(annotate) || !all(annotate %in% boxplot_annotations))) {
    stop("'annotate' must be \"all\" or some of ",
         paste0("\"", boxplot_annotations, "\"", collapse = ", "), ".", call. = FALSE)
  }
  caption <- args$caption
  if (!(isTRUE(caption) || isFALSE(caption) ||
        (is.character(caption) && length(caption) == 1 && !is.na(caption)))) {
    stop("'caption' must be TRUE, FALSE or a character string.", call. = FALSE)
  }
  invisible(args)
}

# The boxplot of the statistics `res` (from robust_boxplot_data()), with the
# options `args`; `method` is "adjusted" or "generalized", and `alpha` the
# expected proportion of outlying values in clean data.
robust_boxplot_plot <- function(res, args, method, alpha) {
  st <- res$stats
  data <- res$data
  ann <- args$annotate
  tested <- "test" %in% ann && !is.null(res$tests) && nrow(res$tests) > 0
  if ("test" %in% ann && is.null(res$tests) && !args$annotate_all) {
    warning("annotate = \"test\" needs 'group': no test is shown.", call. = FALSE)
  }
  grouped <- "group" %in% names(st)
  n_vars <- nlevels(st$variable)
  panels <- grouped || (args$scales == "free_y" && n_vars > 1)
  if (args$log && any(data$value <= 0)) {
    stop("'log = TRUE' needs positive values.", call. = FALSE)
  }

  # the position of the boxes: the groups within the panel of each variable,
  # else the variables (one panel), else one box per panel; with the number
  # of values below each position
  base <- if (grouped) as.character(st$group) else if (panels) "" else as.character(st$variable)
  missing <- if ("missing" %in% ann) {
    ifelse(st$n_missing > 0, paste0(" (", st$n_missing, " missing)"), "")
  } else {
    ""
  }
  label <- if (args$show_n) {
    trimws(paste0(base, ifelse(nzchar(base), "\n", ""), "n = ", st$n, missing))
  } else {
    base
  }
  order <- if (grouped) order(st$group, st$variable) else order(st$variable)
  st$.x <- factor(label, levels = unique(label[order]))
  st$.key <- if (grouped) st$group else st$variable
  keys <- c("variable", if (grouped) "group")
  position <- st[c(keys, ".x", ".key")]
  data <- merge(data, position, by = keys, sort = FALSE)
  outliers <- data[!is.na(data$out), , drop = FALSE]

  lower <- q1 <- median <- q3 <- upper <- notch_lower <- notch_upper <- .x <- .key <- value <-
    NULL
  # vertical boxes, or horizontal ones (flipped aesthetics, which unlike
  # coord_flip() allow free scales in the panels)
  width <- 0.12 * args$box_width
  if (args$horizontal) {
    box_aes <- ggplot2::aes(y = .x, xmin = lower, xlower = q1, xmiddle = median, xupper = q3,
                            xmax = upper, notchlower = notch_lower, notchupper = notch_upper,
                            group = .x, fill = .key)
    point_aes <- ggplot2::aes(x = value, y = .x)
    mean_aes <- ggplot2::aes(x = .data$mean, y = .x)
    jitter <- ggplot2::position_jitter(width = 0, height = width, seed = 1)
  } else {
    box_aes <- ggplot2::aes(x = .x, ymin = lower, lower = q1, middle = median, upper = q3,
                            ymax = upper, notchlower = notch_lower, notchupper = notch_upper,
                            group = .x, fill = .key)
    point_aes <- ggplot2::aes(x = .x, y = value)
    mean_aes <- ggplot2::aes(x = .x, y = .data$mean)
    jitter <- ggplot2::position_jitter(width = width, height = 0, seed = 1)
  }
  point_size <- 1.6 * args$base_size / 11
  p <- ggplot2::ggplot()
  if (args$points == "all") {
    p <- p + ggplot2::geom_point(data = data[is.na(data$out), , drop = FALSE], point_aes,
                                 position = jitter, colour = "grey55", size = 0.7 * point_size,
                                 alpha = 0.5)
  }
  p <- p +
    ggplot2::geom_boxplot(
      data = st, box_aes, stat = StatBoxplotIdentity,
      orientation = if (args$horizontal) "y" else "x",
      width = args$box_width, colour = "grey15", alpha = if (args$points == "all") 0.6 else 1,
      linewidth = args$base_size / 22, notch = args$notch, notchwidth = args$notch_width,
      staplewidth = args$staple_width
    ) +
    ggplot2::scale_fill_manual(values = rep_len(args$fill, nlevels(st$.key)), guide = "none")
  if (args$points != "none" && nrow(outliers) > 0) {
    p <- p + ggplot2::geom_point(data = outliers, point_aes, position = jitter, shape = 21,
                                 fill = "grey15", colour = "grey15", size = point_size,
                                 alpha = 0.75)
    if (args$label_outliers) {
      # labels alternately on either side of the points, in the order of their
      # values in each tail, so that close values keep apart
      outliers$.label <- as.character(if ("id" %in% names(outliers)) outliers$id else outliers$row)
      tails <- interaction(outliers$.x, outliers$variable, outliers$out, drop = TRUE)
      rank <- stats::ave(outliers$value, tails, FUN = function(v) rank(v, ties.method = "first"))
      hjust <- ifelse(rank %% 2 == 1, -0.25, 1.25)
      p <- p + ggplot2::geom_text(data = outliers, point_aes,
                                  label = outliers$.label, position = jitter, hjust = hjust,
                                  colour = "grey25", size = 0.7 * args$base_size / ggplot2::.pt)
    }
  }
  if (args$show_mean) {
    p <- p + ggplot2::geom_point(data = st, mean_aes, shape = 23, fill = "white",
                                 colour = "grey15", size = point_size)
  }
  if ("location" %in% ann) {
    location_aes <- if (args$horizontal) {
      ggplot2::aes(x = .data$biweight, y = .x)
    } else {
      ggplot2::aes(x = .x, y = .data$biweight)
    }
    p <- p + ggplot2::geom_point(data = st, location_aes, shape = 4, stroke = 0.9,
                                 colour = "grey10", size = point_size)
  }
  if ("fences" %in% ann) {
    # the fences beyond which there are outlying values
    cell <- function(d) paste(d$variable, if (grouped) d$group)
    low <- st[cell(st) %in% cell(outliers[outliers$out == "lower", ]), ]
    up <- st[cell(st) %in% cell(outliers[outliers$out == "upper", ]), ]
    low$.fence <- low$lower_fence
    up$.fence <- up$upper_fence
    fences <- rbind(low, up)
    if (nrow(fences) > 0) {
      fence_aes <- if (args$horizontal) {
        ggplot2::aes(y = .x, xmin = .data$.fence, xmax = .data$.fence)
      } else {
        ggplot2::aes(x = .x, ymin = .data$.fence, ymax = .data$.fence)
      }
      p <- p + ggplot2::geom_errorbar(data = fences, fence_aes,
                                      orientation = if (args$horizontal) "y" else "x",
                                      width = 0.8 * args$box_width, linetype = "22",
                                      colour = "grey35", linewidth = args$base_size / 30)
    }
  }
  # the statistics written above each box (at the right of horizontal ones)
  several <- grouped || (!panels && n_vars > 1)
  text <- boxplot_text(st, ann, method, compact = several)
  room <- 0.05
  if (!is.null(text)) {
    st$.text <- text
    n_lines <- length(strsplit(text[1], "\n", fixed = TRUE)[[1]])
    text_size <- 0.62 * args$base_size / ggplot2::.pt
    p <- p + if (args$horizontal) {
      ggplot2::geom_text(data = st, ggplot2::aes(x = Inf, y = .x, label = .data$.text),
                         hjust = 1.02, vjust = 0.5, size = text_size, lineheight = 0.95,
                         colour = "grey25")
    } else {
      ggplot2::geom_text(data = st, ggplot2::aes(x = .x, y = Inf, label = .data$.text),
                         vjust = 1.1, size = text_size, lineheight = 0.95, colour = "grey25")
    }
    widest <- max(nchar(unlist(strsplit(text, "\n", fixed = TRUE))))
    room <- if (args$horizontal) 0.05 + 0.009 * widest else 0.05 + 0.065 * n_lines
  }
  value_scale <- if (args$horizontal) {
    if (args$log) ggplot2::scale_x_log10 else ggplot2::scale_x_continuous
  } else {
    if (args$log) ggplot2::scale_y_log10 else ggplot2::scale_y_continuous
  }
  p <- p + value_scale(labels = plain_numbers, expand = ggplot2::expansion(mult = c(0.05, room)))

  if (panels) {
    # free value axes, or the same for all panels; the positions are always
    # free, each panel having its own boxes
    facet_scales <- if (args$scales == "free_y") {
      "free"
    } else if (args$horizontal) {
      "free_y"
    } else {
      "free_x"
    }
    # the Kruskal-Wallis test between the groups, below the variable name
    strips <- stats::setNames(levels(st$variable), levels(st$variable))
    if (tested) {
      tested_vars <- as.character(res$tests$variable)
      strips[tested_vars] <- paste0(strips[tested_vars], "\nKruskal-Wallis p ",
                                    format_p(res$tests$p_value))
    }
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(.data$variable), scales = facet_scales,
      nrow = if (!args$horizontal && n_vars <= 6) 1,
      ncol = if (args$horizontal && n_vars <= 6) 1,
      labeller = ggplot2::as_labeller(strips)
    )
  }
  caption_text <- if (isTRUE(args$caption)) {
    boxplot_caption(method, alpha, setdiff(ann, if (!tested) "test"), args$show_mean)
  } else if (is.character(args$caption)) {
    args$caption
  }
  p <- p +
    ggplot2::labs(x = if (args$horizontal) args$ylab else args$xlab,
                  y = if (args$horizontal) args$xlab else args$ylab,
                  title = args$title, caption = caption_text) +
    ggplot2::theme_classic(base_size = args$base_size) +
    ggplot2::theme(
      legend.position = "none",
      axis.line = ggplot2::element_line(colour = "#4b4b4b", linewidth = args$base_size / 16),
      axis.ticks = ggplot2::element_line(colour = "#4b4b4b"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30", size = ggplot2::rel(0.8)),
      plot.caption.position = "plot"
    )
  if (!args$horizontal) {
    angle <- args$x_labels_angle
    hjust <- args$x_labels_hjust %||% (if (angle == 0) 0.5 else 1)
    vjust <- args$x_labels_vjust %||% (if (angle == 0) 1 else if (angle == 90) 0.5 else 1)
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust,
                                                                vjust = vjust))
  }
  if (panels && !grouped && !args$show_n) {
    # one box per panel, named by the strip: no position labels needed
    p <- p + if (args$horizontal) {
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
    } else {
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
    }
  }
  finish_title(p)
}

# The caption of a method: what the whiskers show, and the key of the
# annotations and marks drawn.
boxplot_caption <- function(method, alpha = NULL, annotate = NULL, show_mean = FALSE) {
  text <- if (method == "adjusted") {
    paste("Adjusted boxplot (Hubert and Vandervieren, 2008). Whiskers: the most extreme",
          "values within the medcouple-adjusted fences.")
  } else {
    paste0("Generalized boxplot (Bruffaerts et al., 2014). Whiskers: the most extreme values ",
           "within the fences of a fitted Tukey g-and-h distribution (alpha = ",
           format(signif(100 * alpha, 2)), "%).")
  }
  key <- c(
    shape = if (method == "adjusted") "MC: medcouple." else
      "g, h: skewness and tail heaviness of the g-and-h fit.",
    outliers = paste0("Flagged (exp.): outlying values (expected in clean data, ",
                      format(signif(100 * alpha, 2)), "%)."),
    fences = "Dashed: fences.",
    median = "Md: median [95% CI].",
    spread = "rCV: IQR / (1.349 median).",
    location = "x: biweight location.",
    test = "Kruskal-Wallis test between the groups."
  )
  key <- c(if (show_mean) "Diamond: mean.", key[intersect(names(key), annotate)])
  # short lines, which fit a figure of a journal column
  paste(strwrap(paste(c(text, key), collapse = " "), width = 60), collapse = "\n")
}

# The statistics written with each box: one line per annotation (two short
# ones when `compact`, for boxes side by side), or NULL.
boxplot_text <- function(st, annotate, method, compact = FALSE) {
  sep <- if (compact) "\n" else " "
  two <- function(v) sprintf("%.2f", round(v, 2) + 0) # no "-0.00"
  number <- function(v) {
    ifelse(is.na(v), "NA", trimws(formatC(signif(v, 3), digits = 3, format = "fg", big.mark = ",")))
  }
  lines <- list()
  if ("shape" %in% annotate) {
    lines$shape <- if (method == "adjusted") {
      paste0("MC = ", two(st$medcouple), ifelse(abs(st$medcouple) > 0.6, " (> 0.6)", ""))
    } else {
      paste0("g = ", two(st$g), if (compact) "\n" else ", ", "h = ", two(st$h))
    }
  }
  if ("outliers" %in% annotate) {
    lines$outliers <- paste0(st$n_outliers, " flagged", sep,
                             sprintf("(%.1f exp.)", st$expected_outliers))
  }
  if ("median" %in% annotate) {
    lines$median <- paste0("Md ", number(st$median), sep, "[", number(st$median_lower), ", ",
                           number(st$median_upper), "]")
  }
  if ("spread" %in% annotate) {
    lines$spread <- paste0("IQR ", number(st$iqr), if (compact) "\n" else ", ", "rCV ",
                           ifelse(is.na(st$rcv), "NA", sprintf("%.0f%%", st$rcv)))
  }
  if (length(lines) == 0) {
    return(NULL)
  }
  do.call(paste, c(unname(lines), sep = "\n"))
}

format_p <- function(p) {
  vapply(p, function(v) {
    if (v < 0.001) "< 0.001" else paste("=", format(signif(v, 2), scientific = FALSE))
  }, character(1))
}

# The identity stat, accepting the notch limits of precomputed statistics
# (stat_boxplot() computes them otherwise).
StatBoxplotIdentity <- ggplot2::ggproto("StatBoxplotIdentity", ggplot2::StatIdentity,
  optional_aes = c("notchlower", "notchupper")
)
