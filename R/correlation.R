#' @title Correlation Coefficients: Pearson, Spearman, Kendall, Chatterjee, and Biweight Midcorrelation
#'
#' @description
#' Computes various correlation coefficients between one or several response
#' variables and each of the remaining variables in a given data frame or tibble. The available correlation
#' methods are Pearson's product-moment correlation (parametric), Spearman's rank correlation,
#' Kendall's tau correlation (non-parametric), Chatterjee's new correlation coefficient, and
#' the biweight midcorrelation (a robust correlation measure).
#'
#' @details
#' The Pearson correlation coefficient measures the linear relationship between two continuous
#' variables and is suitable when the data follows a bivariate normal distribution. The Spearman
#' and Kendall correlations are non-parametric measures of monotonic association, making them
#' suitable for non-linear relationships and when the data deviates from normality.
#' The Chatterjee correlation coefficient \eqn{\xi_n(X, Y)} measures how much the response
#' `var` is a (possibly non-monotonic) function of each other variable; it lies between
#' 0 and 1 (asymptotically) and is not symmetric. The biweight midcorrelation is
#' a robust correlation measure that downweights the influence of outliers and is
#' recommended when the data contains extreme values or deviates significantly
#' from normality.
#'
#' Missing values are handled pairwise: each coefficient uses the observations where both
#' the response and the other variable are available. With several responses, each one
#' uses its own observations, so a response with many missing values does not reduce the
#' data of the others.
#'
#' @param x A data frame or tibble containing the variables of interest.
#' @param var The response variable(s): one or several columns, given unquoted, as
#'   strings, or with a tidyselect helper, such as `c(K, Ca)` or
#'   `dplyr::all_of(minerals)`. The other columns are the variables correlated with
#'   each response.
#' @param method A character string indicating the correlation method to use. Allowed values are "pearson", "spearman", "kendall", "chatterjee", or "bicor" (for biweight midcorrelation). The default is "pearson".
#' @param plot A logical value indicating whether to produce a visualization of the correlations. Default is FALSE (no plot).
#' @param color The colors of the plot: one color, or two for positive and
#'   negative correlations (the two ends of the color scale of a heatmap).
#'   Default is `c("#1f4e79", "#c0392b")`.
#' @param interactive A logical value indicating whether to create an interactive plot using plotly. Default is FALSE (static ggplot2 plot).
#' @param top For a bar chart (or a heatmap of variables that are not
#'   wavelengths), the number of variables with the largest absolute
#'   correlations to show. Default is `NULL` (all).
#' @param cluster With several responses, a logical: order the rows of the
#'   heatmap by a hierarchical clustering of the responses on their
#'   correlations, and draw its dendrogram (`FALSE`, default). Ignored with a
#'   single response.
#'
#' @section Plots:
#' When the variables are named by wavelengths (the channels of spectra),
#' the plot is a correlation spectrum: the correlation at each wavelength,
#' with the thresholds of significance at the 5% level (for the Pearson,
#' Spearman and biweight coefficients, from the t distribution; not
#' corrected for multiple testing). Otherwise, it is a chart of the
#' correlation of each variable, sorted, colored by sign and labeled with
#' its value. The interactive versions show the variable and its
#' correlation on hover; the correlation spectrum uses WebGL, so it stays
#' fast with thousands of channels.
#'
#' With several responses, the plot is a heatmap: one row per response, in
#' the order given, and one column per wavelength (each tile as wide as the
#' spacing of the channels, with gaps between detectors) or per variable.
#' The color scale is fixed, from -1 to 1 (0 to 1 for Chatterjee's
#' coefficient), so that a color means the same correlation in every row and
#' in every plot.
#'
#' With `cluster = TRUE`, the responses are clustered on their correlation
#' profiles, the correlations with all the variables (as a function of
#' wavelength, for spectra): the distance between two responses is
#' \eqn{1 - r}, where \eqn{r} is the Pearson correlation between their
#' profiles, and the clusters are merged by average linkage. Responses whose
#' correlations rise and fall at the same wavelengths, such as elements with
#' lines in the same regions or that vary together in the samples, are then
#' adjacent, and the dendrogram on the left shows how similar they are. The
#' static plot then needs the patchwork package; the interactive one is
#' only reordered.
#'
#' @return
#' - If `plot = FALSE`, a tibble with columns `variable`, `.correlation` and `method`, sorted by decreasing correlation. With several responses, it starts with a column `outcome`, and is sorted within each response.
#' - If `plot = TRUE`, a list containing the tibble (`correlation`) and a `ggplot2` object (`plot`; a patchwork object with `cluster = TRUE`), and with `cluster = TRUE` the clustering (`clustering`, an [stats::hclust()] object).
#' - If `plot = TRUE` and `interactive = TRUE`, a `plotly` object.
#'
#' @references
#' - Chatterjee, S. (2021). A new coefficient of correlation.
#'   Journal of the American Statistical Association, 116(536):2009-2022.
#' - Wilcox, R. (2012). Introduction to robust estimation and hypothesis testing (3rd ed.).
#'   Academic Press. (ISBN 978-0123869838).
#'
#' @author Christian L. Goueguel
#'
#' @export correlation
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(50))
#' df$a <- 2 * df$y + rnorm(50, sd = 0.5)
#' df$b <- -df$y + rnorm(50)
#' df$c <- rnorm(50)
#' correlation(df, y)
#' correlation(df, "y", method = "bicor")
#' correlation(df, y, plot = TRUE)$plot
#'
#' # LIBS spectra of forage samples and their mineral contents
#' data(forageLIBS)
#' spectra_id <- names(forageLIBS)[1:2]
#' minerals <- names(forageLIBS)[3:14]
#' spectra <- forageLIBS[setdiff(names(forageLIBS), spectra_id)]
#'
#' # a correlation spectrum: potassium against every channel
#' k <- correlation(spectra[setdiff(names(spectra), setdiff(minerals, "K"))], K, plot = TRUE)
#' k$plot
#'
#' # a heatmap: every mineral against every channel
#' all_minerals <- correlation(spectra, dplyr::all_of(minerals), plot = TRUE)
#' all_minerals$plot
#'
#' # the same, with the minerals clustered by their correlation profiles
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   correlation(spectra, dplyr::all_of(minerals), plot = TRUE, cluster = TRUE)$plot
#' }
#'
correlation <- function(x, var, method = "pearson", plot = FALSE,
                        color = c("#1f4e79", "#c0392b"), interactive = FALSE, top = NULL,
                        cluster = FALSE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.data.frame(x) || !all(vapply(x, is.numeric, logical(1)))) {
    stop("Input 'x' must be a numeric data frame")
  }
  outcomes <- tryCatch(names(dplyr::select(x, {{ var }})), error = function(e) character())
  if (length(outcomes) == 0) {
    stop("'var' not found in the data frame")
  }
  valid_methods <- c("pearson", "spearman", "kendall", "chatterjee", "bicor")
  if (!is.character(method) || length(method) != 1 || !method %in% valid_methods) {
    stop("Invalid method specified.")
  }
  if (!is.logical(plot)) {
    stop("'plot' must be of type boolean (TRUE or FALSE)")
  }
  check_flag(interactive, "interactive")
  check_flag(cluster, "cluster")

  others <- setdiff(names(x), outcomes)
  if (length(others) == 0) {
    stop("'x' must have variables other than 'var'.", call. = FALSE)
  }

  tbl_corr <- dplyr::bind_rows(lapply(outcomes, function(response) {
    values <- correlation_values(x, response, others, method)
    tibble::tibble(outcome = response, variable = others, .correlation = values, method = method)
  }))
  tbl_corr <- tbl_corr[!is.na(tbl_corr$.correlation), ]
  tbl_corr <- tbl_corr[order(match(tbl_corr$outcome, outcomes), -tbl_corr$.correlation), ]
  several <- length(outcomes) > 1
  if (!several) {
    tbl_corr$outcome <- NULL
  }

  if (!plot) {
    return(tbl_corr)
  }

  if (!is.character(color) || length(color) < 1 || length(color) > 2) {
    stop("'color' must be one color, or two (positive and negative correlations).", call. = FALSE)
  }
  color <- rep_len(color, 2)
  if (!is.null(top)) check_count(top, "top")
  wl <- suppressWarnings(as.numeric(tbl_corr$variable))
  spectral <- length(unique(tbl_corr$variable)) > 1 && !anyNA(wl)
  if (several) {
    clustering <- NULL
    rows <- outcomes
    if (cluster) {
      clustering <- correlation_clustering(tbl_corr, outcomes)
      rows <- clustering$labels[clustering$order]
      if (!interactive) {
        rlang::check_installed("patchwork", reason = "to draw the dendrogram of the clustering.")
      }
    }
    p <- correlation_heatmap(tbl_corr, rows, spectral, method, top, color, interactive,
                             if (interactive) NULL else clustering)
    if (interactive) {
      return(p)
    }
    out <- list(correlation = tbl_corr, plot = p)
    if (cluster) out$clustering <- clustering
    return(out)
  }
  n <- sum(!is.na(x[[outcomes]]))
  label <- correlation_label(method, outcomes)
  threshold <- if (method %in% c("pearson", "spearman", "bicor") && n > 3) {
    t <- stats::qt(0.975, df = n - 2)
    t / sqrt(n - 2 + t^2)
  }
  if (spectral) {
    p <- correlation_spectrum(tbl_corr, wl, label, threshold, color, interactive)
  } else {
    if (!is.null(top) && top < nrow(tbl_corr)) {
      tbl_plot <- tbl_corr[order(abs(tbl_corr$.correlation), decreasing = TRUE)[seq_len(top)], ]
    } else {
      tbl_plot <- tbl_corr
    }
    p <- correlation_bars(tbl_plot, method, label, threshold, color, interactive)
  }
  if (interactive) {
    return(p)
  }
  list(correlation = tbl_corr, plot = p)
}

# ---- internals ---------------------------------------------------------------

# The correlation of one response with each of the other variables, on the
# observations where both are available. Pearson and Spearman correlations of
# variables without missing values are computed in one call.
correlation_values <- function(x, outcome, others, method) {
  response <- x[[outcome]]
  keep <- !is.na(response)
  y <- response[keep]
  if (length(y) < 3 || length(unique(y)) < 2) {
    return(rep(NA_real_, length(others)))
  }
  xm <- as.matrix(x[keep, others, drop = FALSE])
  if (method %in% c("pearson", "spearman") && !anyNA(xm)) {
    # constant variables give NA
    return(unname(suppressWarnings(stats::cor(xm, y, method = method))[, 1]))
  }
  coef_fun <- switch(
    method,
    pearson = function(a, b) stats::cor(a, b, method = "pearson"),
    spearman = function(a, b) stats::cor(a, b, method = "spearman"),
    kendall = function(a, b) stats::cor(a, b, method = "kendall"),
    chatterjee = function(a, b) XICOR::calculateXI(a, b),
    bicor = function(a, b) tryCatch(biweight_midcorrelation(a, b), error = function(e) NA_real_)
  )
  unname(vapply(seq_along(others), function(j) {
    v <- xm[, j]
    ok <- !is.na(v)
    if (sum(ok) < 3 || length(unique(v[ok])) < 2 || length(unique(y[ok])) < 2) {
      return(NA_real_)
    }
    as.numeric(coef_fun(v[ok], y[ok]))
  }, numeric(1)))
}

correlation_name <- function(method) {
  switch(method, pearson = "Pearson correlation", spearman = "Spearman correlation",
         kendall = "Kendall correlation", chatterjee = "Chatterjee's xi",
         bicor = "Biweight midcorrelation")
}

correlation_label <- function(method, var_name) {
  paste(correlation_name(method), "with", var_name)
}

# Correlation against wavelength.
correlation_spectrum <- function(tbl, wl, label, threshold, color, interactive) {
  df <- data.frame(wavelength = wl, correlation = tbl$.correlation)
  df <- df[order(df$wavelength), ]
  # break the line at the gaps between detectors
  step <- diff(df$wavelength)
  df$segment <- cumsum(c(TRUE, step > 5 * stats::median(step)))
  if (interactive) {
    rlang::check_installed("plotly", reason = "to create interactive plots.")
    gaps <- which(diff(df$segment) > 0)
    if (length(gaps) > 0) {
      breaks <- data.frame(wavelength = (df$wavelength[gaps] + df$wavelength[gaps + 1]) / 2,
                           correlation = NA_real_, segment = NA_integer_)
      df <- rbind(df, breaks)
      df <- df[order(df$wavelength), ]
    }
    p <- plotly::plot_ly(df, x = ~wavelength, y = ~correlation, type = "scattergl", mode = "lines",
                         line = list(color = color[1], width = 1), name = "correlation",
                         hovertemplate = "%{x:.3f} nm<br>r = %{y:.3f}<extra></extra>")
    shapes <- list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                        line = list(color = "grey", width = 1)))
    if (!is.null(threshold)) {
      shapes <- c(shapes, lapply(c(-1, 1) * threshold, function(h) {
        list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = h, y1 = h,
             line = list(color = color[2], width = 1, dash = "dash"))
      }))
    }
    return(plotly::layout(p, title = plotly_title(label), shapes = shapes, hovermode = "x",
                          xaxis = list(title = "Wavelength (nm)"),
                          yaxis = list(title = "Correlation")))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(.data$wavelength, .data$correlation)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3)
  if (!is.null(threshold)) {
    p <- p + ggplot2::geom_hline(yintercept = c(-1, 1) * threshold, colour = color[2],
                                 linetype = "dashed", linewidth = 0.4)
  }
  p <- p +
    ggplot2::geom_line(ggplot2::aes(group = .data$segment), colour = color[1], linewidth = 0.3) +
    ggplot2::labs(x = "Wavelength (nm)", y = "Correlation", title = label,
                  subtitle = if (!is.null(threshold)) "Dashed lines: p = 0.05 (not corrected for multiple testing)") +
    ggplot2::theme_bw()
  finish_title(p)
}

# Sorted chart of the correlation of each variable.
correlation_bars <- function(tbl, method, label, threshold, color, interactive) {
  df <- data.frame(variable = tbl$variable, correlation = tbl$.correlation)
  df <- df[order(df$correlation), ]
  df$variable <- factor(df$variable, levels = df$variable)
  df$sign <- ifelse(df$correlation >= 0, "positive", "negative")
  lower <- if (method == "chatterjee") min(0, df$correlation) else -1
  if (interactive) {
    rlang::check_installed("plotly", reason = "to create interactive plots.")
    p <- plotly::plot_ly(df, x = ~correlation, y = ~variable, type = "bar", orientation = "h",
                         marker = list(color = ifelse(df$sign == "positive", color[1], color[2])),
                         text = sprintf("%.2f", df$correlation), textposition = "outside",
                         hovertemplate = "%{y}<br>%{x:.3f}<extra></extra>")
    shapes <- if (!is.null(threshold)) {
      lapply(c(-1, 1) * threshold, function(v) {
        list(type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = v, x1 = v,
             line = list(color = "grey", width = 1, dash = "dash"))
      })
    }
    return(plotly::layout(p, title = plotly_title(label), shapes = shapes,
                          xaxis = list(title = "Correlation", range = c(lower, 1.1), zeroline = TRUE),
                          yaxis = list(title = "")))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$correlation, y = .data$variable)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3)
  if (!is.null(threshold)) {
    p <- p + ggplot2::geom_vline(xintercept = c(-1, 1) * threshold, colour = "grey60",
                                 linetype = "dashed", linewidth = 0.4)
  }
  p <- p +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$correlation, yend = .data$variable,
                                       colour = .data$sign), linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$sign), size = 2.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$correlation),
                                    hjust = ifelse(.data$correlation >= 0, -0.35, 1.35)),
                       size = 3, colour = "grey20") +
    ggplot2::scale_colour_manual(values = c(positive = color[1], negative = color[2]), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(lower - 0.1 * (lower < 0), 1.1),
                                breaks = if (lower < 0) c(-1, -0.5, 0, 0.5, 1) else c(0, 0.25, 0.5, 0.75, 1)) +
    ggplot2::labs(x = if (method == "chatterjee") "xi" else "Correlation", y = NULL, title = label,
                  subtitle = if (!is.null(threshold)) "Dashed lines: p = 0.05") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
  finish_title(p)
}

# Heatmap of the correlations of several responses (rows, in the order given)
# with wavelengths or variables (columns).
correlation_heatmap <- function(tbl, outcomes, spectral, method, top, color, interactive,
                                clustering = NULL) {
  xi <- method == "chatterjee"
  limits <- if (xi) c(0, 1) else c(-1, 1)
  name <- correlation_name(method)
  legend <- if (xi) "xi" else "Correlation"
  outcomes <- outcomes[outcomes %in% tbl$outcome]
  df <- data.frame(outcome = tbl$outcome, variable = tbl$variable, correlation = tbl$.correlation)

  if (spectral) {
    df$wavelength <- as.numeric(df$variable)
    wl <- sort(unique(df$wavelength))
    # each tile spans halfway to its neighbors, within a detector segment
    step <- diff(wl)
    gap <- step > 5 * stats::median(step)
    left <- c(NA, step)
    right <- c(step, NA)
    left[c(TRUE, gap)] <- right[c(TRUE, gap)]
    right[c(gap, TRUE)] <- left[c(gap, TRUE)]
    half <- data.frame(wavelength = wl, xmin = wl - left / 2, xmax = wl + right / 2)
    df <- merge(df, half, by = "wavelength")
  } else {
    variables <- unique(df$variable)
    if (!is.null(top) && top < length(variables)) {
      strength <- tapply(abs(df$correlation), df$variable, max)
      variables <- names(sort(strength, decreasing = TRUE))[seq_len(top)]
      df <- df[df$variable %in% variables, ]
    }
    df$variable <- factor(df$variable, levels = variables)
  }
  df$outcome <- factor(df$outcome, levels = rev(outcomes))

  if (interactive) {
    rlang::check_installed("plotly", reason = "to create interactive plots.")
    columns <- if (spectral) sort(unique(df$wavelength)) else levels(df$variable)
    key <- if (spectral) df$wavelength else as.character(df$variable)
    z <- matrix(NA_real_, length(outcomes), length(columns))
    z[cbind(match(as.character(df$outcome), outcomes), match(key, columns))] <- df$correlation
    if (spectral) {
      # empty columns between detectors, so that the gaps are not filled
      step <- diff(columns)
      gaps <- which(step > 5 * stats::median(step))
      if (length(gaps) > 0) {
        columns <- c(columns, (columns[gaps] + columns[gaps + 1]) / 2)
        z <- cbind(z, matrix(NA_real_, nrow(z), length(gaps)))
        o <- order(columns)
        columns <- columns[o]
        z <- z[, o, drop = FALSE]
      }
    }
    scale <- if (xi) {
      list(c(0, "white"), c(1, color[1]))
    } else {
      list(c(0, color[2]), c(0.5, "white"), c(1, color[1]))
    }
    p <- plotly::plot_ly(x = columns, y = outcomes, z = z, type = "heatmap", colorscale = scale,
                         zmin = limits[1], zmax = limits[2], colorbar = list(title = legend),
                         hovertemplate = paste0("%{y}<br>%{x}", if (spectral) " nm", "<br>",
                                                legend, " = %{z:.3f}<extra></extra>"))
    return(plotly::layout(p, title = plotly_title(name),
                          xaxis = list(title = if (spectral) "Wavelength (nm)" else ""),
                          yaxis = list(title = "", autorange = "reversed")))
  }

  if (spectral) {
    p <- ggplot2::ggplot(df) +
      ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                      ymin = as.numeric(.data$outcome) - 0.5,
                                      ymax = as.numeric(.data$outcome) + 0.5,
                                      fill = .data$correlation)) +
      ggplot2::scale_y_continuous(breaks = seq_along(levels(df$outcome)), labels = levels(df$outcome),
                                  expand = c(0, 0)) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::labs(x = "Wavelength (nm)", y = NULL)
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(.data$variable, .data$outcome, fill = .data$correlation)) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      ggplot2::labs(x = NULL, y = NULL)
    if (nlevels(df$variable) <= 30) {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$correlation)),
                                  size = 2.6, colour = "grey15")
    }
  }
  fill <- if (xi) {
    ggplot2::scale_fill_gradient(low = "white", high = color[1], limits = limits, name = legend)
  } else {
    ggplot2::scale_fill_gradient2(low = color[2], mid = "white", high = color[1], midpoint = 0,
                                  limits = limits, name = legend)
  }
  p <- p +
    fill +
    ggplot2::labs(title = name) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  if (!spectral) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  if (is.null(clustering)) {
    return(finish_title(p))
  }
  # the dendrogram on the left, its leaves level with the rows
  rows <- levels(df$outcome)
  segments <- dendrogram_segments(clustering, stats::setNames(seq_along(rows), rows))
  tree <- ggplot2::ggplot(segments) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                          colour = "grey30", linewidth = 0.4) +
    ggplot2::scale_x_reverse(expand = ggplot2::expansion(mult = c(0.05, 0))) +
    ggplot2::scale_y_continuous(limits = c(0.5, length(rows) + 0.5), expand = c(0, 0)) +
    ggplot2::theme_void()
  heading <- split_title(name)
  patchwork::wrap_plots(tree, p + ggplot2::labs(title = NULL), widths = c(1, 8)) +
    patchwork::plot_annotation(title = heading$title, subtitle = heading$subtitle,
                               theme = bold_title())
}

# Hierarchical clustering of the responses on their correlation profiles:
# 1 - r between profiles, average linkage.
correlation_clustering <- function(tbl, outcomes) {
  profiles <- tapply(tbl$.correlation, list(factor(tbl$outcome, levels = outcomes), tbl$variable),
                     function(v) v[1])
  similarity <- suppressWarnings(stats::cor(t(profiles), use = "pairwise.complete.obs"))
  distance <- 1 - similarity
  distance[is.na(distance)] <- 1
  diag(distance) <- 0
  stats::hclust(stats::as.dist(distance), method = "average")
}

# The segments of a dendrogram drawn sideways: heights along x, the leaves at
# the y `position`s (named by label).
dendrogram_segments <- function(clustering, position) {
  merges <- clustering$merge
  node_y <- numeric(nrow(merges))
  node <- function(k) {
    if (k < 0) {
      c(y = position[[clustering$labels[-k]]], height = 0)
    } else {
      c(y = node_y[k], height = clustering$height[k])
    }
  }
  segments <- lapply(seq_len(nrow(merges)), function(i) NULL)
  for (i in seq_len(nrow(merges))) {
    a <- node(merges[i, 1])
    b <- node(merges[i, 2])
    h <- clustering$height[i]
    node_y[i] <- (a[["y"]] + b[["y"]]) / 2
    segments[[i]] <- data.frame(x = c(a[["height"]], b[["height"]], h), xend = c(h, h, h),
                                y = c(a[["y"]], b[["y"]], a[["y"]]),
                                yend = c(a[["y"]], b[["y"]], b[["y"]]))
  }
  do.call(rbind, segments)
}
