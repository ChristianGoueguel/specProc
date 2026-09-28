#' @title Correlation Coefficients: Pearson, Spearman, Kendall, Chatterjee, and Biweight Midcorrelation
#'
#' @description
#' Computes various correlation coefficients between a specified response variable
#' and each of the remaining variables in a given data frame or tibble. The available correlation
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
#' the response and the other variable are available.
#'
#' @param x A data frame or tibble containing the variables of interest.
#' @param var The response variable, given unquoted or as a string.
#' @param method A character string indicating the correlation method to use. Allowed values are "pearson", "spearman", "kendall", "chatterjee", or "bicor" (for biweight midcorrelation). The default is "pearson".
#' @param plot A logical value indicating whether to produce a visualization of the correlations. Default is FALSE (no plot).
#' @param color The colors of the plot: one color, or two for positive and
#'   negative correlations. Default is `c("#1f4e79", "#c0392b")`.
#' @param interactive A logical value indicating whether to create an interactive plot using plotly. Default is FALSE (static ggplot2 plot).
#' @param top For a bar chart, the number of variables with the largest
#'   absolute correlations to show. Default is `NULL` (all).
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
#' @return
#' - If `plot = FALSE`, a tibble with columns `variable`, `.correlation` and `method`, sorted by decreasing correlation.
#' - If `plot = TRUE`, a list containing the tibble (`correlation`) and a `ggplot2` object (`plot`).
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
#' # a correlation spectrum: clay content against every channel
#' data(soilLIBS)
#' clay <- average(soilLIBS[c(1, 3, 9:7160)], Sample)
#' correlation(clay[-1], Clay, plot = TRUE)$plot
#'
correlation <- function(x, var, method = "pearson", plot = FALSE,
                        color = c("#1f4e79", "#c0392b"), interactive = FALSE, top = NULL) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.data.frame(x) || !all(vapply(x, is.numeric, logical(1)))) {
    stop("Input 'x' must be a numeric data frame")
  }
  var_name <- rlang::as_name(rlang::enquo(var))
  if (!var_name %in% colnames(x)) {
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

  response <- x[[var_name]]
  others <- setdiff(names(x), var_name)

  coef_fun <- switch(
    method,
    pearson = function(a, b) stats::cor(a, b, method = "pearson"),
    spearman = function(a, b) stats::cor(a, b, method = "spearman"),
    kendall = function(a, b) stats::cor(a, b, method = "kendall"),
    chatterjee = function(a, b) XICOR::calculateXI(a, b),
    bicor = function(a, b) tryCatch(biweight_midcorrelation(a, b), error = function(e) NA_real_)
  )

  values <- vapply(others, function(nm) {
    v <- x[[nm]]
    ok <- stats::complete.cases(v, response)
    if (sum(ok) < 3 || length(unique(v[ok])) < 2 || length(unique(response[ok])) < 2) {
      return(NA_real_)
    }
    as.numeric(coef_fun(v[ok], response[ok]))
  }, numeric(1))

  tbl_corr <- tibble::tibble(
    variable = others,
    .correlation = unname(values),
    method = method
  )
  tbl_corr <- tbl_corr[!is.na(tbl_corr$.correlation), ]
  tbl_corr <- tbl_corr[order(tbl_corr$.correlation, decreasing = TRUE), ]

  if (!plot) {
    return(tbl_corr)
  }

  if (!is.character(color) || length(color) < 1 || length(color) > 2) {
    stop("'color' must be one color, or two (positive and negative correlations).", call. = FALSE)
  }
  color <- rep_len(color, 2)
  if (!is.null(top)) check_count(top, "top")
  n <- sum(stats::complete.cases(response))
  label <- correlation_label(method, var_name)
  wl <- suppressWarnings(as.numeric(tbl_corr$variable))
  spectral <- nrow(tbl_corr) > 1 && !anyNA(wl)
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

correlation_label <- function(method, var_name) {
  name <- switch(method, pearson = "Pearson correlation", spearman = "Spearman correlation",
                 kendall = "Kendall correlation", chatterjee = "Chatterjee's xi",
                 bicor = "Biweight midcorrelation")
  paste(name, "with", var_name)
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
    return(plotly::layout(p, title = label, shapes = shapes, hovermode = "x",
                          xaxis = list(title = "Wavelength (nm)"),
                          yaxis = list(title = "Correlation")))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(.data$wavelength, .data$correlation)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3)
  if (!is.null(threshold)) {
    p <- p + ggplot2::geom_hline(yintercept = c(-1, 1) * threshold, colour = color[2],
                                 linetype = "dashed", linewidth = 0.4)
  }
  p +
    ggplot2::geom_line(ggplot2::aes(group = .data$segment), colour = color[1], linewidth = 0.3) +
    ggplot2::labs(x = "Wavelength (nm)", y = "Correlation", title = label,
                  subtitle = if (!is.null(threshold)) "Dashed lines: p = 0.05 (not corrected for multiple testing)") +
    ggplot2::theme_bw()
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
    return(plotly::layout(p, title = label, shapes = shapes,
                          xaxis = list(title = "Correlation", range = c(lower, 1.1), zeroline = TRUE),
                          yaxis = list(title = "")))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$correlation, y = .data$variable)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3)
  if (!is.null(threshold)) {
    p <- p + ggplot2::geom_vline(xintercept = c(-1, 1) * threshold, colour = "grey60",
                                 linetype = "dashed", linewidth = 0.4)
  }
  p +
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
}
