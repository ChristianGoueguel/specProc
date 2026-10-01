#' @title Loadings of a PCA as Spectra
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Plots the loadings of a PCA against wavelength, one panel per component,
#' and labels the wavelengths that contribute most to each component, with
#' the emission lines they match when a line list is given. With
#' `type = "contribution"`, plots instead the variance of each wavelength
#' explained by the components, which shows in one panel which lines drive
#' the variance of the spectra.
#'
#' @details
#' **Loadings.** Each loading is drawn red above zero and blue below. The
#' wavelengths on the same side of zero increase together along the
#' component; those on opposite sides vary in opposite directions, as with
#' matrix effects or competing emitters. The sign of a component is
#' arbitrary: each component is oriented so that its largest absolute
#' loading is positive, which keeps the plots stable between fits.
#'
#' **Peaks.** The `top` largest positive and negative local extrema of each
#' loading are labeled with their wavelength (see [loading_peaks()]). A local
#' extremum is the largest (or smallest) value within `span` channels on
#' each side, on the same detector segment. Labels that would overlap are
#' moved aside, with a leader line to their peak. A positive and a negative
#' peak side by side on the same line (a derivative shape) usually comes
#' from a shift or a broadening of the line between spectra rather than
#' from a change in its intensity; [wavelength_calibration()] corrects
#' shifts.
#'
#' **Emission lines.** With a line list from [libs_lines()], each peak
#' within `tol` nm of lines is labeled with the nearest one (in bold). All
#' the candidates are listed by [loading_peaks()]. Uncalibrated spectra can
#' be shifted by more than `tol` from the tabulated lines: correct them with
#' [wavelength_calibration()] or increase `tol`, and check the candidates.
#' Only the lines of the species in the list can be matched.
#'
#' **Contribution.** The variance of wavelength \eqn{j} explained by the
#' components \eqn{K} is \eqn{s_j^2 \sum_{k \in K} \lambda_k p_{jk}^2},
#' with \eqn{\lambda_k} the variance of the scores of component \eqn{k},
#' \eqn{p_{jk}} the loading and \eqn{s_j} the scale of the variable (1
#' without scaling), in the squared units of the data.
#'
#' **Preprocessing.** The loadings reflect the preprocessing. With centered
#' spectra, the variance of a channel grows with its intensity, so strong
#' lines dominate the loadings. With autoscaled spectra, every channel has
#' the same variance, and weak lines and noise weigh as much as strong
#' lines.
#'
#' **Explained variance.** For a [stats::prcomp()] fit, the percentages in
#' the panel titles are shares of the total variance. For [robpca()],
#' [rospca()] and [macropca()] fits, which estimate only the `k` components,
#' they are shares of the variance of these `k` components.
#'
#' @param model A [stats::prcomp()] fit or an object returned by [robpca()],
#'   [rospca()] or [macropca()]. The variable names must be the
#'   wavelengths; otherwise, the variables are numbered.
#' @param components The components to show (numbers). Default is the first
#'   three (or all of them, if fewer).
#' @param type `"loadings"` (default) for one panel of loadings per
#'   component, or `"contribution"` for the variance of each wavelength
#'   explained by `components`.
#' @param top The number of peaks labeled per component (per panel). Default
#'   is 10; use 0 for no labels.
#' @param lines Optional line list returned by [libs_lines()], to label the
#'   peaks with the emission lines they match.
#' @param tol The largest distance, in nm, between a peak and a line it
#'   matches. Default is 0.1.
#' @param spectra Optional spectra (a data frame or matrix with the
#'   variables of the model, other columns being ignored) or a single
#'   spectrum (a named numeric vector), whose mean is drawn in grey behind
#'   each panel, rescaled to it, to show whether a peak is on an emission
#'   line.
#' @param span The half-width, in channels, of the window in which a peak
#'   is a local extremum. Default is 5.
#' @param interactive If `TRUE`, the plot is made with plotly, with the
#'   peaks and their candidate lines shown on hover. Default is `FALSE`.
#' @param title The plot title.
#'
#' @return A ggplot object, or a plotly object if `interactive = TRUE`. The
#'   room left for the labels suits plots about 8 inches high; make the plot
#'   taller if labels are clipped.
#'
#' @seealso [loading_peaks()], [libs_lines()], [robpca()],
#'   [plot_outlier_map()]
#' @export
#'
#' @examples
#' # LIBS spectra of forage samples
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' spectra <- dplyr::select(forageLIBS, -Measurement, -Sample, -dplyr::all_of(minerals))
#' set.seed(1)
#' fit <- robpca(center(spectra))
#' plot_loadings(fit, spectra = spectra)
#' plot_loadings(fit, type = "contribution", spectra = spectra)
#' \dontrun{
#' # label the peaks with emission lines from the NIST database
#' lines <- libs_lines(c("Ca I", "Ca II", "K I", "Mg I", "Mg II", "Na I", "C I", "H I"))
#' plot_loadings(fit, lines = lines, spectra = spectra)
#' }
#'
plot_loadings <- function(model, components = NULL, type = c("loadings", "contribution"),
                          top = 10, lines = NULL, tol = 0.1, spectra = NULL, span = 5,
                          interactive = FALSE, title = NULL) {
  type <- match.arg(type)
  check_flag(interactive, "interactive")
  parts <- loading_parts(model)
  components <- loading_components(parts, components)
  check_peak_args(top, span, lines, tol, parts)
  curves <- loading_curves(parts, components, type)
  peaks <- find_loading_peaks(curves, parts, top, span, lines, tol)
  background <- if (is.null(spectra)) NULL else loading_background(spectra, parts, curves)
  if (is.null(title)) {
    title <- paste0(model_label(model), if (type == "loadings") " loadings" else
      " explained variance by wavelength")
  }
  x_lab <- if (parts$has_wavelength) "Wavelength (nm)" else "Variable"
  y_lab <- if (type == "loadings") "Loading" else "Explained variance"
  if (interactive) {
    plot_loadings_plotly(curves, peaks, background, x_lab, y_lab, title)
  } else {
    plot_loadings_ggplot(curves, peaks, background, parts, x_lab, y_lab, title)
  }
}

#' @title Peaks of the Loadings of a PCA
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Finds the wavelengths that contribute most to the components of a PCA:
#' the largest positive and negative local extrema of the loadings, or the
#' largest maxima of the variance explained by wavelength, optionally
#' matched to emission lines. [plot_loadings()] labels the same peaks.
#'
#' @details
#' See [plot_loadings()] for the definitions of the peaks, of the explained
#' variance, of the orientation of the components and of the line matching.
#' A peak is flagged as part of a derivative shape (`derivative = TRUE`)
#' when a local extremum of the opposite sign, at least half as large, lies
#' within `2 * span` channels on the same detector segment: this usually
#' means a shift or a broadening of the line between spectra.
#'
#' @inheritParams plot_loadings
#'
#' @return A tibble with one row per peak, sorted by component and by
#'   decreasing absolute value, and columns `component`, `variable` (the
#'   column name), `wavelength`, `value` (the loading, or the explained
#'   variance), `sign` (`"positive"` or `"negative"`) and, for loadings,
#'   `derivative`. With `lines`, it also has the columns `species` and
#'   `line_wavelength` of the matched line (`NA` when none is within `tol`)
#'   and `candidates`, all the lines within `tol`, from the nearest.
#'
#' @seealso [plot_loadings()], [libs_lines()]
#' @export
#'
#' @examples
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' spectra <- dplyr::select(forageLIBS, -Measurement, -Sample, -dplyr::all_of(minerals))
#' set.seed(1)
#' fit <- robpca(center(spectra))
#' loading_peaks(fit, top = 5)
#'
loading_peaks <- function(model, components = NULL, type = c("loadings", "contribution"),
                          top = 10, lines = NULL, tol = 0.1, span = 5) {
  type <- match.arg(type)
  parts <- loading_parts(model)
  components <- loading_components(parts, components)
  check_peak_args(top, span, lines, tol, parts)
  curves <- loading_curves(parts, components, type)
  peaks <- find_loading_peaks(curves, parts, top, span, lines, tol)
  keep <- c("component", "variable", "wavelength", "value", "sign",
            if (type == "loadings") "derivative",
            if (!is.null(lines)) c("species", "line_wavelength", "candidates"))
  out <- peaks[, keep, drop = FALSE]
  out$component <- as.character(out$component)
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# ---- internals ---------------------------------------------------------------

# Loadings (oriented), component variances and shares, variable scales and
# wavelengths of a PCA model.
loading_parts <- function(model) {
  if (inherits(model, "prcomp")) {
    p <- model$rotation
    variance <- model$sdev[seq_len(ncol(p))]^2
    share <- variance / sum(model$sdev^2)
    scale <- if (is.numeric(model$scale)) model$scale else rep(1, nrow(p))
  } else if (inherits(model, "specproc_robpca")) {
    p <- model$loadings
    variance <- model$eigenvalues[seq_len(ncol(p))]
    share <- variance / sum(variance)
    scale <- model$scale %||% rep(1, nrow(p))
  } else {
    stop("'model' must be a prcomp fit or returned by robpca(), rospca() or macropca().",
         call. = FALSE)
  }
  p <- as.matrix(p)
  variables <- rownames(p) %||% attr(model, "variables")
  wavelength <- names_to_wavelength(variables)
  has_wavelength <- length(wavelength) == nrow(p)
  if (is.null(variables)) variables <- as.character(seq_len(nrow(p)))
  if (!has_wavelength) wavelength <- seq_len(nrow(p))
  # the sign of a component is arbitrary: make its largest absolute loading positive
  largest <- p[cbind(apply(abs(p), 2, which.max), seq_len(ncol(p)))]
  p <- sweep(p, 2, ifelse(largest < 0, -1, 1), "*")
  list(loadings = p, variance = variance, share = share, scale = unname(scale),
       variables = variables, wavelength = wavelength, has_wavelength = has_wavelength,
       segment = if (has_wavelength) wavelength_segments(wavelength) else rep(1L, nrow(p)))
}

loading_components <- function(parts, components) {
  m <- ncol(parts$loadings)
  if (is.null(components)) return(seq_len(min(3, m)))
  if (!is.numeric(components) || length(components) == 0 || anyNA(components) ||
      any(components %% 1 != 0) || any(components < 1 | components > m) ||
      anyDuplicated(components)) {
    stop("'components' must be distinct component numbers between 1 and ", m, ".", call. = FALSE)
  }
  as.integer(components)
}

check_peak_args <- function(top, span, lines, tol, parts) {
  check_count(top, "top", lower = 0)
  check_count(span, "span")
  check_number(tol, "tol", lower = 0)
  if (!is.null(lines)) {
    check_lines_table(lines)
    if (!parts$has_wavelength) {
      stop("Matching 'lines' needs variables named by wavelength.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# One curve per panel, in long format: panel, wavelength, value, segment.
loading_curves <- function(parts, components, type) {
  pct <- function(s) paste0(formatC(100 * s, format = "f", digits = 1), "%")
  comp <- paste0("PC", components)
  if (type == "loadings") {
    labels <- paste0(comp, " (", pct(parts$share[components]), ")")
    values <- parts$loadings[, components, drop = FALSE]
  } else {
    range_label <- if (length(components) == 1) comp else
      if (identical(components, seq(min(components), max(components)))) {
        paste0("PC", min(components), "-", max(components))
      } else {
        paste(comp, collapse = ", ")
      }
    share <- sum(parts$share[components])
    labels <- if (share > 1 - 1e-8) range_label else paste0(range_label, " (", pct(share), ")")
    squared <- sweep(parts$loadings[, components, drop = FALSE]^2, 2,
                     parts$variance[components], "*")
    values <- matrix(parts$scale^2 * rowSums(squared), ncol = 1)
  }
  n <- nrow(values)
  data.frame(
    panel = factor(rep(labels, each = n), levels = labels),
    index = rep(seq_len(n), length(labels)),
    wavelength = rep(parts$wavelength, length(labels)),
    value = as.vector(values),
    segment = rep(parts$segment, length(labels)),
    type = type,
    stringsAsFactors = FALSE
  )
}

# Indices of the local maxima of `v`: at least as large as every value within
# `span` channels on each side, on the same segment.
local_maxima <- function(v, segment, span) {
  n <- length(v)
  keep <- rep(TRUE, n)
  for (d in c(-seq_len(span), seq_len(span))) {
    j <- seq_len(n) + d
    inside <- j >= 1 & j <= n
    j[!inside] <- seq_len(n)[!inside]
    compare <- inside & segment[j] == segment
    keep <- keep & (!compare | v >= v[j])
  }
  which(keep)
}

find_loading_peaks <- function(curves, parts, top, span, lines, tol) {
  empty <- data.frame(panel = factor(levels = levels(curves$panel)), component = character(),
                      variable = character(), index = integer(), wavelength = numeric(),
                      value = numeric(), sign = character(), derivative = logical(),
                      species = character(), line_wavelength = numeric(),
                      candidates = character(), stringsAsFactors = FALSE)
  if (top == 0) return(empty)
  out <- lapply(split(curves, curves$panel), function(cv) {
    v <- cv$value
    pos <- local_maxima(v, cv$segment, span)
    pos <- pos[v[pos] > 0]
    neg <- if (cv$type[1] == "loadings") local_maxima(-v, cv$segment, span) else integer()
    neg <- neg[v[neg] < 0]
    idx <- c(pos, neg)
    if (length(idx) == 0) return(NULL)
    chosen <- idx[order(abs(v[idx]), decreasing = TRUE)][seq_len(min(top, length(idx)))]
    # derivative shape: an opposite extremum at least half as large nearby
    derivative <- vapply(chosen, function(i) {
      other <- if (v[i] > 0) neg else pos
      near <- other[abs(other - i) <= 2 * span & cv$segment[other] == cv$segment[i]]
      any(abs(v[near]) >= 0.5 * abs(v[i]))
    }, logical(1))
    data.frame(panel = cv$panel[chosen], component = sub(" \\(.*$", "", as.character(cv$panel[chosen])),
               variable = parts$variables[chosen], index = chosen,
               wavelength = cv$wavelength[chosen], value = v[chosen],
               sign = ifelse(v[chosen] > 0, "positive", "negative"),
               derivative = derivative, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) return(empty)
  matched <- match_peak_lines(out$wavelength, lines, tol)
  cbind(out, matched)
}

# The nearest line within `tol` of each peak, and all the candidates by
# distance (relative intensities compare only lines of the same species).
match_peak_lines <- function(wavelength, lines, tol) {
  n <- length(wavelength)
  out <- data.frame(species = rep(NA_character_, n), line_wavelength = rep(NA_real_, n),
                    candidates = rep(NA_character_, n), stringsAsFactors = FALSE)
  if (is.null(lines) || nrow(lines) == 0) return(out)
  for (i in seq_len(n)) {
    near <- which(abs(lines$wavelength - wavelength[i]) <= tol)
    if (length(near) == 0) next
    near <- near[order(abs(lines$wavelength[near] - wavelength[i]))]
    out$species[i] <- lines$species[near[1]]
    out$line_wavelength[i] <- lines$wavelength[near[1]]
    out$candidates[i] <- paste(lines$species[near], format_wavelength(lines$wavelength[near], 2),
                               collapse = ", ")
  }
  out
}

format_wavelength <- function(w, digits) formatC(w, format = "f", digits = digits)

# Digits that resolve the channel spacing.
wavelength_digits <- function(wl) {
  spacing <- stats::median(abs(diff(sort(unique(wl)))))
  if (!is.finite(spacing) || spacing <= 0) return(2L)
  as.integer(min(4, max(0, ceiling(-log10(spacing)))))
}

# Peak labels, spread along the wavelength axis so that they do not overlap
# (each keeps a leader line to its peak).
loading_labels <- function(peaks, parts) {
  empty <- data.frame(panel = peaks$panel[0], wavelength = numeric(), value = numeric(),
                      sign = character(), label = character(), matched = logical(),
                      x = numeric(), stringsAsFactors = FALSE)
  if (nrow(peaks) == 0) return(empty)
  digits <- if (parts$has_wavelength) wavelength_digits(parts$wavelength) else 0L
  out <- data.frame(
    panel = peaks$panel, wavelength = peaks$wavelength, value = peaks$value, sign = peaks$sign,
    label = ifelse(is.na(peaks$species), format_wavelength(peaks$wavelength, digits),
                   paste(peaks$species, format_wavelength(peaks$line_wavelength, 2))),
    matched = !is.na(peaks$species), stringsAsFactors = FALSE
  )
  gap <- 0.013 * diff(range(parts$wavelength))
  out$x <- out$wavelength
  for (g in split(seq_len(nrow(out)), list(out$panel, out$sign), drop = TRUE)) {
    out$x[g] <- spread_positions(out$wavelength[g], gap)
  }
  out
}

# Positions as close as possible to `x`, at least `gap` apart: overlapping
# labels are merged into evenly spaced groups centered on their targets.
spread_positions <- function(x, gap) {
  o <- order(x)
  target <- x[o]
  groups <- as.list(seq_along(target))
  layout <- function(members) {
    mean(target[members]) + (seq_along(members) - (length(members) + 1) / 2) * gap
  }
  repeat {
    merged <- FALSE
    k <- 1
    while (k < length(groups)) {
      right_edge <- max(layout(groups[[k]]))
      left_edge <- min(layout(groups[[k + 1]]))
      if (left_edge - right_edge < gap) {
        groups[[k]] <- c(groups[[k]], groups[[k + 1]])
        groups[[k + 1]] <- NULL
        merged <- TRUE
      } else {
        k <- k + 1
      }
    }
    if (!merged) break
  }
  pos <- numeric(length(target))
  for (members in groups) pos[members] <- layout(members)
  pos[order(o)]
}

# The mean of `spectra` (a data frame or matrix with the variables `vars`,
# other columns being ignored, or a single named spectrum), as a numeric
# vector in the order of `vars`.
mean_spectrum <- function(spectra, vars) {
  if (is.numeric(spectra) && is.null(dim(spectra))) {
    out <- if (!is.null(names(spectra)) && all(vars %in% names(spectra))) {
      spectra[vars]
    } else if (length(spectra) == length(vars)) {
      spectra
    } else {
      stop("'spectra' must have the variables of the model.", call. = FALSE)
    }
  } else if (is.data.frame(spectra) || is.matrix(spectra)) {
    if (!is.null(colnames(spectra)) && all(vars %in% colnames(spectra))) {
      spectra <- spectra[, vars, drop = FALSE]
    } else if (ncol(spectra) != length(vars)) {
      stop("'spectra' must have the variables of the model.", call. = FALSE)
    }
    out <- colMeans(as_numeric_matrix(spectra, "spectra"), na.rm = TRUE)
  } else {
    stop("'spectra' must be a data frame, a matrix or a named numeric vector.", call. = FALSE)
  }
  unname(as.numeric(out))
}

# The mean spectrum, rescaled to the largest absolute value of each panel.
loading_background <- function(spectra, parts, curves) {
  spectrum <- mean_spectrum(spectra, parts$variables)
  top_value <- max(abs(spectrum), na.rm = TRUE)
  if (!is.finite(top_value) || top_value == 0) top_value <- 1
  do.call(rbind, lapply(split(curves, curves$panel), function(cv) {
    cv$value <- spectrum / top_value * max(abs(cv$value), na.rm = TRUE)
    cv
  }))
}

model_label <- function(model) {
  if (inherits(model, "prcomp")) return("PCA")
  switch(class(model)[1], specproc_rospca = "ROSPCA", specproc_macropca = "MacroPCA", "ROBPCA")
}

loading_colours <- c(positive = "#b2182b", negative = "#2166ac")

plot_loadings_ggplot <- function(curves, peaks, background, parts, x_lab, y_lab, title) {
  labels <- loading_labels(peaks, parts)
  curves$sign <- ifelse(curves$value >= 0, "positive", "negative")
  p <- ggplot2::ggplot(curves, ggplot2::aes(x = .data$wavelength))
  if (!is.null(background)) {
    p <- p + ggplot2::geom_line(data = background,
                                ggplot2::aes(y = .data$value, group = .data$segment),
                                colour = "grey80", linewidth = 0.25)
  }
  # one bar per channel: peaks are often a single channel wide
  p <- p +
    ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = .data$value, colour = .data$sign),
                            linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
    ggplot2::scale_colour_manual(values = loading_colours, guide = "none")
  if (nrow(labels) > 0) {
    # vertical labels above positive peaks and below negative ones, with a
    # leader line when the label was moved aside
    extent <- tapply(abs(curves$value), curves$panel, max)[as.character(labels$panel)]
    up <- labels$sign == "positive"
    labels$y <- labels$value + ifelse(up, 0.06, -0.06) * extent
    labels$hjust <- ifelse(up, 0, 1)
    labels$fontface <- ifelse(labels$matched, "bold", "plain")
    # room for the labels, growing with their length and with the number of
    # stacked panels (tuned for plots about 8 inches high)
    per_char <- 0.02 + 0.035 * nlevels(curves$panel)
    room <- data.frame(panel = labels$panel, wavelength = labels$wavelength,
                       value = labels$y + ifelse(up, 1, -1) * per_char * nchar(labels$label) * extent)
    p <- p +
      ggplot2::geom_segment(data = labels,
                            ggplot2::aes(x = .data$wavelength, xend = .data$x,
                                         y = .data$value, yend = .data$y),
                            colour = "grey55", linewidth = 0.25, inherit.aes = FALSE) +
      ggplot2::geom_text(data = labels,
                         ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                                      hjust = .data$hjust, fontface = .data$fontface),
                         angle = 90, size = 2.6, colour = "grey15", inherit.aes = FALSE) +
      ggplot2::geom_blank(data = room, ggplot2::aes(x = .data$wavelength, y = .data$value),
                          inherit.aes = FALSE)
  }
  p <- p +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), ncol = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.05)) +
    ggplot2::labs(x = x_lab, y = y_lab, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey95"),
                   strip.text = ggplot2::element_text(hjust = 0),
                   panel.grid.minor = ggplot2::element_blank())
  finish_title(p)
}

plot_loadings_plotly <- function(curves, peaks, background, x_lab, y_lab, title) {
  rlang::check_installed("plotly", reason = "for interactive plots.")
  # NA between detector segments, so that they are not joined
  with_breaks <- function(cv, column) {
    breaks <- c(diff(cv$segment) != 0, FALSE)
    rows <- rep(seq_len(nrow(cv)), 1 + breaks)
    out <- cv[rows, c("wavelength", column)]
    out[which(duplicated(rows)), ] <- NA
    out
  }
  panels <- levels(curves$panel)
  plots <- lapply(panels, function(panel) {
    cv <- curves[curves$panel == panel, ]
    p <- plotly::plot_ly()
    if (!is.null(background)) {
      bg <- with_breaks(background[background$panel == panel, ], "value")
      p <- plotly::add_trace(p, x = bg$wavelength, y = bg$value, type = "scatter", mode = "lines",
                             fill = "tozeroy", fillcolor = "rgba(190,190,190,0.5)",
                             line = list(color = "rgba(160,160,160,0.8)", width = 0.5),
                             name = "mean spectrum", hoverinfo = "skip", showlegend = FALSE)
    }
    line <- with_breaks(cv, "value")
    p <- plotly::add_trace(p, x = line$wavelength, y = line$value, type = "scattergl",
                           mode = "lines", line = list(color = "#404040", width = 1),
                           name = panel, hoverinfo = "x+y", showlegend = FALSE)
    pk <- peaks[peaks$panel == panel, ]
    if (nrow(pk) > 0) {
      hover <- sprintf("%.3f nm<br>value %.3g%s%s", pk$wavelength, pk$value,
                       ifelse(is.na(pk$candidates), "", paste0("<br>", pk$candidates)),
                       ifelse(pk$derivative %in% TRUE, "<br>derivative shape", ""))
      p <- plotly::add_trace(p, x = pk$wavelength, y = pk$value, type = "scatter", mode = "markers",
                             marker = list(color = unname(loading_colours[pk$sign]), size = 7),
                             text = hover, hoverinfo = "text", showlegend = FALSE)
    }
    plotly::layout(p, yaxis = list(title = panel, zeroline = TRUE))
  })
  p <- plotly::subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE)
  plotly::layout(p, title = plotly_title(title), xaxis = list(title = x_lab), hovermode = "closest")
}
