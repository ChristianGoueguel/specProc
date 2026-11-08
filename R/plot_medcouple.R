#' @title Plot of the Medcouple by Wavelength
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Draws the medcouple ([medcouple()]), a robust measure of skewness, of
#' the intensities at each wavelength of a set of spectra: as bars along
#' the wavelength axis, or with `group` as a heatmap with one row per group
#' of spectra, such as the shots of each sample.
#'
#' @details
#' The medcouple of a channel describes the shape of the distribution of
#' its intensities over the spectra: 0 when it is symmetric, positive when
#' a few spectra are far above the others (a longer upper tail), negative
#' when they are far below. Channels that saturate the detector in many
#' spectra are strongly negative, since their intensities pile up at the
#' top of the range; the lines of an element whose contents are skewed, or
#' that appears in a few spectra only, are positive.
#'
#' Without `group`, the plot has one bar per channel, from 0 to its
#' medcouple, in the first color of the scale if it is negative and in the
#' last if it is positive. With `group`, the medcouple is computed within each group
#' and the plot is a heatmap: one row per group (in the order of the factor
#' levels, or of their first appearance in `x`), and one column per
#' wavelength, each tile as wide as the spacing of the channels, with gaps
#' between detectors (grey where there are no channels). The medcouple of a few spectra takes few distinct
#' values and varies much from one group to another: groups of 20 spectra
#' or more give a clearer picture.
#'
#' The color scale is fixed from -1 to 1, so that a color means the same
#' skewness in every row and in every plot. With two colors (default, blue
#' and red), the scale is diverging, from the first color at -1 to the
#' second at 1 through white at 0, so that the symmetric channels fade out
#' and the skewed ones stand out. With three colors or more, they are
#' spread evenly from -1 to 1, such as `grDevices::hcl.colors(9, "Spectral")`
#' or `grDevices::rainbow(7)`; use an odd number of colors to place the
#' middle one at 0. A diverging palette shows the sign of the medcouple,
#' its main information, better than a rainbow, whose bright yellow and
#' cyan bands draw the eye to values that are not special.
#'
#' @param x A data frame, tibble or matrix with one spectrum per row and the
#'   spectral columns named by their wavelengths; the other columns are
#'   ignored.
#' @param group An optional column of `x` (unquoted or as a string) that
#'   splits the spectra into groups, such as the sample of each shot: one
#'   row of the heatmap per group. Default is `NULL`, a single set of
#'   spectra.
#' @param drop_na Logical value indicating whether to remove missing values
#'   (NA) from the intensities of each channel. If `FALSE` (default), a
#'   channel with missing values has no medcouple (a gap in the plot).
#' @param color The colors of the scale from -1 to 1: two colors, for the
#'   negative and positive medcouples (through white at 0), or three or
#'   more spread evenly. The bars use the first and the last. Default is
#'   `c("#2166ac", "#b2182b")`.
#' @param interactive If `TRUE`, the plot is made with plotly, with the
#'   wavelength and the medcouple (and the group) on hover. Default is
#'   `FALSE`.
#' @param title The plot title. Default is `"Medcouple by wavelength"`. A
#'   long title is split into a title and a subtitle.
#' @param wavelength Optional (`NULL` by default). The wavelength range
#'   `c(min, max)` to draw, in nm.
#' @param xlab,ylab The axis titles. Default are `"Wavelength (nm)"` and
#'   `"Medcouple"`, or the name of `group` for the heatmap.
#' @param base_size The size of the text, in points. Default is 11. Ignored
#'   for interactive plots.
#' @param linewidth The width of the bars (without `group`), or of the line
#'   of an interactive plot. Default is 0.3.
#'
#' @return A ggplot object, or a plotly object if `interactive = TRUE`. The
#'   data of the ggplot holds the medcouple of each wavelength (and group).
#'
#' @seealso [medcouple()], [plot_spectra()], [reject_shots()]
#' @export plot_medcouple
#'
#' @examples
#' data(forageLIBS)
#' # the saturated Ca II lines (393.37 and 396.85 nm) are strongly negative
#' plot_medcouple(forageLIBS, wavelength = c(380, 430))
#'
#' # one row per third of the calcium contents: the Ca II lines saturate at
#' # every level
#' forage <- forageLIBS
#' forage$Calcium <- cut(forage$Ca, stats::quantile(forage$Ca, 0:3 / 3),
#'                       include.lowest = TRUE, labels = c("low", "medium", "high"))
#' plot_medcouple(forage, group = Calcium, wavelength = c(380, 430))
#'
#' # any gradient from -1 to 1
#' plot_medcouple(forage, group = Calcium, wavelength = c(380, 430),
#'                color = grDevices::hcl.colors(9, "Spectral"))
#'
plot_medcouple <- function(x, group = NULL, drop_na = FALSE, color = c("#2166ac", "#b2182b"),
                           interactive = FALSE, title = NULL, wavelength = NULL, xlab = NULL,
                           ylab = NULL, base_size = 11, linewidth = 0.3) {
  if (is.matrix(x)) {
    x <- as.data.frame(x, optional = TRUE)
  }
  if (!is.data.frame(x)) {
    stop("'x' must be a data frame or matrix with one spectrum per row.", call. = FALSE)
  }
  group_quo <- rlang::enquo(group)
  group <- if (rlang::quo_is_null(group_quo)) NULL else rlang::as_name(group_quo)
  if (!is.null(group) && !group %in% names(x)) {
    stop("Column `", group, "` not found in 'x'.", call. = FALSE)
  }
  check_flag(drop_na, "drop_na")
  check_flag(interactive, "interactive")
  check_plot_text(title, xlab, ylab, base_size)
  check_size(linewidth, "linewidth")
  colours <- medcouple_colours(color)
  window <- wavelength_window(wavelength)

  wl <- suppressWarnings(as.numeric(names(x)))
  channels <- !is.na(wl) & vapply(x, is.numeric, logical(1)) & names(x) != (group %||% "")
  channels <- channels & in_window(wl, window)
  if (sum(channels) < 2) {
    stop("'x' needs at least 2 spectral columns named by their wavelengths",
         if (!is.null(window)) " in the wavelength range", ".", call. = FALSE)
  }
  spectra <- as.matrix(x[channels])
  storage.mode(spectra) <- "double"
  wl <- wl[channels]
  o <- order(wl)
  spectra <- spectra[, o, drop = FALSE]
  wl <- wl[o]

  groups <- medcouple_groups(x, group)
  df <- do.call(rbind, lapply(seq_along(groups$rows), function(i) {
    data.frame(group = names(groups$rows)[i], wavelength = wl,
               medcouple = unname(apply(spectra[groups$rows[[i]], , drop = FALSE], 2,
                                        medcouple, drop_na = drop_na)))
  }))
  if (all(is.na(df$medcouple))) {
    stop("Every channel has missing values; use drop_na = TRUE.", call. = FALSE)
  }
  df$group <- factor(df$group, levels = names(groups$rows))
  sizes <- lengths(groups$rows)
  subtitle <- if (is.null(group)) {
    paste(nrow(spectra), "spectra")
  } else if (min(sizes) == max(sizes)) {
    paste0("One row per ", group, ", ", sizes[1], " spectra each")
  } else {
    paste0("One row per ", group, ", ", min(sizes), " to ", max(sizes), " spectra")
  }
  style <- list(title = title %||% "Medcouple by wavelength", subtitle = subtitle,
                x_lab = xlab %||% "Wavelength (nm)",
                y_lab = ylab %||% if (is.null(group)) "Medcouple" else group,
                colours = colours, base_size = base_size, linewidth = linewidth)
  if (is.null(group)) {
    df$group <- NULL
    if (interactive) medcouple_bars_plotly(df, style) else medcouple_bars(df, style)
  } else {
    if (interactive) medcouple_heatmap_plotly(df, style) else medcouple_heatmap(df, style)
  }
}

# ---- internals ---------------------------------------------------------------

# The colors of the scale from -1 to 1: two colors (negative and positive)
# through white, or three or more as given.
medcouple_colours <- function(color) {
  if (!is.character(color) || length(color) < 2) {
    stop("'color' must be two colors (negative and positive medcouples), or more for a ",
         "gradient from -1 to 1.", call. = FALSE)
  }
  for (colour in color) check_colour(colour, "color")
  if (length(color) == 2) c(color[1], "white", color[2]) else color
}

# The rows of `x` in each group, named by the groups: in the order of the
# factor levels, or of their first appearance.
medcouple_groups <- function(x, group) {
  if (is.null(group)) {
    return(list(rows = list(all = seq_len(nrow(x)))))
  }
  g <- x[[group]]
  if (anyNA(g)) {
    stop("Column `", group, "` has missing values.", call. = FALSE)
  }
  levels <- if (is.factor(g)) levels(droplevels(g)) else unique(as.character(g))
  list(rows = split(seq_len(nrow(x)), factor(as.character(g), levels = levels)))
}

medcouple_fill <- function(colours, ...) {
  ggplot2::scale_fill_gradientn(colours = colours, values = seq(0, 1, length.out = length(colours)),
                                limits = c(-1, 1), ...)
}

medcouple_colorscale <- function(colours) {
  at <- seq(0, 1, length.out = length(colours))
  lapply(seq_along(colours), function(i) list(at[i], plotly_colour(colours[i])))
}

# One bar per channel, from 0 to its medcouple, in the colors of the ends of
# the scale (the values near 0 would be white).
medcouple_bars <- function(df, style) {
  df <- df[!is.na(df$medcouple), ]
  df$sign <- ifelse(df$medcouple >= 0, "positive", "negative")
  ends <- c(negative = style$colours[1], positive = style$colours[length(style$colours)])
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$wavelength)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = .data$medcouple, colour = .data$sign),
                            linewidth = style$linewidth) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
    ggplot2::scale_colour_manual(values = ends, guide = "none") +
    ggplot2::scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    ggplot2::labs(x = style$x_lab, y = style$y_lab, title = style$title, subtitle = style$subtitle) +
    ggplot2::theme_bw(base_size = style$base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  finish_title(p)
}

medcouple_bars_plotly <- function(df, style) {
  rlang::check_installed("plotly", reason = "for interactive plots.")
  # NA between detector segments, so that they are not joined
  seg <- wavelength_segments(df$wavelength)
  rows <- rep(seq_len(nrow(df)), 1 + c(diff(seg) != 0, FALSE))
  line <- df[rows, ]
  line[which(duplicated(rows)), ] <- NA
  ends <- vapply(style$colours[c(1, length(style$colours))], plotly_colour, character(1))
  p <- plotly::plot_ly(line, x = ~wavelength, y = ~medcouple, type = "scattergl", mode = "lines+markers",
                       line = list(color = "#808080", width = plotly_width(style$linewidth)),
                       marker = list(color = unname(ifelse(line$medcouple >= 0, ends[2], ends[1])),
                                     size = 4),
                       hovertemplate = "%{x:.3f} nm<br>MC = %{y:.3f}<extra></extra>")
  plotly::layout(p, title = plotly_title(style$title, style$subtitle), hovermode = "closest",
                 xaxis = list(title = style$x_lab),
                 yaxis = list(title = style$y_lab, range = c(-1.05, 1.05), zeroline = TRUE))
}

# One row per group, one tile per wavelength.
medcouple_heatmap <- function(df, style) {
  groups <- levels(df$group)
  df <- merge(df, wavelength_tiles(unique(df$wavelength)), by = "wavelength")
  # the first group on top
  df$row <- length(groups) + 1 - as.integer(df$group)
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$row - 0.5,
                                    ymax = .data$row + 0.5, fill = .data$medcouple)) +
    medcouple_fill(style$colours, name = "Medcouple", na.value = "grey85") +
    ggplot2::scale_y_continuous(breaks = rev(seq_along(groups)), labels = groups, expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = style$x_lab, y = style$y_lab, title = style$title, subtitle = style$subtitle) +
    ggplot2::theme_bw(base_size = style$base_size) +
    # grey where there are no channels, unlike the white of a medcouple of 0
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "grey92"))
  finish_title(p)
}

medcouple_heatmap_plotly <- function(df, style) {
  rlang::check_installed("plotly", reason = "for interactive plots.")
  groups <- levels(df$group)
  wl <- sort(unique(df$wavelength))
  z <- matrix(NA_real_, length(groups), length(wl))
  z[cbind(as.integer(df$group), match(df$wavelength, wl))] <- df$medcouple
  gapped <- heatmap_gaps(wl, z)
  p <- plotly::plot_ly(x = gapped$wl, y = groups, z = gapped$z, type = "heatmap",
                       colorscale = medcouple_colorscale(style$colours), zmin = -1, zmax = 1,
                       colorbar = list(title = "MC"),
                       hovertemplate = "%{y}<br>%{x:.3f} nm<br>MC = %{z:.3f}<extra></extra>")
  plotly::layout(p, title = plotly_title(style$title, style$subtitle),
                 xaxis = list(title = style$x_lab),
                 yaxis = list(title = style$y_lab, type = "category", autorange = "reversed"))
}
