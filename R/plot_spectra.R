#' @title Plotting of Spectra
#'
#' @description
#' Spectrum plots are commonly x–y plots in which the x-axis represents the
#' wavelength and the y-axis represents intensity of a spectrum's signal.
#' The function allows to plot a spectrum or several spectra in a single plot,
#' identified either by an id (for example, the samples or spectra id) or by a
#' target variable (for example, the concentration of a chemical element).
#' The spectra can be split into panels by a grouping variable, summarized by
#' their mean or median, and labeled with the emission lines they show.
#'
#' @author Christian L. Goueguel
#'
#' @details
#' This function is based on the ggplot2 package, thus allowing users to easily
#' add or modify different components of the plot. Each row of `x` is drawn as
#' a separate line. All columns other than `id`, `colvar` and `panel` must be
#' named by their wavelength.
#'
#' **Filled spectra.** With `color_as = "fill"`, the area between each
#' spectrum and its baseline (zero intensity, shifted by `offset`) is filled,
#' with the spectrum drawn as a thin dark line. Overlaid spectra are filled
#' with translucent colors. Stacked spectra (with a vertical `offset`) are
#' filled with opaque colors and drawn from the back to the front (from the
#' top to the bottom of the stack), so that the front spectra hide the back
#' ones, as in a waterfall plot.
#'
#' **Summaries.** With `summary = "mean"`, each group of spectra is drawn as
#' its mean spectrum, in a band of plus or minus one standard deviation; with
#' `summary = "median"`, as its median spectrum, in a band from the first to
#' the third quartile. The groups are the panels and, within them, the values
#' of `id`; the color of `colvar` is then the mean of `colvar` in each group.
#' The band is not drawn with `color_as = "fill"`.
#'
#' **Emission lines.** Each line of `lines` within the wavelength range is
#' marked by a dashed vertical line, labeled with its species and wavelength
#' above the plot (above the top panels). Lines closer than 2% of the
#' wavelength range share a label, such as "Ca II 393.37, 396.85". The
#' markers are at the tabulated wavelengths: with a horizontal `offset`, they
#' match the first spectrum of each panel. Over a wide range, keep the
#' strongest lines, for example with the `top` or `min_relative` arguments of
#' [libs_lines()], or labels become long.
#'
#' **Publication figures.** Journals print figures about 85 mm wide for a
#' single column and 175 mm for a double column, with text of 7 to 9 points.
#' Make the plot at its printed size, with `base_size` set to the text size,
#' and save it with [ggplot2::ggsave()], in a vector format (PDF or EPS) or
#' as a TIFF of 600 dpi, for example
#' `ggsave("figure.pdf", p, width = 85, height = 65, units = "mm")`. Many
#' long spectra make large and slow vector files: `rasterize = TRUE` draws
#' the spectra as an image within the plot, keeping the text, axes and
#' markers as vectors (this needs the ragg package).
#'
#' @param x data frame or tibble of the spectra.
#' @param id optional (`NULL` by default). Column name of a factor variable
#' that identified each spectrum.
#' @param colvar optional (`NULL` by default). Column name of a numeric
#' variable to be display in color scale.
#' @param .interactive optional (`FALSE` by default). When set to `TRUE`
#' enables interactive plot.
#' @param drop_na Optional (`FALSE` by default). Remove rows with NA intensity
#' if drop_na is `TRUE`.
#' @param offset Optional (`NULL` by default). The offsets between successive
#'   spectra, to separate them: a number (vertical offset, in intensity
#'   units), or two numbers `c(horizontal, vertical)` (in nm and intensity
#'   units). The spectrum of row `i` is shifted by `(i - 1)` times the
#'   offsets, so that the first one stays in place. With `panel`, the rows
#'   are counted in each panel, so that the first spectrum of every panel
#'   stays in place. With `summary`, the summary spectra are offset.
#' @param panel Optional (`NULL` by default). Column name of a grouping
#'   variable: the spectra of each group are drawn in a separate panel.
#' @param layout The arrangement of the panels: `"vertical"` (default), one
#'   above the other, or `"horizontal"`, side by side. Ignored without
#'   `panel`.
#' @param grid A logical: draw grid lines at the major breaks of the axes
#'   (`FALSE`, default).
#' @param color_as How the colors of `colvar` (or of `id`, without `colvar`)
#'   are shown: as the color of the lines (`"line"`, default), or as the fill
#'   of the area under each spectrum (`"fill"`).
#' @param legend The position of the legend of `colvar` (a color bar) or `id`:
#'   `"none"` (default), `"right"`, `"top"`, `"bottom"`, or `"inside"` (the
#'   top right corner of the plot).
#' @param legend_title The title of the legend. Default is the name of
#'   `colvar` (or `id`); give it with units, such as `"K (%)"`.
#' @param xlab,ylab The axis titles. Default are `"Wavelength (nm)"` and
#'   `"Intensity (arb. units)"`.
#' @param title The plot title. A long title is split into a title and a
#'   subtitle.
#' @param lines Optional table of emission lines, such as returned by
#'   [libs_lines()]: a data frame with the columns `species` (such as
#'   `"K I"`) and `wavelength` (nm). Each line within the wavelength range is
#'   marked and labeled (see Details).
#' @param palette The colors of `colvar` or `id`: the name of a viridis
#'   palette (`"viridis"`, default, `"magma"`, `"inferno"`, `"plasma"`,
#'   `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`), all readable by
#'   colorblind readers and in grayscale except `"turbo"`, or a vector of at
#'   least two colors, interpolated for `colvar` and for the levels of `id`
#'   when there are fewer colors than levels.
#' @param base_size The size of the text, in points. Default is 11; use the
#'   text size of the journal (often 7 to 9 points) for a figure saved at its
#'   printed size. The axis lines and labels scale with it.
#' @param linewidth The width of the spectra lines. Default is 0.5; about 0.3
#'   suits small figures.
#' @param xlim Optional wavelength range `c(from, to)`, in nm: only the
#'   channels in this range are drawn.
#' @param scales `"fixed"` (default), the same intensity axis for all panels,
#'   or `"free_y"`, an intensity axis fitted to each panel.
#' @param panel_tags A logical: tag the panels (a), (b), (c), ... before their
#'   labels (`FALSE`, default).
#' @param label_spectra A logical: label each spectrum at its right end
#'   (`FALSE`, default), with its `id`, else its value of `colvar`, else its
#'   row number (or `"Mean"`/`"Median"` for summaries). Most useful for
#'   spectra stacked by `offset`.
#' @param summary `"none"` (default), each spectrum drawn; `"mean"`, the mean
#'   spectrum of each group with a band of plus or minus one standard
#'   deviation; or `"median"`, the median spectrum of each group with a band
#'   of the first to third quartiles (see Details).
#' @param rasterize `FALSE` (default), `TRUE`, or a resolution in dpi: draw
#'   the spectra as an image, of 300 dpi with `TRUE`, within an otherwise
#'   vector plot (see Details). Ignored for interactive plots.
#'
#' @return Object of class ggplot or of class plotly if `.interactive = TRUE`.
#'
#' @seealso [libs_lines()] for the emission lines.
#'
#' @export plot_spectra
#'
#' @examples
#' data(forageLIBS)
#' wl <- as.numeric(names(forageLIBS)[-(1:14)])
#' k_lines <- names(forageLIBS)[-(1:14)][wl > 764 & wl < 772]
#' # the K I resonance lines, colored by potassium content
#' plot_spectra(forageLIBS[1:20, c("K", k_lines)], colvar = K, legend = "right",
#'              legend_title = "K (%)")
#'
#' # five spectra stacked, each shifted up by 20000 counts and right by 0.5 nm,
#' # labeled at their right end
#' plot_spectra(forageLIBS[1:5, c("Measurement", k_lines)], id = Measurement,
#'              offset = c(0.5, 20000), label_spectra = TRUE)
#'
#' # one panel per potassium level, side by side, with the area under the
#' # spectra filled by potassium content and grid lines
#' spectra <- forageLIBS[1:30, c("K", k_lines)]
#' spectra$level <- cut(spectra$K, 3, labels = c("Low K", "Medium K", "High K"))
#' plot_spectra(spectra, colvar = K, panel = level, layout = "horizontal",
#'              offset = c(0.2, 8000), color_as = "fill", grid = TRUE)
#'
#' # a figure for a journal column: the mean spectrum of each potassium level,
#' # with the K I lines labeled (from libs_lines() in practice)
#' lines <- data.frame(species = "K I", wavelength = c(766.49, 769.90))
#' p <- plot_spectra(spectra, colvar = K, panel = level, summary = "mean",
#'                   lines = lines, panel_tags = TRUE, legend = "right",
#'                   legend_title = "K (%)", ylab = "Intensity (counts)",
#'                   base_size = 8, linewidth = 0.3, xlim = c(765, 771))
#' p
#' # ggplot2::ggsave("figure.pdf", p, width = 85, height = 90, units = "mm")
plot_spectra <- function(x, id = NULL, colvar = NULL, .interactive = FALSE, drop_na = FALSE,
                         offset = NULL, panel = NULL, layout = c("vertical", "horizontal"),
                         grid = FALSE, color_as = c("line", "fill"),
                         legend = c("none", "right", "top", "bottom", "inside"),
                         legend_title = NULL, xlab = "Wavelength (nm)",
                         ylab = "Intensity (arb. units)", title = NULL, lines = NULL,
                         palette = "viridis", base_size = 11, linewidth = 0.5, xlim = NULL,
                         scales = c("fixed", "free_y"), panel_tags = FALSE,
                         label_spectra = FALSE, summary = c("none", "mean", "median"),
                         rasterize = FALSE) {
  if (missing(x)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(x)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  id_quo <- rlang::enquo(id)
  col_quo <- rlang::enquo(colvar)
  panel_quo <- rlang::enquo(panel)
  id_name <- if (rlang::quo_is_null(id_quo)) NULL else rlang::as_name(id_quo)
  col_name <- if (rlang::quo_is_null(col_quo)) NULL else rlang::as_name(col_quo)
  panel_name <- if (rlang::quo_is_null(panel_quo)) NULL else rlang::as_name(panel_quo)
  if (!is.null(id_name) && !id_name %in% colnames(x)) {
    stop("The 'id' column does not exist in the provided data.")
  }
  if (!is.null(col_name) && !col_name %in% colnames(x)) {
    stop("The 'colvar' column does not exist in the provided data.")
  }
  if (!is.null(panel_name) && !panel_name %in% colnames(x)) {
    stop("The 'panel' column does not exist in the provided data.")
  }
  if (!is.logical(.interactive)) {
    stop("The argument '.interactive' must be of type boolean (TRUE or FALSE)")
  }
  if (!is.logical(drop_na)) {
    stop("The argument 'drop_na' must be of type boolean (TRUE or FALSE)")
  }
  check_flag(grid, "grid")
  check_flag(panel_tags, "panel_tags")
  check_flag(label_spectra, "label_spectra")
  layout <- match.arg(layout)
  color_as <- match.arg(color_as)
  legend <- match.arg(legend)
  scales <- match.arg(scales)
  summary <- match.arg(summary)
  check_number(base_size, "base_size", lower = 0, lower_open = TRUE)
  check_number(linewidth, "linewidth", lower = 0, lower_open = TRUE)
  check_palette(palette)
  dpi <- raster_dpi(rasterize)
  if (!is.null(dpi) && !.interactive) {
    rlang::check_installed("ragg", reason = "to rasterize the spectra.")
  }
  if (!is.null(offset)) {
    if (!is.numeric(offset) || !length(offset) %in% 1:2 || anyNA(offset)) {
      stop("'offset' must be a number (vertical offset) or two numbers c(horizontal, vertical).",
           call. = FALSE)
    }
    offset <- if (length(offset) == 1) c(0, offset) else offset
  }
  if (!is.null(xlim) && (!is.numeric(xlim) || length(xlim) != 2 || anyNA(xlim) || xlim[1] >= xlim[2])) {
    stop("'xlim' must be two increasing wavelengths c(from, to).", call. = FALSE)
  }

  spec_cols <- setdiff(names(x), c(id_name, col_name, panel_name))
  wl <- parse_wavelength(spec_cols)
  if (anyNA(wl)) {
    stop("Spectral column names must be wavelengths; use 'id', 'colvar' and 'panel' for the other columns.")
  }
  if (!is.null(xlim)) {
    keep <- wl >= xlim[1] & wl <= xlim[2]
    if (!any(keep)) {
      stop("No wavelength of the spectra is within 'xlim'.", call. = FALSE)
    }
    spec_cols <- spec_cols[keep]
    wl <- wl[keep]
  }
  spectra <- as.matrix(x[spec_cols])
  if (!is.numeric(spectra)) {
    stop("The spectral columns must be numeric.", call. = FALSE)
  }
  marks <- if (is.null(lines)) NULL else line_marks(lines, wl)

  meta <- spectra_meta(x, id_name, col_name, panel_name, panel_tags)
  curves <- if (summary == "none") {
    list(meta = meta, center = spectra)
  } else {
    summarize_spectra(spectra, meta, id_name, col_name, summary)
  }
  fill <- color_as == "fill"
  long <- spectra_long(curves, wl, offset, fill)
  if (drop_na) {
    long <- long[!is.na(long$intensity), ]
  }

  intensity <- wavelength <- .draw <- .base <- .lower <- .upper <- NULL

  p <- ggplot2::ggplot(long) +
    ggplot2::aes(x = wavelength, y = intensity, group = .draw)

  if (!is.null(marks)) {
    p <- p + ggplot2::geom_vline(data = data.frame(wavelength = marks$at),
                                 ggplot2::aes(xintercept = .data$wavelength),
                                 linetype = "dashed", colour = "grey60", linewidth = 0.3)
  }
  colour_by <- if (!is.null(col_name)) {
    rlang::quo(.data[[!!col_name]])
  } else if (!is.null(id_name)) {
    rlang::quo(factor(.data[[!!id_name]]))
  }
  band <- summary != "none" && !fill
  layers <- list()
  if (band) {
    layers$band <- if (is.null(colour_by)) {
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .lower, ymax = .upper), fill = "#002a52",
                           alpha = 0.2, colour = NA)
    } else {
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .lower, ymax = .upper, fill = !!colour_by),
                           alpha = 0.25, colour = NA)
    }
  }
  if (fill) {
    # opaque fills when the spectra are stacked, translucent when they overlap
    stacked <- !is.null(offset) && offset[2] != 0
    alpha <- if (stacked) 1 else 0.6
    layers$spectra <- if (is.null(colour_by)) {
      # #667f97 is #002a52 at alpha 0.6 on white
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .base, ymax = intensity),
                           fill = if (stacked) "#667f97" else "#002a52", alpha = alpha,
                           colour = "#002a52", linewidth = 0.6 * linewidth, outline.type = "upper")
    } else {
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .base, ymax = intensity, fill = !!colour_by),
                           alpha = alpha, colour = "grey20", linewidth = 0.6 * linewidth,
                           outline.type = "upper")
    }
  } else {
    layers$spectra <- if (is.null(colour_by)) {
      ggplot2::geom_line(color = "#002a52", linewidth = linewidth)
    } else {
      ggplot2::geom_line(ggplot2::aes(colour = !!colour_by), linewidth = linewidth)
    }
  }
  if (!is.null(dpi) && !.interactive) {
    layers <- lapply(layers, rasterize_layer, dpi = dpi)
  }
  p <- p + layers

  if (!is.null(colour_by)) {
    aesthetics <- c(if (!fill) "colour", if (fill || band) "fill")
    n_levels <- if (is.null(col_name)) length(unique(stats::na.omit(curves$meta[[id_name]]))) else 0
    p <- p + palette_scale(palette, discrete = is.null(col_name), aesthetics = aesthetics,
                           name = legend_title %||% col_name %||% id_name, n = n_levels)
  }
  if (label_spectra) {
    ends <- spectra_ends(long, curves$meta, id_name, col_name, summary)
    p <- p +
      ggplot2::geom_text(data = ends, ggplot2::aes(x = .data$wavelength, y = .data$intensity,
                                                   label = .data$.label),
                         hjust = 0, nudge_x = 0.01 * diff(range(long$wavelength)),
                         size = 0.8 * base_size / ggplot2::.pt, colour = "grey20",
                         inherit.aes = FALSE) +
      ggplot2::coord_cartesian(clip = "off")
  }
  # labels at the right end need room; the emission lines are labeled on a
  # top axis, above the spectra
  x_scale <- list()
  if (label_spectra) {
    x_scale$expand <- ggplot2::expansion(mult = c(0.05, 0.05 + 0.012 * max(0, nchar(ends$.label))))
  }
  if (!is.null(marks)) {
    x_scale$sec.axis <- ggplot2::dup_axis(name = NULL, breaks = marks$labels$wavelength,
                                          labels = marks$labels$label)
  }
  if (length(x_scale) > 0) {
    p <- p + do.call(ggplot2::scale_x_continuous, x_scale)
  }
  p <- p + ggplot2::scale_y_continuous(labels = plain_numbers)

  p <- p +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "#4b4b4b", linewidth = base_size / 16),
      axis.ticks = ggplot2::element_line(color = "#4b4b4b"),
      axis.line.x.top = ggplot2::element_blank(),
      axis.ticks.x.top = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5)
    ) +
    legend_theme(legend)

  if (!is.null(panel_name)) {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(.data$.panel),
                          ncol = if (layout == "vertical") 1 else NULL,
                          nrow = if (layout == "horizontal") 1 else NULL,
                          scales = scales, axes = "all", axis.labels = "margins") +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
                     strip.text = ggplot2::element_text(hjust = 0))
  }
  if (grid) {
    p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.3))
  }
  p <- finish_title(p)

  if (.interactive == FALSE) {
    return(p)
  }
  rlang::check_installed("plotly", reason = "to create interactive plots.")
  out <- plotly::ggplotly(p, tooltip = "all")
  if (!is.null(title)) {
    out <- plotly::layout(out, title = plotly_title(title))
  }
  out
}

# ---- internals ---------------------------------------------------------------

# The variables of each spectrum: `id` and `colvar` under their names, and the
# panel as the factor `.panel` (its levels tagged (a), (b), ... on request).
spectra_meta <- function(x, id_name, col_name, panel_name, panel_tags) {
  meta <- data.frame(.row = seq_len(nrow(x)))
  for (nm in unique(c(id_name, col_name))) {
    meta[[nm]] <- x[[nm]]
  }
  if (!is.null(panel_name)) {
    panel <- x[[panel_name]]
    panel <- if (is.factor(panel)) droplevels(panel) else factor(panel)
    if (panel_tags) {
      k <- nlevels(panel)
      tags <- if (k <= 26) letters[seq_len(k)] else seq_len(k)
      levels(panel) <- paste0("(", tags, ") ", levels(panel))
    }
    meta$.panel <- panel
  }
  meta
}

# The mean (with +/- one SD) or median (with the quartiles) spectrum of each
# group of rows sharing a panel and an id; `colvar` is averaged per group.
summarize_spectra <- function(spectra, meta, id_name, col_name, summary) {
  keys <- intersect(c(".panel", id_name), names(meta))
  key <- if (length(keys) == 0) {
    rep("all", nrow(meta))
  } else {
    do.call(paste, c(lapply(meta[keys], as.character), sep = "\r"))
  }
  groups <- split(seq_len(nrow(meta)), factor(key, levels = unique(key)))
  bands <- lapply(groups, function(rows) summary_band(spectra[rows, , drop = FALSE], summary))
  part <- function(name) do.call(rbind, lapply(bands, `[[`, name))
  out <- meta[vapply(groups, `[`, integer(1), 1), , drop = FALSE]
  if (!is.null(col_name)) {
    out[[col_name]] <- vapply(groups, function(rows) mean(meta[[col_name]][rows], na.rm = TRUE),
                              numeric(1))
  }
  list(meta = out, center = part("center"), lower = part("lower"), upper = part("upper"))
}

summary_band <- function(s, summary) {
  if (summary == "mean") {
    center <- colMeans(s, na.rm = TRUE)
    n <- colSums(!is.na(s))
    sd <- sqrt(colSums(sweep(s, 2, center)^2, na.rm = TRUE) / (n - 1))
    sd[n < 2] <- NA
    list(center = center, lower = center - sd, upper = center + sd)
  } else {
    q <- apply(s, 2, stats::quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
    list(center = q[2, ], lower = q[1, ], upper = q[3, ])
  }
}

# One row per curve and wavelength, offset by the position of the curve in
# its panel. `.draw` is the drawing order: filled curves are drawn from the
# back (the top of a stack) to the front.
spectra_long <- function(curves, wl, offset, fill) {
  meta <- curves$meta
  n <- nrow(meta)
  position <- if (is.null(meta$.panel)) {
    seq_len(n)
  } else {
    stats::ave(seq_len(n), match(meta$.panel, unique(meta$.panel)), FUN = seq_along)
  }
  index <- rep(seq_len(n), times = length(wl))
  long <- meta[index, , drop = FALSE]
  rownames(long) <- NULL
  long$.curve <- index
  long$.draw <- if (fill && (is.null(offset) || offset[2] >= 0)) n + 1 - index else index
  long$wavelength <- rep(wl, each = n)
  long$intensity <- as.vector(curves$center)
  if (!is.null(curves$lower)) {
    long$.lower <- as.vector(curves$lower)
    long$.upper <- as.vector(curves$upper)
  }
  long$.base <- 0
  if (!is.null(offset)) {
    shift <- position[index] - 1
    long$wavelength <- long$wavelength + shift * offset[1]
    long$.base <- shift * offset[2]
    long$intensity <- long$intensity + long$.base
    if (!is.null(curves$lower)) {
      long$.lower <- long$.lower + long$.base
      long$.upper <- long$.upper + long$.base
    }
  }
  long
}

# The right end of each curve, with its label: the id, else the value of
# colvar, else the row number (or the name of the summary).
spectra_ends <- function(long, meta, id_name, col_name, summary) {
  long <- long[!is.na(long$intensity), ]
  last <- vapply(split(seq_len(nrow(long)), long$.curve),
                 function(rows) rows[which.max(long$wavelength[rows])], integer(1))
  ends <- long[last, , drop = FALSE]
  ends$.label <- if (!is.null(id_name)) {
    as.character(ends[[id_name]])
  } else if (!is.null(col_name)) {
    vapply(ends[[col_name]], format, character(1), digits = 3)
  } else if (summary != "none") {
    c(mean = "Mean", median = "Median")[[summary]]
  } else {
    as.character(ends$.curve)
  }
  ends
}

# The emission lines within the wavelength range: their wavelengths (`at`),
# and their labels. Lines closer than 2% of the range share one label, at
# their mean wavelength, such as "Ca II 393.37, 396.85".
line_marks <- function(lines, wl) {
  if (!is.data.frame(lines) || !all(c("species", "wavelength") %in% names(lines)) ||
      !is.numeric(lines$wavelength)) {
    stop("'lines' must be a data frame with the columns 'species' and 'wavelength', ",
         "such as a table returned by libs_lines().", call. = FALSE)
  }
  inside <- !is.na(lines$wavelength) & lines$wavelength >= min(wl) & lines$wavelength <= max(wl)
  if (!any(inside)) {
    warning("None of 'lines' is within the wavelength range of the spectra.", call. = FALSE)
    return(NULL)
  }
  lines <- unique(data.frame(species = as.character(lines$species[inside]),
                             wavelength = lines$wavelength[inside]))
  lines <- lines[order(lines$wavelength), ]
  gap <- 0.02 * diff(range(wl))
  group <- cumsum(c(TRUE, diff(lines$wavelength) >= gap))
  labels <- lapply(split(lines, group), function(g) {
    species <- unique(g$species)
    text <- vapply(species, function(s) {
      paste(s, paste(format_wavelength(g$wavelength[g$species == s], 2), collapse = ", "))
    }, character(1))
    data.frame(wavelength = mean(g$wavelength), label = paste(text, collapse = ", "))
  })
  list(at = lines$wavelength, labels = do.call(rbind, labels))
}

viridis_palettes <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")

check_palette <- function(palette) {
  is_colour <- function(x) {
    vapply(x, function(col) !inherits(try(grDevices::col2rgb(col), silent = TRUE), "try-error"),
           logical(1))
  }
  ok <- is.character(palette) && length(palette) >= 1 && !anyNA(palette) &&
    ((length(palette) == 1 && palette %in% viridis_palettes) ||
       (length(palette) >= 2 && all(is_colour(palette))))
  if (!ok) {
    stop("'palette' must be the name of a viridis palette (",
         paste0("\"", viridis_palettes, "\"", collapse = ", "), ") or at least two colors.",
         call. = FALSE)
  }
  invisible(palette)
}

# The color scale of colvar (continuous) or id (discrete with n levels). The
# lightest end of the viridis palettes is left out, too pale on white.
palette_scale <- function(palette, discrete, aesthetics, name, n) {
  if (length(palette) == 1) {
    if (discrete) {
      ggplot2::scale_colour_viridis_d(name = name, option = palette, end = 0.9, direction = -1,
                                      aesthetics = aesthetics)
    } else {
      ggplot2::scale_colour_viridis_c(name = name, option = palette, end = 0.9,
                                      aesthetics = aesthetics)
    }
  } else if (discrete) {
    values <- if (length(palette) >= n) palette else grDevices::colorRampPalette(palette)(n)
    ggplot2::scale_colour_manual(name = name, values = values, aesthetics = aesthetics)
  } else {
    ggplot2::scale_colour_gradientn(name = name, colours = palette, aesthetics = aesthetics)
  }
}

legend_theme <- function(legend) {
  if (legend == "inside") {
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.99, 0.99),
                   legend.justification.inside = c(1, 1),
                   legend.background = ggplot2::element_rect(fill = "white", colour = NA))
  } else {
    ggplot2::theme(legend.position = legend)
  }
}

# Axis numbers in plain notation, with thousands separators ("100,000" rather
# than "1e+05").
plain_numbers <- function(x) {
  out <- format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  out[is.na(x)] <- NA
  out
}

raster_dpi <- function(rasterize) {
  if (isFALSE(rasterize)) {
    return(NULL)
  }
  if (isTRUE(rasterize)) {
    return(300)
  }
  if (!is.numeric(rasterize) || length(rasterize) != 1 || is.na(rasterize) || rasterize <= 0) {
    stop("'rasterize' must be TRUE, FALSE or a resolution in dpi.", call. = FALSE)
  }
  rasterize
}

# A layer whose grobs are drawn as an image, at the size of the panel and the
# resolution `dpi`, when the plot is rendered (as by the ggrastr package).
rasterize_layer <- function(layer, dpi) {
  geom <- layer$geom
  ggplot2::ggproto(NULL, layer, geom = ggplot2::ggproto(NULL, geom,
    draw_layer = function(self, data, params, layout, coord) {
      grobs <- ggplot2::ggproto_parent(geom, self)$draw_layer(data, params, layout, coord)
      lapply(grobs, function(g) grid::gTree(children = grid::gList(g), dpi = dpi,
                                            cl = "specproc_raster"))
    }
  ))
}

#' @exportS3Method grid::makeContext
#' @noRd
makeContext.specproc_raster <- function(x) {
  width <- grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  height <- grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  if (width <= 0 || height <= 0) {
    return(grid::nullGrob())
  }
  current <- grDevices::dev.cur()
  capture <- ragg::agg_capture(width = width, height = height, units = "in", res = x$dpi,
                               background = "transparent")
  image <- tryCatch({
    # the grobs are in native units, of the 0-1 scales of a new viewport (the
    # root viewport of a device has other scales)
    grid::pushViewport(grid::viewport())
    grid::grid.draw(x$children)
    grid::popViewport()
    capture(native = TRUE)
  }, finally = {
    grDevices::dev.off()
    grDevices::dev.set(current)
  })
  grid::rasterGrob(image, width = grid::unit(width, "in"), height = grid::unit(height, "in"))
}
