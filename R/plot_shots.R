#' @title Plots of the Laser Shots and of their Rejection
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Shows the shots of a LIBS data set and the shots rejected by
#' [reject_shots()]: their criteria, their spectra, the trends along the
#' shots, and the samples, to judge the rejection before averaging the
#' shots.
#'
#' @details
#' Five views of the result of [reject_shots()]:
#'  - `"heatmap"` (default): a map of the samples (rows) by the shot number
#'    (columns), one panel per criterion, colored by the robust z-score of
#'    each shot: white below 1 (the ordinary variability of the shots), then
#'    in steps up to the cutoff and beyond it, the rejected shots outlined.
#'    It shows at a glance the rejected shots, the samples whose shots vary
#'    (rows), consecutive rejected shots (a problem of focus or surface
#'    rather than a misfire), and the trends along the shot number
#'    (columns). With `acquisition`, the rows follow the order of the
#'    measurements; `arrange = "extreme"` puts the samples with the most
#'    extreme shots first. With `smooth = k` (and `acquisition`), each cell
#'    is the running median of the z-scores of that shot number over `k`
#'    consecutive measurements, the rejected shots as points: the drift of
#'    the shots along the acquisition (laser energy, fouling of the optics),
#'    when the trend along the shot number changes from one period to
#'    another. Without the acquisition order, neighboring rows are unrelated
#'    samples, and smoothing them would invent patterns.
#'  - `"criteria"`: the robust z-scores of the shots (total intensity against
#'    the dissimilarity of shape, or the criteria used), with the cutoff:
#'    the rejected shots are colored by their criteria, and the most extreme
#'    labeled (sample and shot). Shots just beyond the cutoff are
#'    borderline; a gap between the rejected shots and the others shows
#'    clear outliers.
#'  - `"spectra"`: the shots of one sample (`sample`, by default the sample
#'    with the most rejected shots): the kept shots in grey, the median
#'    spectrum in black, and each rejected shot in color, with its criteria,
#'    its total intensity relative to the median shot and its correlation
#'    with the median spectrum. `wavelength` zooms on a range.
#'  - `"order"`: along the shot number, the total intensity of the shots
#'    relative to their sample, their dissimilarity of shape to the median
#'    spectrum of their sample (\eqn{1 - r}, on a log scale) and the share
#'    of rejected shots. A trend shows the first shots on the surface of the
#'    sample (cleaning shots) or the drift of the ablation in its crater,
#'    which the rejection, made within each sample, does not correct:
#'    discard the first shots, or check that the trend is the same for all
#'    samples.
#'  - `"samples"`: the number of samples by their number of rejected shots,
#'    and the relative standard deviation (RSD) of the total intensity of
#'    the shots of each sample, with all its shots and with the kept ones:
#'    the gain of precision of the rejection, and the samples whose shots
#'    vary most (heterogeneous samples, or a poor laser focus).
#'
#' The shot numbers are the `.shot` column of [reject_shots()] (its `shot`
#' argument, or the order of the rows in each sample).
#'
#' @param x The result of [reject_shots()] (with `drop = FALSE`).
#' @param type The view: `"heatmap"` (default), `"criteria"`, `"spectra"`,
#'   `"order"` or `"samples"` (see Details).
#' @param sample For `type = "spectra"`, the sample to draw: a value of the
#'   sample column. Default is the sample with the most rejected shots.
#' @param wavelength For `type = "spectra"`, an optional wavelength range
#'   (nm) to zoom on.
#' @param label For `type = "criteria"`, the number of rejected shots
#'   labeled, the most extreme first. Default is 10; 0 for none.
#' @param acquisition For `type = "heatmap"`, an optional column of `x`
#'   (unquoted or as a string) giving the order of the measurements, such as
#'   the measurement number or the acquisition time: the rows follow it.
#' @param arrange For `type = "heatmap"` without `acquisition`, the order of
#'   the rows: `"data"` (default), that of the samples in `x`, or
#'   `"extreme"`, the samples with the most extreme shots first.
#' @param smooth For `type = "heatmap"`, an odd number `k` of consecutive
#'   measurements: each cell is the running median of the z-scores of that
#'   shot number over them. Needs `acquisition`. Default is `NULL`, no
#'   smoothing.
#' @param plot A logical: plot (`TRUE`, default) or return the table of the
#'   view (the shots with their z-scores, smoothed or not, the shots of the
#'   sample, the shot numbers or the samples).
#' @param title The plot title. A long title is split into a title and a
#'   subtitle.
#' @param caption `TRUE` (default), a caption saying how to read the plot,
#'   with the number of rejected shots; `FALSE`, no caption; or a caption of
#'   your own.
#' @param base_size The size of the text, in points. Default is 11.
#'
#' @return A ggplot (or patchwork) object, or with `plot = FALSE` a tibble.
#'
#' @seealso [reject_shots()], [forageShots]
#' @export plot_shots
#'
#' @examples
#' data(forageShots)
#' res <- reject_shots(forageShots, Measurement, shot = shot)
#' if (rlang::is_installed("patchwork")) {
#'   # the samples by the shot number, in the order of the measurements
#'   plot_shots(res, acquisition = Measurement)
#'   # the drift of the shots along the acquisition: running medians over 5
#'   # consecutive measurements
#'   plot_shots(res, acquisition = Measurement, smooth = 5)
#' }
#' # the criteria of the shots, the rejected ones labeled
#' plot_shots(res, type = "criteria")
#' # the shots of the sample with the most rejected shots
#' plot_shots(res, type = "spectra")
#' # a zoom on the K I lines
#' plot_shots(res, type = "spectra", sample = 121306, wavelength = c(764, 772))
#' \donttest{
#' if (rlang::is_installed("patchwork")) {
#'   # the trends along the shots, and the precision of the samples
#'   plot_shots(res, type = "order")
#'   plot_shots(res, type = "samples")
#' }
#' }
#' # the table of the samples
#' plot_shots(res, type = "samples", plot = FALSE)
plot_shots <- function(x, type = c("heatmap", "criteria", "spectra", "order", "samples"),
                       sample = NULL, wavelength = NULL, label = 10, acquisition = NULL,
                       arrange = c("data", "extreme"), smooth = NULL, plot = TRUE, title = NULL,
                       caption = TRUE, base_size = 11) {
  settings <- attr(x, "reject_shots")
  if (!is.data.frame(x) || is.null(settings) ||
      !all(c(".shot", ".rejected", ".intensity", ".correlation") %in% names(x))) {
    stop("'x' must be the result of reject_shots() with drop = FALSE (not subset since).",
         call. = FALSE)
  }
  type <- match.arg(type)
  arrange <- match.arg(arrange)
  acquisition_quo <- rlang::enquo(acquisition)
  acquisition <- if (rlang::quo_is_null(acquisition_quo)) NULL else rlang::as_name(acquisition_quo)
  if (!is.null(acquisition) && !acquisition %in% names(x)) {
    stop("Column `", acquisition, "` not found in 'x'.", call. = FALSE)
  }
  if (!is.null(smooth)) {
    smooth <- check_count(smooth, "smooth", lower = 3)
    if (smooth %% 2 == 0) {
      stop("'smooth' must be odd: the running median is centered on each sample.", call. = FALSE)
    }
    if (is.null(acquisition)) {
      stop("'smooth' needs `acquisition`, the order of the measurements: neighboring rows ",
           "are otherwise unrelated samples.", call. = FALSE)
    }
  }
  check_flag(plot, "plot")
  label <- check_count(label, "label", lower = 0)
  check_number(base_size, "base_size", lower = 0, lower_open = TRUE)
  if (!(isTRUE(caption) || isFALSE(caption) ||
        (is.character(caption) && length(caption) == 1 && !is.na(caption)))) {
    stop("'caption' must be TRUE, FALSE or a character string.", call. = FALSE)
  }
  if (!is.null(wavelength)) check_wavelength_range(wavelength)
  x <- tibble::as_tibble(x)
  x$.sample <- x[[settings$sample]]
  view <- switch(type,
                 heatmap = shots_heatmap(x, settings, acquisition, arrange, smooth, base_size),
                 criteria = shots_criteria(x, settings, label, base_size),
                 spectra = shots_spectra(x, settings, sample, wavelength, base_size),
                 order = shots_order(x, settings, base_size),
                 samples = shots_samples(x, settings, base_size))
  if (!plot) {
    return(view$table)
  }
  caption_text <- if (isTRUE(caption)) view$caption else if (is.character(caption)) caption
  title <- title %||% view$title
  if (inherits(view$plot, "patchwork")) {
    heading <- split_title(title)
    return(view$plot + patchwork::plot_annotation(
      title = heading$title, subtitle = heading$subtitle, caption = caption_text,
      theme = bold_title() + ggplot2::theme(
        plot.title = ggplot2::element_text(size = 1.2 * base_size),
        plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30", size = 0.8 * base_size)
      )
    ))
  }
  p <- view$plot +
    ggplot2::labs(title = title, caption = caption_text) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30",
                                                        size = ggplot2::rel(0.8)),
                   plot.caption.position = "plot")
  finish_title(p)
}

# ---- internals ---------------------------------------------------------------

unname_vapply <- function(...) unname(vapply(...))

shot_palette <- c("#d95f02", "#7570b3", "#1b9e77", "#e7298a", "#66a61e", "#e6ab02")

shots_theme <- function(base_size) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "#4b4b4b", linewidth = base_size / 16),
      axis.ticks = ggplot2::element_line(colour = "#4b4b4b"),
      legend.position = "bottom"
    )
}

shot_label <- function(x) paste0(x$.sample, " #", x$.shot)

shots_count <- function(x) {
  n <- nrow(x)
  k <- sum(x$.rejected)
  samples <- length(unique(x$.sample))
  hit <- length(unique(x$.sample[x$.rejected]))
  sprintf("Rejected: %d of %d shots (%.1f%%), in %d of %d samples.", k, n, 100 * k / n, hit,
          samples)
}

shots_scale_text <- function(settings) {
  switch(settings$scale,
         floor = "the MAD of the sample, not less than the pooled MAD of all the samples",
         sample = "the MAD of the sample",
         pooled = "the pooled MAD of all the samples")
}

# The samples by the shot number, colored by the robust z-scores (binned at
# 1, 2 and the cutoff), or by their running median along the acquisition.
shots_heatmap <- function(x, settings, acquisition, arrange, smooth, base_size) {
  rlang::check_installed("patchwork", reason = "to combine the panels of the criteria.")
  methods <- intersect(c("intensity", "correlation", "distance"), settings$method)
  cutoff <- settings$cutoff
  samples <- unique(as.character(x$.sample))
  z <- as.matrix(as.data.frame(x[paste0(".", methods, "_z")]))
  colnames(z) <- methods
  if (!is.null(acquisition)) {
    key <- vapply(split(x[[acquisition]], factor(as.character(x$.sample), levels = samples)),
                  function(v) as.numeric(v[1]), numeric(1))
    samples <- samples[order(key)]
  } else if (arrange == "extreme") {
    extreme <- vapply(split(apply(abs(z), 1, function(v) max(c(v, 0), na.rm = TRUE)),
                            factor(as.character(x$.sample), levels = samples)), max, numeric(1))
    samples <- samples[order(-extreme)]
  }
  shots <- sort(unique(x$.shot))
  tbl <- tibble::tibble(sample = as.character(x$.sample), shot = x$.shot)
  if (!is.null(acquisition) && acquisition != settings$sample) {
    tbl[[acquisition]] <- x[[acquisition]]
  }
  for (m in methods) tbl[[paste0(m, "_z")]] <- z[, m]
  tbl$rejected <- x$.rejected
  rank <- match(tbl$sample, samples)
  if (!is.null(smooth)) {
    k <- min(smooth, length(samples) - (length(samples) + 1) %% 2)
    for (m in methods) {
      smoothed <- rep(NA_real_, nrow(tbl))
      for (s in shots) {
        r <- which(tbl$shot == s & !is.na(z[, m]))
        r <- r[order(rank[r])]
        if (length(r) >= 3) {
          smoothed[r] <- stats::runmed(z[r, m], max(1, min(k, length(r) - (length(r) + 1) %% 2)),
                                       endrule = "median")
        }
      }
      tbl[[paste0(m, "_smoothed")]] <- smoothed
    }
  }
  tbl <- tbl[order(rank, tbl$shot), ]
  d <- tbl
  d$.row <- factor(d$sample, levels = rev(samples))
  d$.column <- factor(d$shot, levels = shots)
  many <- length(samples) > 60
  size <- base_size / 11
  bins <- function(v, signed) {
    breaks <- if (signed) {
      c(-Inf, -cutoff, -2, -1, 1, 2, cutoff, Inf)
    } else {
      c(-Inf, 1, 2, cutoff, Inf)
    }
    labels <- if (signed) {
      c(paste("<", -cutoff), paste(-cutoff, "to -2"), "-2 to -1", "-1 to 1", "1 to 2",
        paste("2 to", cutoff), paste(">", cutoff))
    } else {
      c("< 1", "1 to 2", paste("2 to", cutoff), paste(">", cutoff))
    }
    factor(cut(v, breaks, labels = labels), levels = labels)
  }
  colours <- list(
    intensity = c("#2166ac", "#67a9cf", "#d1e5f0", "white", "#fddbc7", "#ef8a62", "#b2182b"),
    correlation = c("white", "#e7d4e8", "#af8dc3", "#762a83"),
    distance = c("white", "#fee391", "#fe9929", "#cc4c02")
  )
  titles <- c(intensity = "Total intensity", correlation = "Dissimilarity of shape",
              distance = "Distance to the median spectrum")
  .column <- .row <- .fill <- NULL
  panel <- function(m) {
    signed <- m == "intensity"
    if (is.null(smooth)) {
      d$.fill <- bins(d[[paste0(m, "_z")]], signed)
      palette <- stats::setNames(colours[[m]], levels(d$.fill))
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .column, y = .row)) +
        ggplot2::geom_tile(ggplot2::aes(fill = .fill), colour = if (many) NA else "grey92",
                           linewidth = 0.2) +
        ggplot2::scale_fill_manual(values = palette, na.value = "grey85", drop = FALSE,
                                   name = "Robust z") +
        ggplot2::geom_tile(data = d[d$rejected, , drop = FALSE], fill = NA, colour = "black",
                           linewidth = 0.6 * size)
    } else {
      d$.fill <- d[[paste0(m, "_smoothed")]]
      limit <- max(1, stats::quantile(abs(d$.fill), 0.98, na.rm = TRUE))
      ends <- if (signed) c("#2166ac", "#b2182b") else c("#1b7837", colours[[m]][4])
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .column, y = .row)) +
        ggplot2::geom_tile(ggplot2::aes(fill = .fill)) +
        ggplot2::scale_fill_gradient2(low = ends[1], mid = "white", high = ends[2],
                                      limits = c(-limit, limit), oob = scales_squish,
                                      na.value = "grey85", name = "Running\nmedian z") +
        ggplot2::geom_point(data = d[d$rejected, , drop = FALSE], size = 1.2 * size,
                            colour = "black")
    }
    p +
      ggplot2::labs(x = "Shot number", y = NULL, title = titles[[m]]) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        legend.key = ggplot2::element_rect(colour = "grey75", fill = NA, linewidth = 0.2),
        panel.grid = ggplot2::element_blank(),
        axis.text.y = if (many) {
          ggplot2::element_blank()
        } else {
          ggplot2::element_text(size = ggplot2::rel(0.7))
        },
        plot.title = ggplot2::element_text(face = "plain", size = ggplot2::rel(0.9)),
        legend.position = "bottom", legend.direction = "horizontal"
      ) +
      if (is.null(smooth)) {
        # the colors of all the classes, those without shots too
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = if (signed) 2 else 1, byrow = TRUE,
                                                     title.position = "top",
                                                     override.aes = list(fill = colours[[m]])))
      } else {
        ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top"))
      }
  }
  plots <- lapply(methods, panel)
  plots[[1]] <- plots[[1]] + ggplot2::labs(y = if (many) {
    paste0("Samples (", length(samples), if (!is.null(acquisition)) ", in acquisition order", ")")
  } else if (!is.null(acquisition)) {
    "Samples, in acquisition order"
  } else {
    "Samples"
  })
  p <- patchwork::wrap_plots(plots, nrow = 1)
  rows <- if (!is.null(acquisition)) {
    paste0("in the order of ", acquisition)
  } else if (arrange == "extreme") {
    "the samples with the most extreme shots first"
  } else {
    "in the order of the data"
  }
  caption <- if (is.null(smooth)) {
    paste0(
      "Rows: the samples (", rows, "); columns: their shots. Robust z-scores of the shots ",
      "within their sample (", shots_scale_text(settings), "): white below 1, then in steps to ",
      "the cutoff, ", format(cutoff), "; outlined: the rejected shots. ", shots_count(x)
    )
  } else {
    paste0(
      "Rows: the samples (", rows, "); columns: their shots. Each cell is the running median ",
      "of the robust z-scores of that shot number over ", k, " consecutive measurements: the ",
      "drift of the shots along the acquisition. Points: the rejected shots. ", shots_count(x)
    )
  }
  tbl <- tibble::as_tibble(tbl)
  # the sample column with its own values and type
  tbl$sample <- x[[settings$sample]][match(tbl$sample, as.character(x$.sample))]
  names(tbl)[1] <- settings$sample
  list(plot = p, table = tbl, caption = paste(strwrap(caption, width = 90), collapse = "\n"),
       title = if (is.null(smooth)) "Robust z-scores of the shots" else
         "Drift of the shots along the acquisition")
}

# squish values out of the limits onto them (as scales::squish)
scales_squish <- function(x, range = c(0, 1), only.finite = TRUE) {
  finite <- if (only.finite) is.finite(x) else rep(TRUE, length(x))
  x[finite & x < range[1]] <- range[1]
  x[finite & x > range[2]] <- range[2]
  x
}

criterion_titles <- c(intensity = "Total intensity (robust z)",
                      correlation = "Dissimilarity of shape (robust z)",
                      distance = "Distance to the median spectrum (robust z)")

# The robust z-scores of the shots, with the cutoff.
shots_criteria <- function(x, settings, label, base_size) {
  methods <- intersect(c("intensity", "correlation", "distance"), settings$method)
  z <- as.data.frame(x[paste0(".", methods, "_z")])
  names(z) <- methods
  status <- ifelse(x$.rejected, x$.reason, "kept")
  status <- paste0(toupper(substring(status, 1, 1)), substring(status, 2))
  levels <- c("Kept", setdiff(unique(status), "Kept"))
  df <- data.frame(.sample = x$.sample, .shot = x$.shot, status = factor(status, levels = levels),
                   z, check.names = FALSE)
  df$.extreme <- apply(abs(as.matrix(z)), 1, function(v) max(v, na.rm = TRUE))
  df <- df[order(df$status != "Kept"), ] # rejected shots on top
  cutoff <- settings$cutoff
  colours <- c(Kept = "grey70", stats::setNames(
    shot_palette[seq_len(length(levels) - 1)],
    levels[-1]
  ))
  if (length(methods) >= 2) {
    xm <- methods[1]
    ym <- methods[2]
    df$.x <- df[[xm]]
    df$.y <- df[[ym]]
    x_title <- criterion_titles[[xm]]
    y_title <- criterion_titles[[ym]]
  } else {
    df$.x <- match(df$.sample, unique(x$.sample))
    df$.y <- df[[methods]]
    x_title <- "Sample (in the order of the data)"
    y_title <- criterion_titles[[methods]]
    xm <- NULL
    ym <- methods
  }
  .x <- .y <- status <- NULL
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .x, y = .y))
  cut_lines <- function(m, axis) {
    at <- if (m == "intensity") c(-cutoff, cutoff) else cutoff
    if (axis == "x") {
      ggplot2::geom_vline(xintercept = at, linetype = "dashed", colour = "grey45", linewidth = 0.4)
    } else {
      ggplot2::geom_hline(yintercept = at, linetype = "dashed", colour = "grey45", linewidth = 0.4)
    }
  }
  if (!is.null(xm)) p <- p + cut_lines(xm, "x")
  p <- p + cut_lines(ym, "y") +
    ggplot2::geom_point(ggplot2::aes(colour = status), size = 1.6 * base_size / 11, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = colours, name = NULL)
  if (label > 0 && any(x$.rejected)) {
    top <- df[df$status != "Kept", ]
    top <- top[order(-top$.extreme), ][seq_len(min(label, nrow(top))), ]
    top$.label <- shot_label(top)
    p <- p + ggplot2::geom_text(data = top, ggplot2::aes(label = .data$.label), vjust = -0.8,
                                size = 0.7 * base_size / ggplot2::.pt, colour = "grey20")
  }
  p <- p +
    ggplot2::scale_x_continuous(labels = plain_numbers) +
    ggplot2::scale_y_continuous(labels = plain_numbers,
                                expand = ggplot2::expansion(mult = c(0.05, 0.1))) +
    ggplot2::labs(x = x_title, y = y_title) +
    shots_theme(base_size)
  caption <- paste(
    "Each point is a shot: its robust z-scores within its sample (scale:",
    paste0(shots_scale_text(settings), ";"),
    if ("correlation" %in% methods) {
      "shape: Fisher's z of the correlation with the median spectrum, positive when lower;"
    },
    paste0("intensity on a log scale). Dashed: the cutoff, ", format(cutoff), "."),
    shots_count(x)
  )
  tbl <- tibble::as_tibble(x[c(settings$sample, ".shot", ".rejected", ".reason", ".intensity",
                                 ".correlation", ".distance", paste0(".", methods, "_z"))])
  list(plot = p, table = tbl, caption = paste(strwrap(caption, width = 90), collapse = "\n"),
       title = "Criteria of the laser shots")
}

# The shots of one sample, the rejected ones in color.
shots_spectra <- function(x, settings, sample, wavelength, base_size) {
  if (is.null(sample)) {
    counts <- tapply(x$.rejected, factor(x$.sample, levels = unique(x$.sample)), sum)
    sample <- names(counts)[which.max(counts)]
  }
  rows <- which(as.character(x$.sample) == as.character(sample))
  if (length(rows) == 0) {
    stop("No shots of sample ", sample, " in 'x'.", call. = FALSE)
  }
  shots <- x[rows, ]
  wl <- suppressWarnings(as.numeric(names(x)))
  spectral <- which(!is.na(wl) & vapply(x, is.numeric, logical(1)))
  if (!is.null(wavelength)) {
    spectral <- spectral[wl[spectral] >= wavelength[1] & wl[spectral] <= wavelength[2]]
  }
  if (length(spectral) < 2) {
    stop("No spectral columns", if (!is.null(wavelength)) " in the wavelength range", ".",
         call. = FALSE)
  }
  wl <- wl[spectral]
  m <- as.matrix(shots[spectral])
  storage.mode(m) <- "double"
  segment <- spectra_segments(wl)
  # separate windows (gaps of more than a tenth of the range) in their own
  # panels, so that the axis does not span the gaps between them
  sorted <- sort(wl)
  breaks <- sorted[-1][diff(sorted) > 0.1 * diff(range(wl))]
  window <- findInterval(wl, breaks) + 1
  describe <- function(i) {
    sprintf("Shot %s (%s): intensity %.2f x the median shot, r = %.3f", shots$.shot[i],
            shots$.reason[i], shots$.intensity[i], shots$.correlation[i])
  }
  series <- ifelse(shots$.rejected, vapply(seq_len(nrow(shots)), describe, character(1)),
                   "Kept shots")
  ranges <- vapply(split(wl, window), function(w) sprintf("%.0f-%.0f nm", min(w), max(w)),
                   character(1))
  panel <- factor(ranges[window], levels = ranges)
  long <- data.frame(series = rep(series, times = length(wl)),
                     shot = rep(seq_len(nrow(shots)), times = length(wl)),
                     wavelength = rep(wl, each = nrow(shots)),
                     segment = rep(segment, each = nrow(shots)),
                     panel = rep(panel, each = nrow(shots)),
                     intensity = as.vector(m))
  median <- data.frame(series = "Median", shot = 0, wavelength = wl, segment = segment,
                       panel = panel, intensity = col_medians(m))
  rejected <- unique(series[shots$.rejected])
  levels <- c("Kept shots", "Median", rejected)
  colours <- c("Kept shots" = "grey70", Median = "black",
               stats::setNames(rep_len(shot_palette, length(rejected)), rejected))
  long$series <- factor(long$series, levels = levels)
  median$series <- factor(median$series, levels = levels)
  .group <- intensity <- series <- NULL
  layer <- function(d, width) {
    d$.group <- interaction(d$shot, d$segment)
    ggplot2::geom_line(data = d, ggplot2::aes(x = wavelength, y = intensity, group = .group,
                                              colour = series), linewidth = width)
  }
  p <- ggplot2::ggplot() +
    layer(long[long$series == "Kept shots", ], 0.3) +
    layer(median, 0.5) +
    layer(long[long$series %in% rejected, ], 0.5) +
    ggplot2::scale_colour_manual(values = colours, name = NULL, drop = TRUE) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::scale_y_continuous(labels = plain_numbers) +
    ggplot2::labs(x = "Wavelength (nm)", y = "Intensity (arb. units)") +
    shots_theme(base_size) +
    ggplot2::theme(legend.key.height = ggplot2::unit(0.9, "lines"),
                   legend.key.spacing.y = ggplot2::unit(0, "pt"),
                   legend.justification = "left")
  if (nlevels(panel) > 1) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$panel), nrow = 1, scales = "free_x") +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey95", colour = NA))
  }
  caption <- paste0(
    "Grey: the kept shots; black: the median spectrum of the sample; colored: the rejected ",
    "shots, with their criteria, their total intensity relative to the median shot and their ",
    "correlation r with the median spectrum. ", sum(shots$.rejected), " of ", nrow(shots),
    " shots rejected."
  )
  tbl <- tibble::as_tibble(shots[c(settings$sample, ".shot", ".rejected", ".reason",
                                     ".intensity", ".correlation", ".distance")])
  list(plot = p, table = tbl, caption = paste(strwrap(caption, width = 90), collapse = "\n"),
       title = paste("Shots of sample", sample))
}

# The total intensity, the dissimilarity of shape and the rejected share
# along the shot number.
shots_order <- function(x, settings, base_size) {
  rlang::check_installed("patchwork", reason = "to combine the panels.")
  numbers <- sort(unique(x$.shot))
  x$.number <- factor(x$.shot, levels = numbers)
  x$.dissimilarity <- pmax(1 - x$.correlation, 1e-6)
  by_shot <- split(seq_len(nrow(x)), x$.number)
  per_shot <- function(f) unname_vapply(by_shot, f, numeric(1))
  tbl <- tibble::tibble(
    shot = numbers,
    n = unname(lengths(by_shot)),
    intensity_median = per_shot(function(r) stats::median(x$.intensity[r])),
    intensity_q1 = per_shot(function(r) stats::quantile(x$.intensity[r], 0.25, names = FALSE)),
    intensity_q3 = per_shot(function(r) stats::quantile(x$.intensity[r], 0.75, names = FALSE)),
    dissimilarity_median = per_shot(function(r) stats::median(x$.dissimilarity[r])),
    rejected_pct = per_shot(function(r) 100 * mean(x$.rejected[r]))
  )
  .number <- .intensity <- .dissimilarity <- rejected_pct <- NULL
  size <- base_size / 11
  box <- function(y) {
    ggplot2::geom_boxplot(ggplot2::aes(x = .number, y = {{ y }}), fill = "grey90",
                          colour = "grey25",
                          width = 0.6, outlier.size = 0.6 * size, outlier.colour = "grey45",
                          linewidth = 0.35)
  }
  p1 <- ggplot2::ggplot(x) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey60", linewidth = 0.3, linetype = "dashed") +
    box(.intensity) +
    ggplot2::labs(x = NULL, y = "Intensity\n(relative)",
                  title = "Total intensity, relative to the median shot of the sample") +
    shots_theme(base_size)
  p2 <- ggplot2::ggplot(x) +
    box(.dissimilarity) +
    ggplot2::scale_y_log10(labels = plain_numbers) +
    ggplot2::labs(x = NULL, y = "1 - r\n(log scale)",
                  title = "Dissimilarity of shape to the median spectrum of the sample") +
    shots_theme(base_size)
  p3 <- ggplot2::ggplot(tbl, ggplot2::aes(x = factor(.data$shot, levels = numbers),
                                          y = rejected_pct)) +
    ggplot2::geom_col(fill = "#d95f02", width = 0.6) +
    ggplot2::labs(x = "Shot number", y = "Rejected\n(%)", title = "Rejected shots") +
    shots_theme(base_size)
  small_titles <- ggplot2::theme(plot.title = ggplot2::element_text(face = "plain",
                                                                    size = ggplot2::rel(0.85)))
  p <- patchwork::wrap_plots(p1 + small_titles, p2 + small_titles, p3 + small_titles, ncol = 1,
                             heights = c(1, 1, 0.7))
  first <- tbl$intensity_median[1]
  last <- tbl$intensity_median[nrow(tbl)]
  caption <- paste0(
    "Each box: the shots of that number in all the samples. Median relative intensity from ",
    sprintf("%.3f", first), " (shot ", numbers[1], ") to ", sprintf("%.3f", last), " (shot ",
    numbers[length(numbers)], "). A trend shows cleaning shots or the drift of the ablation, ",
    "which the rejection within each sample does not correct. ", shots_count(x)
  )
  list(plot = p, table = tbl, caption = paste(strwrap(caption, width = 90), collapse = "\n"),
       title = "Laser shots along the shot number")
}

# The rejected shots per sample, and the RSD of the samples with all their
# shots and with the kept ones.
shots_samples <- function(x, settings, base_size) {
  rlang::check_installed("patchwork", reason = "to combine the panels.")
  rows <- split(seq_len(nrow(x)), factor(x$.sample, levels = unique(x$.sample)))
  rsd <- function(v) if (length(v) > 1) 100 * stats::sd(v) / mean(v) else NA_real_
  tbl <- tibble::tibble(
    sample = names(rows),
    n = unname(lengths(rows)),
    n_rejected = unname_vapply(rows, function(r) sum(x$.rejected[r]), integer(1)),
    rsd_all = unname_vapply(rows, function(r) rsd(x$.intensity[r]), numeric(1)),
    rsd_kept = unname_vapply(rows, function(r) rsd(x$.intensity[r][!x$.rejected[r]]), numeric(1))
  )
  names(tbl)[1] <- settings$sample
  counts <- as.data.frame(table(n_rejected = tbl$n_rejected))
  counts$n_rejected <- factor(counts$n_rejected, levels = sort(unique(tbl$n_rejected)))
  .data_counts <- counts
  p1 <- ggplot2::ggplot(.data_counts, ggplot2::aes(x = .data$n_rejected, y = .data$Freq)) +
    ggplot2::geom_col(fill = "grey45", width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Freq), vjust = -0.4,
                       size = 0.75 * base_size / ggplot2::.pt) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = "Rejected shots", y = "Samples", title = "Rejected shots per sample") +
    shots_theme(base_size)
  tbl$.status <- factor(ifelse(tbl$n_rejected > 0, "With rejected shots", "No rejected shot"),
                          levels = c("No rejected shot", "With rejected shots"))
  limit <- max(c(tbl$rsd_all, tbl$rsd_kept), na.rm = TRUE) * 1.05
  p2 <- ggplot2::ggplot(tbl, ggplot2::aes(x = .data$rsd_all, y = .data$rsd_kept)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey60", linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(colour = .data$.status), size = 1.6 * base_size / 11) +
    ggplot2::scale_colour_manual(values = c("No rejected shot" = "grey65",
                                            "With rejected shots" = "#d95f02"), name = NULL) +
    ggplot2::coord_equal(xlim = c(0, limit), ylim = c(0, limit)) +
    ggplot2::labs(x = "RSD, all shots (%)", y = "RSD, kept shots (%)",
                  title = "RSD of the total intensity") +
    shots_theme(base_size)
  tbl$.status <- NULL
  small_titles <- ggplot2::theme(plot.title = ggplot2::element_text(face = "plain",
                                                                    size = ggplot2::rel(0.85)))
  p <- patchwork::wrap_plots(p1 + small_titles, p2 + small_titles, nrow = 1)
  caption <- paste0(
    "RSD: relative standard deviation of the total intensity of the shots of a sample. Median ",
    "RSD: ", sprintf("%.1f", stats::median(tbl$rsd_all, na.rm = TRUE)), "% with all the shots, ",
    sprintf("%.1f", stats::median(tbl$rsd_kept, na.rm = TRUE)), "% with the kept shots. ",
    shots_count(x)
  )
  list(plot = p, table = tbl, caption = paste(strwrap(caption, width = 90), collapse = "\n"),
       title = "Laser shots of the samples")
}
