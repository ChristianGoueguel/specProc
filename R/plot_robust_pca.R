#' @title Outlier Map of a Robust PCA
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Plots the orthogonal distance of each observation against its score
#' distance, for a robust PCA fitted by [robpca()], [rospca()] or
#' [macropca()] (Hubert, Rousseeuw and Vanden Branden, 2005).
#'
#' @details
#' The dashed lines are the cut-offs of the two distances. They divide the
#' map into four types of observations:
#'  - **regular** observations (bottom left);
#'  - **good leverage** points (bottom right): far from the center, but close
#'    to the PCA subspace, so they follow the structure of the data;
#'  - **orthogonal outliers** (top left): close to the center once
#'    projected, but far from the PCA subspace;
#'  - **bad leverage** points (top right): far on both counts, the most
#'    harmful outliers.
#'
#' New observations (`newdata`) are projected onto the model with
#' [predict()][predict.specproc_robpca] and shown with the calibration
#' cut-offs, which is how new spectra are screened before prediction.
#'
#' With `relative = TRUE`, each distance is divided by its cut-off (the
#' reduced score and orthogonal distances), so both cut-offs are at 1
#' whatever the model. This puts maps of different models
#' (for example, with different numbers of components) on the same scale.
#' A relative distance tells where an observation falls with respect to the
#' cut-off, not how unlikely it is: twice the cut-off is not equally rare
#' in every model.
#'
#' @param object An object returned by [robpca()], [rospca()] or
#'   [macropca()].
#' @param newdata Optional new observations to add to the map (a numeric
#'   matrix or data frame with the calibration variables).
#' @param labels The number of most outlying observations to label (by
#'   their row names, or row numbers). Default is 3; use 0 for no labels.
#' @param relative If `TRUE`, plot the reduced distances (divided by their
#'   cut-offs). Default is `FALSE`.
#' @param shade If `TRUE`, shade the three outlying regions in grey, darker
#'   for more harmful observations (good leverage, orthogonal outliers, bad
#'   leverage), and name them in their corners. The region of regular
#'   observations stays white. Default is `FALSE`.
#' @param log If `TRUE`, use logarithmic axes, which spread out the regular
#'   observations when a few are far away. Zero distances are drawn at the
#'   smallest positive distance. Default is `FALSE`.
#' @param colour_by The colors of the points: `"type"` (default), by
#'   outlier type, or `"distance"`, by their reduced distance from the
#'   origin, \eqn{\max(SD/c_{SD}, OD/c_{OD})}, on a rainbow scale from dark
#'   red (the closest) to blue (the farthest). With `relative = TRUE`, where
#'   both cut-offs are at 1, yellow marks the cut-offs: dark red through
#'   orange for the regular observations, then green and blue for the
#'   outlying ones. Otherwise, the colors spread evenly over the distances.
#' @param title The plot title.
#' @param ... Further arguments passed to [ggplot2::geom_point()] to style
#'   the points, such as `alpha` (default 0.85), `size` (2.2), `stroke`
#'   (0.4) or `colour` (the outline, `"black"`). `size` can also be a numeric
#'   vector with one value per sample (the calibration samples, then those
#'   of `newdata`), such as the concentration of an element, to vary the
#'   size of the points, with a legend.
#'
#' @return A ggplot object.
#'
#' @seealso [robpca()], [rospca()], [macropca()], [plot_cell_map()]
#' @export
#'
#' @examples
#' # LIBS spectra of forage samples
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' set.seed(1)
#' fit <- forageLIBS |>
#'   dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
#'   center() |>
#'   robpca()
#'
#' plot_outlier_map(fit, relative = TRUE, shade = TRUE, log = TRUE)
#' plot_outlier_map(fit, relative = TRUE, shade = TRUE, log = TRUE,
#'                  alpha = 0.5, size = 3, stroke = 0.2)
#'
plot_outlier_map <- function(object, newdata = NULL, labels = 3, relative = FALSE,
                             shade = FALSE, log = FALSE, colour_by = c("type", "distance"),
                             title = NULL, ...) {
  if (!inherits(object, "specproc_robpca")) {
    stop("'object' must be returned by robpca(), rospca() or macropca().", call. = FALSE)
  }
  check_count(labels, "labels", lower = 0)
  colour_by <- match.arg(colour_by)
  for (arg in c("relative", "shade", "log")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("'", arg, "' must be TRUE or FALSE.", call. = FALSE)
    }
  }
  ids <- rownames(object$scores) %||% as.character(seq_along(object$sd))
  df <- data.frame(id = ids, sd = object$sd, od = object$od, type = object$outlier_type,
                   set = "calibration", stringsAsFactors = FALSE)
  if (!is.null(newdata)) {
    pred <- stats::predict(object, newdata)
    new_ids <- rownames(newdata) %||% paste0("new", seq_len(nrow(pred)))
    df <- rbind(df, data.frame(id = new_ids, sd = pred$sd, od = pred$od, type = pred$outlier_type,
                               set = "new", stringsAsFactors = FALSE))
  }
  df$set <- factor(df$set, levels = c("calibration", "new"))
  df$type <- factor(df$type, levels = levels(object$outlier_type))

  if (is.null(title)) {
    title <- switch(class(object)[1], specproc_robpca = "ROBPCA", specproc_rospca = "ROSPCA",
                    specproc_macropca = "MacroPCA")
    title <- paste0(title, " outlier map (", object$k, " components)")
  }
  # most outlying: largest distance relative to the cut-offs
  sd_cut <- max(object$cutoff_sd, .Machine$double.eps)
  od_cut <- max(object$cutoff_od, .Machine$double.eps)
  severity <- pmax(df$sd / sd_cut, df$od / od_cut)
  # distance from the origin in reduced distances: 1 on the cut-offs
  df$radius <- severity
  df$label <- ""
  if (labels > 0) {
    top <- order(severity, decreasing = TRUE)[seq_len(min(labels, nrow(df)))]
    top <- top[severity[top] > 1]
    df$label[top] <- df$id[top]
  }

  cut_x <- object$cutoff_sd
  cut_y <- object$cutoff_od
  x_lab <- "Score distance"
  y_lab <- "Orthogonal distance"
  if (relative) {
    df$sd <- df$sd / sd_cut
    df$od <- df$od / od_cut
    cut_x <- cut_y <- 1
    x_lab <- "Reduced score distance"
    y_lab <- "Reduced orthogonal distance"
  }
  if (log) {
    # plot log10 distances on linear axes labelled in the original units, so
    # the shaded regions can still reach the panel edges (-Inf); zeros are
    # drawn at the smallest positive distance
    for (v in c("sd", "od")) {
      positive <- df[[v]][df[[v]] > 0]
      floor_v <- if (length(positive)) min(positive) else 1
      df[[v]] <- log10(pmax(df[[v]], floor_v))
    }
    cut_x <- log10(cut_x)
    cut_y <- log10(cut_y)
  }

  dots <- rlang::enquos(...)
  point_args <- lapply(dots, rlang::eval_tidy)
  if (length(point_args) && (is.null(names(point_args)) || any(names(point_args) == ""))) {
    stop("The arguments in '...' must be named, such as 'alpha = 0.5'.", call. = FALSE)
  }
  names(point_args)[names(point_args) == "color"] <- "colour"
  # a size per sample is mapped, with a legend
  size_name <- NULL
  if (length(point_args$size) > 1) {
    if (!is.numeric(point_args$size) || length(point_args$size) != nrow(df)) {
      stop("`size` must be a number, or a numeric vector with one value per sample (",
           nrow(df), if (!is.null(newdata)) ", calibration then new samples", ").", call. = FALSE)
    }
    df$.size <- point_args$size
    size_name <- rlang::as_label(dots$size)
    point_args$size <- NULL
  }
  point_args <- utils::modifyList(list(colour = "black", size = 2.2, stroke = 0.4, alpha = 0.85),
                                  point_args)
  point_args$mapping <- ggplot2::aes(fill = .data$type, shape = .data$set)
  if (colour_by == "distance") {
    point_args$mapping$fill <- quote(.data$radius)
    # the farthest points on top
    df <- df[order(df$radius), , drop = FALSE]
  }
  if (!is.null(size_name)) {
    point_args$mapping$size <- quote(.data$.size)
    point_args$size <- NULL
  }

  palette <- c(regular = "grey55", `good leverage` = "#1b9e77",
               `orthogonal outlier` = "#d95f02", `bad leverage` = "#e7298a")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$sd, y = .data$od))
  if (shade) {
    regions <- data.frame(
      xmin = c(cut_x, -Inf, cut_x), xmax = c(Inf, cut_x, Inf),
      ymin = c(-Inf, cut_y, cut_y), ymax = c(cut_y, Inf, Inf),
      alpha = c(0.06, 0.13, 0.22)
    )
    # translucent so that the grid shows through
    p <- p +
      ggplot2::geom_rect(data = regions, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                                      ymin = .data$ymin, ymax = .data$ymax),
                         alpha = regions$alpha, fill = "grey20", inherit.aes = FALSE) +
      # "bad leverage" sits above the OD cut-off rather than in the top
      # right corner, where the most outlying observations usually are
      ggplot2::annotate("text", x = c(Inf, -Inf, Inf), y = c(-Inf, Inf, cut_y),
                        label = c("good leverage", "orthogonal outliers", "bad leverage"),
                        hjust = c(1.05, -0.05, 1.05), vjust = c(-0.6, 1.6, -0.6),
                        size = 3, fontface = "italic", colour = "grey35")
  }
  p <- p +
    ggplot2::geom_vline(xintercept = cut_x, linetype = "dashed", colour = "grey30") +
    ggplot2::geom_hline(yintercept = cut_y, linetype = "dashed", colour = "grey30") +
    do.call(ggplot2::geom_point, point_args) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 3, hjust = -0.2, vjust = -0.4,
                       colour = "grey20", na.rm = TRUE) +
    (if (colour_by == "type") {
      ggplot2::scale_fill_manual(values = palette, drop = FALSE, name = NULL,
                                 guide = ggplot2::guide_legend(order = 1, override.aes = list(shape = 21, size = 2.5)))
    } else {
      distance_fill_scale(range(df$radius, na.rm = TRUE), anchored = relative)
    }) +
    # the kinds of samples only matter with new samples
    ggplot2::scale_shape_manual(values = c(calibration = 21, new = 24), name = NULL, drop = TRUE,
                                guide = if (is.null(newdata)) "none" else
                                  ggplot2::guide_legend(order = 2, override.aes = list(size = 2.5))) +
    ggplot2::labs(x = x_lab, y = y_lab, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    compact_legend()
  if (!is.null(size_name)) {
    p <- p + ggplot2::theme(legend.box = "vertical")
    p <- p + ggplot2::scale_size_continuous(range = c(1.2, 6), name = size_name, breaks = three_breaks,
                                            guide = ggplot2::guide_legend(order = 3))
  }
  if (log) {
    log_breaks <- function(limits) log10(scales::breaks_log(n = 6)(10^limits))
    log_labels <- function(breaks) vapply(10^breaks, format, character(1), digits = 3, scientific = FALSE)
    p <- p +
      ggplot2::scale_x_continuous(breaks = log_breaks, labels = log_labels, minor_breaks = NULL) +
      ggplot2::scale_y_continuous(breaks = log_breaks, labels = log_labels, minor_breaks = NULL)
  }
  p
}

#' @title Cell Map of a MacroPCA Fit
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Shows which cells of the data deviate from a [macropca()] fit: one row
#' per observation, the variables (wavelengths) along the horizontal axis,
#' and the flagged cells colored red when the observed value is higher than
#' the fit and blue when it is lower. A strip on the right shows the outlier
#' type of each observation, and a panel on top the share of flagged cells
#' at each wavelength over the mean spectrum, with the most flagged regions
#' labeled, which shows which emission lines hold the cellwise outliers.
#'
#' @details
#' **Resolution.** Spectra have many more cells than a plot has pixels, so
#' adjacent cells are combined into blocks: the numbers of rows and columns
#' of blocks are at most `resolution`, and a map smaller than that shows
#' every cell. Channels are combined only within a detector segment, and the
#' gaps between segments stay empty. The map fills the plot area, whatever
#' the numbers of observations and variables.
#'
#' **Colors.** A flagged cell with standardized residual \eqn{r} has an
#' intensity that grows with \eqn{\log(|r|/c)}, from 0.25 at the flagging
#' cut-off \eqn{c} to 1 at \eqn{10c} and beyond. The color of a block is the
#' mean of these signed intensities over its cells (0 for cells that are not
#' flagged), on a square-root scale so that blocks with a few flagged cells
#' remain visible: pale blocks have few or mixed flagged cells, saturated
#' blocks many cells that deviate strongly in the same direction.
#'
#' **Order.** With `order = "cluster"`, the observations are sorted by a
#' hierarchical clustering (Ward's method) of their flagged cells, at the
#' column resolution of the map, so that observations that deviate in the
#' same regions form bands (for example, a batch or a type of matrix).
#'
#' **Profile.** The top panel shows, for each variable, the share of the
#' (shown) observations whose cell is flagged: above zero when higher than
#' the fit, below zero when lower. The mean spectrum is drawn in grey
#' behind it, rescaled, to show whether the flagged channels are on emission
#' lines, on the continuum or in noise. It is the mean of `spectra` or, by
#' default, of the data imputed by MacroPCA; it is left out when this mean
#' is close to zero, as for centered data (then give the raw spectra in
#' `spectra`). The dashed lines are at `threshold`, and the `labels` flagged
#' regions with the largest share (see [flagged_regions()]) are labeled with
#' their peak wavelength, or with the emission line they match.
#'
#' @param object An object returned by [macropca()].
#' @param rows,columns Optional indices or names of the rows and columns to
#'   show. Default is all.
#' @param resolution The maximum numbers of rows and columns of blocks.
#'   Default is `c(200, 400)`.
#' @param order The order of the rows: `"data"` (default), `"od"` for
#'   decreasing orthogonal distance, which puts the most outlying
#'   observations at the top, or `"cluster"` to group the observations with
#'   similar flagged cells.
#' @param profile If `TRUE` (default), add the panel of the share of flagged
#'   cells by variable.
#' @param spectra Optional spectra whose mean is drawn behind the profile (a
#'   data frame or matrix with the variables of the model, other columns
#'   being ignored, or a single named spectrum). Default is the data imputed
#'   by MacroPCA.
#' @param threshold The share of flagged observations above which channels
#'   form a flagged region. Default is 0.1.
#' @param labels The number of flagged regions labeled in the profile.
#'   Default is 5; use 0 for none.
#' @param lines Optional line list returned by [libs_lines()], to label the
#'   regions with the emission lines they match.
#' @param tol The largest distance, in nm, between a region and a line it
#'   matches. Default is 0.1.
#' @param title The plot title.
#'
#' @return A patchwork (ggplot2) object.
#'
#' @seealso [flagged_regions()], [macropca()], [plot_outlier_map()]
#' @export
#'
#' @examples
#' \donttest{
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' set.seed(1)
#' fit <- forageLIBS |>
#'   dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
#'   macropca(k = 3)
#'
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   plot_cell_map(fit, order = "cluster")
#'   plot_cell_map(fit, order = "od")
#' }
#' }
#'
plot_cell_map <- function(object, rows = NULL, columns = NULL, resolution = c(200, 400),
                          order = c("data", "od", "cluster"), profile = TRUE, spectra = NULL,
                          threshold = 0.1, labels = 5, lines = NULL, tol = 0.1,
                          title = "MacroPCA cell map") {
  check_macropca(object)
  order <- match.arg(order)
  check_flag(profile, "profile")
  if (!is.numeric(resolution) || length(resolution) != 2 || anyNA(resolution) ||
      any(resolution < 1) || any(resolution %% 1 != 0)) {
    stop("'resolution' must be two positive integers (rows and columns).", call. = FALSE)
  }
  check_count(labels, "labels", lower = 0)
  rlang::check_installed("patchwork", reason = "to combine the panels of the cell map.")
  cells <- cell_map_data(object, rows, columns)
  check_region_args(threshold, lines, tol, cells$has_wavelength)

  # signed intensity of each cell: 0 unless flagged
  resid <- cells$resid
  flagged <- cells$flagged
  cutoff <- if (any(flagged)) min(abs(resid[flagged])) else sqrt(stats::qchisq(0.99, 1))
  intensity <- 0.25 + 0.75 * pmin(1, log(pmax(abs(resid), cutoff) / cutoff) / log(10))
  cell <- ifelse(flagged, sign(resid) * intensity, 0)
  cell[is.na(cell)] <- 0

  col_id <- cell_map_columns(cells$segment, ncol(cell), resolution[2])
  o <- switch(order,
    data = seq_len(nrow(cell)),
    od = order(object$od[cells$rows], decreasing = TRUE),
    cluster = cell_map_cluster(cell, col_id)
  )
  blocks <- cell_map_blocks(cell[o, , drop = FALSE], cells$wavelength, col_id, resolution[1])
  map <- cell_map_panel(blocks, cells$ids[o], cells$has_wavelength, order)
  strip <- cell_map_strip(cells$type[o])
  x_limits <- attr(blocks, "extent")[1:2]
  map <- map + ggplot2::scale_x_continuous(limits = x_limits, expand = c(0, 0))
  if (!profile) {
    return(patchwork::wrap_plots(map, strip, widths = c(1, 0.025)) +
             patchwork::plot_layout(guides = "collect") +
             patchwork::plot_annotation(title = title) &
             ggplot2::theme(legend.position = "bottom", legend.box = "vertical"))
  }
  if (is.null(spectra)) {
    # the mean of the imputed data, unless it is close to zero (centered data)
    imputed <- object$imputed[cells$rows, cells$columns, drop = FALSE]
    spectrum <- colMeans(imputed, na.rm = TRUE)
    spread <- max(apply(imputed, 2, stats::sd, na.rm = TRUE), na.rm = TRUE)
    if (!is.finite(spread) || max(abs(spectrum)) < 0.01 * spread) spectrum <- NULL
  } else {
    spectrum <- mean_spectrum(spectra, cells$variables)
  }
  regions <- find_flagged_regions(cells, threshold, lines, tol)
  top <- cell_map_profile(cells, spectrum, regions, threshold, labels, x_limits)
  patchwork::wrap_plots(top, patchwork::plot_spacer(), map, strip,
                        ncol = 2, widths = c(1, 0.025),
                        heights = if (labels > 0 && nrow(regions) > 0) c(1.6, 4) else c(1, 4)) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(title = title) &
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical")
}

#' @title Flagged Regions of a MacroPCA Fit
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Lists the wavelength regions where many observations have cells flagged
#' by [macropca()]: runs of adjacent channels (on the same detector segment)
#' whose share of flagged observations is at least `threshold`. These are
#' the channels that persistently deviate from the PCA fit, for example
#' emission lines affected by self-absorption, saturation or matrix
#' effects, and candidates to exclude or down-weight. [plot_cell_map()]
#' labels the regions with the largest share.
#'
#' @details
#' Runs separated by at most two channels below the threshold are merged. A
#' region is `"higher"` when at least two thirds of its flagged cells are
#' higher than the fit, `"lower"` when at most one third are, and `"mixed"`
#' otherwise. With `lines`, the lines between `start - tol` and `end + tol`
#' are the candidates, from the nearest to the peak.
#'
#' @inheritParams plot_cell_map
#'
#' @return A tibble with one row per region, sorted by decreasing share,
#'   and columns `start`, `end` and `peak` (the wavelength, or variable
#'   number, of the largest share), `channels` (the number of channels),
#'   `share` (the largest share of flagged observations), `mean_share` and
#'   `direction`. With `lines`, it also has the columns `species` and
#'   `line_wavelength` of the nearest line (`NA` when none) and
#'   `candidates`.
#'
#' @seealso [plot_cell_map()], [macropca()], [libs_lines()]
#' @export
#'
#' @examples
#' \donttest{
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' set.seed(1)
#' fit <- forageLIBS |>
#'   dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
#'   macropca(k = 3)
#'
#' flagged_regions(fit)
#' }
#'
flagged_regions <- function(object, threshold = 0.1, rows = NULL, columns = NULL, lines = NULL,
                            tol = 0.1) {
  check_macropca(object)
  cells <- cell_map_data(object, rows, columns)
  check_region_args(threshold, lines, tol, cells$has_wavelength)
  out <- find_flagged_regions(cells, threshold, lines, tol)
  if (is.null(lines)) out <- out[, setdiff(names(out), c("species", "line_wavelength", "candidates"))]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# Rainbow of the distances of plot_outlier_map(): dark red at the origin,
# through orange, yellow at the cut-offs (distance 1), then green and blue.
radial_rainbow <- c("#67001f", "#b2182b", "#f46d43", "#fdae61", "#fdd835", "#d9ef8b",
                    "#66bd63", "#1a9850", "#4575b4", "#313695")

# The fill scale of the distances. Anchored (relative distances, both
# cut-offs at 1): the colors below yellow spread over [0, 1], the others over
# [1, the largest distance], and 1 is labeled. Otherwise, the colors spread
# evenly over the range of the distances.
distance_fill_scale <- function(range, anchored) {
  if (!anchored) {
    return(ggplot2::scale_fill_gradientn(
      colours = radial_rainbow, name = "Reduced distance",
      guide = ggplot2::guide_colourbar(order = 1, barwidth = 12, barheight = 0.5, title.vjust = 0.9)
    ))
  }
  top <- max(range[2], 1.5)
  stops <- c(0, 0.35, 0.65, 0.85, 1, 1 + (top - 1) * c(0.15, 0.35, 0.55, 0.8, 1))
  breaks <- pretty(c(0, top), n = 5)
  breaks <- sort(c(1, breaks[breaks >= 0 & breaks <= top & abs(breaks - 1) > 0.12 * top]))
  ggplot2::scale_fill_gradientn(
    colours = radial_rainbow, values = stops / top, limits = c(0, top),
    breaks = breaks, labels = ifelse(breaks == 1, "1 (cut-off)", format(breaks, trim = TRUE)),
    name = "Reduced distance",
    guide = ggplot2::guide_colourbar(order = 1, barwidth = 12, barheight = 0.5, title.vjust = 0.9)
  )
}

# ---- cell map internals ------------------------------------------------------

check_macropca <- function(object) {
  if (!inherits(object, "specproc_macropca")) {
    stop("'object' must be returned by macropca().", call. = FALSE)
  }
  invisible(object)
}

check_region_args <- function(threshold, lines, tol, has_wavelength) {
  check_number(threshold, "threshold", lower = 0, upper = 1, lower_open = TRUE)
  check_number(tol, "tol", lower = 0)
  if (!is.null(lines)) {
    check_lines_table(lines)
    if (!has_wavelength) {
      stop("Matching 'lines' needs variables named by wavelength.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# The selected residuals and flags, with the row ids, outlier types,
# variable names, wavelengths and detector segments.
cell_map_data <- function(object, rows, columns) {
  resid <- object$std_resid
  n <- nrow(resid)
  ids <- rownames(resid) %||% as.character(seq_len(n))
  variables <- colnames(resid) %||% attr(object, "variables") %||% as.character(seq_len(ncol(resid)))
  keep_rows <- cell_map_index(rows, ids, n, "rows")
  keep_cols <- cell_map_index(columns, variables, ncol(resid), "columns")
  variables <- variables[keep_cols]
  wavelength <- names_to_wavelength(variables)
  has_wavelength <- length(wavelength) == length(variables)
  if (!has_wavelength) wavelength <- seq_along(variables)
  list(
    resid = resid[keep_rows, keep_cols, drop = FALSE],
    flagged = object$flagged_cells[keep_rows, keep_cols, drop = FALSE],
    rows = keep_rows, columns = keep_cols, ids = ids[keep_rows],
    type = object$outlier_type[keep_rows], variables = variables,
    wavelength = wavelength, has_wavelength = has_wavelength,
    segment = if (has_wavelength) wavelength_segments(wavelength) else rep(1L, length(wavelength))
  )
}

cell_map_index <- function(select, names, n, arg) {
  if (is.null(select)) return(seq_len(n))
  idx <- if (is.character(select)) match(select, names) else select
  if (!is.numeric(idx) || length(idx) == 0 || anyNA(idx) || any(idx < 1 | idx > n) ||
      any(idx %% 1 != 0)) {
    stop("'", arg, "' must be indices or names of the ", arg, " of the data.", call. = FALSE)
  }
  as.integer(idx)
}

# Column block of each channel: adjacent channels of the same detector
# segment (segments are contiguous in column order).
cell_map_columns <- function(segment, p, max_blocks) {
  size <- ceiling(p / max_blocks)
  block <- stats::ave(seq_len(p), segment, FUN = function(i) ceiling(seq_along(i) / size))
  key <- paste(segment, block)
  match(key, unique(key))
}

# Row order of a Ward clustering of the rows, at the column resolution.
cell_map_cluster <- function(cell, col_id) {
  if (nrow(cell) < 3) return(seq_len(nrow(cell)))
  reduced <- t(rowsum(t(cell), col_id, reorder = TRUE)) / rep(as.vector(table(col_id)), each = nrow(cell))
  stats::hclust(stats::dist(reduced), method = "ward.D2")$order
}

# Mean signed intensity of blocks of adjacent rows and of column blocks;
# only the nonzero blocks are returned.
cell_map_blocks <- function(cell, wavelength, col_id, max_rows) {
  n <- nrow(cell)
  row_block <- ceiling(seq_len(n) / ceiling(n / max_rows))
  # sums over row blocks, then over column blocks
  by_rows <- rowsum(cell, row_block, reorder = TRUE)
  sums <- t(rowsum(t(by_rows), col_id, reorder = TRUE))
  counts <- outer(as.vector(table(row_block)), as.vector(table(col_id)))
  value <- sums / counts
  spacing <- channel_spacing(wavelength)
  x_lo <- tapply(wavelength, col_id, min) - spacing / 2
  x_hi <- tapply(wavelength, col_id, max) + spacing / 2
  r_lo <- tapply(seq_len(n), row_block, min) - 0.5
  r_hi <- tapply(seq_len(n), row_block, max) + 0.5
  nz <- which(value != 0, arr.ind = TRUE)
  out <- data.frame(xmin = x_lo[nz[, 2]], xmax = x_hi[nz[, 2]],
                    ymin = r_lo[nz[, 1]], ymax = r_hi[nz[, 1]],
                    value = value[nz])
  # keep the extent of the map even without flagged cells
  attr(out, "extent") <- c(min(x_lo), max(x_hi), 0.5, n + 0.5)
  out$fill <- sign(out$value) * sqrt(abs(out$value))
  rownames(out) <- NULL
  out
}

channel_spacing <- function(wavelength) {
  spacing <- stats::median(abs(diff(wavelength)))
  if (!is.finite(spacing) || spacing == 0) 1 else spacing
}

# Runs of channels flagged in at least `threshold` of the observations.
find_flagged_regions <- function(cells, threshold, lines, tol) {
  flagged <- cells$flagged
  resid <- cells$resid
  n <- nrow(flagged)
  up <- colSums(flagged & resid > 0, na.rm = TRUE)
  down <- colSums(flagged & resid < 0, na.rm = TRUE)
  share <- (up + down) / n
  above <- which(share >= threshold)
  empty <- data.frame(start = numeric(), end = numeric(), peak = numeric(), channels = integer(),
                      share = numeric(), mean_share = numeric(), direction = character(),
                      species = character(), line_wavelength = numeric(),
                      candidates = character(), stringsAsFactors = FALSE)
  if (length(above) == 0) return(empty)
  # a new region after more than two channels below the threshold, or a new segment
  starts <- c(TRUE, diff(above) > 3 | diff(cells$segment[above]) != 0)
  region <- cumsum(starts)
  wl <- cells$wavelength
  out <- do.call(rbind, lapply(split(above, region), function(i) {
    i <- seq(min(i), max(i))
    top <- i[which.max(share[i])]
    higher <- sum(up[i]) / max(1, sum(up[i] + down[i]))
    data.frame(start = min(wl[i]), end = max(wl[i]), peak = wl[top], channels = length(i),
               share = share[top], mean_share = mean(share[i]),
               direction = if (higher >= 2 / 3) "higher" else if (higher <= 1 / 3) "lower" else "mixed",
               stringsAsFactors = FALSE)
  }))
  out <- out[order(out$share, decreasing = TRUE), , drop = FALSE]
  out$species <- NA_character_
  out$line_wavelength <- NA_real_
  out$candidates <- NA_character_
  if (!is.null(lines) && nrow(lines) > 0) {
    for (r in seq_len(nrow(out))) {
      near <- which(lines$wavelength >= out$start[r] - tol & lines$wavelength <= out$end[r] + tol)
      if (length(near) == 0) next
      near <- near[order(abs(lines$wavelength[near] - out$peak[r]))]
      out$species[r] <- lines$species[near[1]]
      out$line_wavelength[r] <- lines$wavelength[near[1]]
      out$candidates[r] <- paste(lines$species[near], format_wavelength(lines$wavelength[near], 2),
                                 collapse = ", ")
    }
  }
  rownames(out) <- NULL
  out
}

cell_map_panel <- function(blocks, ids, has_wavelength, order) {
  n <- length(ids)
  if (n <= 40) {
    breaks <- seq_len(n)
  } else {
    breaks <- pretty(c(1, n), n = 8)
    breaks <- unique(c(1, breaks[breaks >= 1 & breaks <= n]))
  }
  labels <- ids[breaks]
  extent <- attr(blocks, "extent")
  ggplot2::ggplot(blocks) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                    ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill)) +
    ggplot2::geom_blank(data = data.frame(x = extent[1:2], y = extent[3:4]),
                        ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                                  midpoint = 0, limits = c(-1, 1), breaks = c(-1, 1),
                                  labels = c("lower than fit", "higher than fit"),
                                  name = "Flagged cells",
                                  guide = ggplot2::guide_colourbar(order = 1, barwidth = 8)) +
    ggplot2::scale_y_reverse(breaks = breaks, labels = labels, expand = c(0, 0)) +
    ggplot2::labs(x = if (has_wavelength) "Wavelength (nm)" else "Variable",
                  y = switch(order, data = "Observation", od = "Observation (by decreasing OD)",
                             cluster = "Observation (clustered)")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

cell_map_strip <- function(type) {
  palette <- c(regular = "grey85", `good leverage` = "#1b9e77",
               `orthogonal outlier` = "#d95f02", `bad leverage` = "#e7298a")
  df <- data.frame(x = 1, y = seq_along(type), type = type)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$type)) +
    ggplot2::geom_tile(width = 1, height = 1) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE, name = NULL,
                               guide = ggplot2::guide_legend(order = 2)) +
    ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::theme_void()
}

cell_map_profile <- function(cells, spectrum, regions, threshold, labels, x_limits) {
  n <- nrow(cells$resid)
  wavelength <- cells$wavelength
  up <- colSums(cells$flagged & cells$resid > 0, na.rm = TRUE) / n
  down <- colSums(cells$flagged & cells$resid < 0, na.rm = TRUE) / n
  # one bar per variable, as wide as its column of the map
  spacing <- channel_spacing(wavelength)
  df <- data.frame(xmin = rep(wavelength, 2) - spacing / 2, xmax = rep(wavelength, 2) + spacing / 2,
                   value = c(up, -down),
                   sign = rep(c("higher", "lower"), each = length(wavelength)))
  df <- df[df$value != 0, , drop = FALSE]
  top_share <- max(c(up, threshold))
  p <- ggplot2::ggplot()
  spread <- if (is.null(spectrum)) NA else max(abs(spectrum), na.rm = TRUE)
  if (is.finite(spread) && spread > 0) {
    bg <- data.frame(wavelength = wavelength, value = spectrum / spread * top_share,
                     segment = cells$segment)
    p <- p + ggplot2::geom_line(data = bg, ggplot2::aes(x = .data$wavelength, y = .data$value,
                                                        group = .data$segment),
                                colour = "grey75", linewidth = 0.3)
  }
  p <- p +
    ggplot2::geom_rect(data = df, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = 0,
                                               ymax = .data$value, fill = .data$sign,
                                               colour = .data$sign), linewidth = 0.1) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = c(-threshold, threshold), colour = "grey40",
                        linetype = "dashed", linewidth = 0.3)
  shown <- utils::head(regions, labels)
  if (nrow(shown) > 0) {
    digits <- if (cells$has_wavelength) wavelength_digits(wavelength) else 0L
    shown$label <- ifelse(is.na(shown$species), format_wavelength(shown$peak, digits),
                          paste(shown$species, format_wavelength(shown$line_wavelength, 2)))
    # horizontal labels, spread apart by their width (for plots about 10
    # inches wide) and kept inside the panel
    half <- 0.0033 * nchar(shown$label) * diff(x_limits)
    shown$x <- spread_positions(shown$peak, 2 * max(half) + 0.005 * diff(x_limits))
    shown$x <- pmin(pmax(shown$x, x_limits[1] + half), x_limits[2] - half)
    lower <- shown$direction == "lower"
    shown$y0 <- ifelse(lower, -down[match(shown$peak, wavelength)], up[match(shown$peak, wavelength)])
    shown$y <- ifelse(lower, -1, 1) * (pmax(abs(shown$y0), threshold) + 0.12 * top_share)
    shown$vjust <- ifelse(lower, 1, 0)
    room <- data.frame(x = shown$x, y = shown$y + ifelse(lower, -1, 1) * 0.3 * top_share)
    p <- p +
      ggplot2::geom_segment(data = shown, ggplot2::aes(x = .data$peak, xend = .data$x,
                                                       y = .data$y0, yend = .data$y),
                            colour = "grey55", linewidth = 0.25) +
      ggplot2::geom_text(data = shown, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                                                    vjust = .data$vjust),
                         size = 2.6, colour = "grey15",
                         fontface = ifelse(is.na(shown$species), "plain", "bold")) +
      ggplot2::geom_blank(data = room, ggplot2::aes(x = .data$x, y = .data$y))
  }
  p +
    ggplot2::scale_fill_manual(values = c(higher = "#b2182b", lower = "#2166ac"), guide = "none",
                               aesthetics = c("fill", "colour")) +
    ggplot2::scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = function(v) scales::percent(abs(v))) +
    ggplot2::labs(x = NULL, y = "Flagged") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
}
