#' @title Plot a Two-Dimensional Embedding
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Draws the samples in two dimensions of an embedding, such as UMAP
#' coordinates from `embed::step_umap()`, principal component scores from
#' `recipes::step_pca()`, [robpca()] or [stats::prcomp()], colored by a
#' variable.
#'
#' @details
#' By default, the axes are the first two columns named like embedding
#' coordinates (`UMAP1`, `PC1`, `Comp1`, ...; the name followed by a
#' number), or else the first two numeric columns. A numeric `colour` uses
#' a continuous viridis scale, other types a discrete palette.
#'
#' With `ellipse = TRUE`, a confidence ellipse is drawn for each group of a
#' discrete `colour` (or for all the samples otherwise), with
#' [ConfidenceEllipse::confidence_ellipse()], at each level of `conf_level`
#' (0.975 by default). It covers the region expected to hold that share of
#' the samples of the group if they follow a
#' bivariate normal distribution, from their mean and covariance, or from
#' robust estimates (MCD) with `robust = TRUE`, which resist outlying
#' samples. `distribution = "hotelling"` uses the quantile of Hotelling's
#' \eqn{T^2} distribution, which accounts for the uncertainty of the
#' estimates and suits small groups. Robust estimates need larger groups
#' (the MCD fits a subset of about three quarters of the samples): with
#' fewer than about 10 samples per group, their ellipses can be flat or
#' leave out several samples. Groups with fewer than 4 samples get no
#' ellipse.
#'
#' With `hotelling = "all"`, the ellipses of Hotelling's \eqn{T^2} at each
#' level of `conf_level` (97.5% by default) are drawn for all the
#' samples (contours of \eqn{T^2} on the two components shown, from their
#' mean and covariance, with [HotellingEllipse::ellipseCoord()]), and the
#' samples beyond the limit at the highest level of \eqn{T^2} on `k`
#' components (see [hotelling_t2()]) are circled and labeled: the classical
#' outlier limits of a score plot. With `hotelling
#' = "group"`, each group of a discrete `colour` gets its own ellipses and
#' limits, which flags the samples atypical of their own group. The
#' ellipses are labeled with their level (instead of a legend), and drawn
#' for the two components shown, while the limits use
#' `k` components, so with `k > 2` a flagged sample can lie inside the
#' ellipses. \eqn{T^2} suits linear scores such as PCA or PLS; on a UMAP
#' map, whose distances are not meaningful, prefer `ellipse`.
#'
#' **Style.** The panel is drawn in the style of SIMCA score plots: grey
#' outside the outermost ellipse (of \eqn{T^2}, or of each group) and white
#' inside, so that the samples beyond the limits stand out, with no grid and
#' a fixed `aspect_ratio` (0.7 by default; `NULL` lets the plot fill the
#' space), and thin black lines through the origin (when it lies in the
#' range of the samples, as for centered scores). Without ellipses, the
#' panel is white.
#'
#' **Axes.** For a [stats::prcomp()] fit or a robust PCA, the axis titles
#' give the share of the variance of each component: of the total variance
#' for [stats::prcomp()], and of the variance of the `k` components of the
#' model for [robpca()], [rospca()] and [macropca()], which estimate only
#' these. With groups, the ellipses (confidence, or \eqn{T^2} with
#' `hotelling = "group"`) are filled with the color of their group.
#'
#' **Point size.** `size` is a number, or a numeric variable (a column of
#' `data` or a vector with one value per sample), such as the concentration
#' of an element, which sets the size of each point, with a legend.
#'
#' **Biplot.** With `biplot = TRUE` (for a [stats::prcomp()] fit or a robust
#' PCA), the loadings of the two components are drawn over the scores: all
#' the variables as faint points, and the `biplot_top` longest as labeled
#' arrows. For spectra, the arrows are the local peaks of the loading
#' length along the wavelength, one per emission line, labeled with their
#' wavelength. Each axis of loadings is scaled to the range of its scores,
#' and read on the top and right axes. A sample lies toward the arrows of
#' the variables in which it is high.
#'
#' In a UMAP embedding, only the neighborhoods are meaningful: the sizes of
#' the clusters and the distances between them are not, and they change
#' with `neighbors` and `min_dist`. Read the plot as a map of which samples
#' are similar, not as a quantitative projection.
#'
#' @param data The embedding: a data frame (such as a baked recipe), a
#'   matrix, a [stats::prcomp()] fit, or an object of [robpca()],
#'   [rospca()] or [macropca()].
#' @param x,y The columns of the axes, unquoted or as strings. By default,
#'   the first two embedding coordinates (see details).
#' @param colour The variable coloring the points: a column of `data`
#'   (unquoted or as a string), or a vector with one value per sample.
#' @param size The size of the points: a number (default 2), or a numeric
#'   column of `data` or vector with one value per sample to vary it.
#' @param alpha The opacity of the points.
#' @param ellipse A logical: draw confidence ellipses (`FALSE`, default).
#'   Needs the ConfidenceEllipse package.
#' @param conf_level The confidence level(s) of the ellipses, confidence
#'   and \eqn{T^2}: one or more values between 0 and 1. Default is 0.975.
#'   The samples beyond a \eqn{T^2} limit are flagged at the highest level.
#' @param robust A logical: robust ellipses (`FALSE`, default).
#' @param distribution The quantile of the ellipses: `"normal"` (default,
#'   chi-square) or `"hotelling"`.
#' @param hotelling Hotelling's \eqn{T^2} ellipses and outliers: `"none"`
#'   (default), `"all"` (all samples) or `"group"` (within the groups of a
#'   discrete `colour`). Needs the HotellingEllipse package (1.3.0 or
#'   later).
#' @param k The number of components of \eqn{T^2}: the two axes, then the
#'   next embedding coordinates. Default is 2.
#' @param t2_method The distribution of the \eqn{T^2} limits: `"f"`
#'   (default) or `"beta"` (see [hotelling_t2()]).
#' @param label The labels of the outlying samples: a column of `data` or a
#'   vector with one value per sample. By default, their row numbers.
#' @param biplot A logical: draw the loadings over the scores (`FALSE`,
#'   default).
#' @param biplot_top The number of loadings drawn as labeled arrows.
#'   Default is 10.
#' @param aspect_ratio The ratio of the height to the width of the panel.
#'   Default is 0.7; `NULL` lets the panel fill the plot.
#' @param title The plot title.
#'
#' @return A ggplot object.
#' @seealso [hotelling_t2()], [plot_outlier_map()], [plot_loadings()], [robpca()]
#' @export plot_embedding
#'
#' @examples
#' data(soilLIBS)
#' spectra <- average(soilLIBS[-(2:8)], Sample)
#'
#' # soil texture in three classes (USDA general terms: coarse = sands and
#' # sandy loams, medium = loams and silty loams, fine = clays and clay loams)
#' classes <- c(Sand = "coarse", `Loamy Sand` = "coarse", `Sandy Loam` = "coarse",
#'              Loam = "medium", `Silt Loam` = "medium", Silt = "medium",
#'              `Clay Loam` = "fine", `Silty Clay Loam` = "fine", `Sandy Clay Loam` = "fine",
#'              Clay = "fine", `Silty Clay` = "fine", `Sandy Clay` = "fine")
#' texture <- soilLIBS$Texture[match(spectra$Sample, soilLIBS$Sample)]
#' texture <- factor(classes[as.character(texture)], levels = c("fine", "medium", "coarse"))
#'
#' pca <- stats::prcomp(spectra[-1], scale. = TRUE)
#' plot_embedding(pca, colour = texture, title = "PCA of the sample spectra")
#'
#' if (rlang::is_installed("ConfidenceEllipse")) {
#'   plot_embedding(pca, colour = texture, ellipse = TRUE, distribution = "hotelling")
#' }
#'
#' # Hotelling's T-squared limits of the score plot, on 3 components
#' if (rlang::is_installed("ConfidenceEllipse")) {
#'   plot_embedding(pca, colour = texture, hotelling = "all", k = 3, label = spectra$Sample)
#' }
#'
#' # point size by clay content, and a biplot of the emission lines
#' clay <- soilLIBS$Clay[match(spectra$Sample, soilLIBS$Sample)]
#' plot_embedding(pca, colour = texture, size = clay)
#' plot_embedding(pca, colour = texture, biplot = TRUE)
#'
#' if (rlang::is_installed(c("recipes", "embed"))) {
#'   set.seed(1)
#'   umap <- recipes::recipe(~ ., data = spectra) |>
#'     recipes::update_role(Sample, new_role = "id") |>
#'     recipes::step_normalize(recipes::all_predictors()) |>
#'     recipes::step_pca(recipes::all_predictors(), num_comp = 10) |>
#'     embed::step_umap(recipes::all_predictors(), neighbors = 10) |>
#'     recipes::prep() |>
#'     recipes::bake(new_data = NULL)
#'   plot_embedding(umap, colour = texture)
#' }
plot_embedding <- function(data, x = NULL, y = NULL, colour = NULL, size = 2, alpha = 0.8,
                           ellipse = FALSE, conf_level = 0.975, robust = FALSE,
                           distribution = "normal", hotelling = "none", k = 2,
                           t2_method = "f", label = NULL, biplot = FALSE, biplot_top = 10,
                           aspect_ratio = 0.7, title = NULL) {
  df <- embedding_data(data)
  variance <- embedding_variance(data)
  colour_quo <- rlang::enquo(colour)
  size_quo <- rlang::enquo(size)
  x <- embedding_column(rlang::enquo(x), df, "x")
  y <- embedding_column(rlang::enquo(y), df, "y")
  if (is.null(x) || is.null(y)) {
    axes <- embedding_axes(df)
    x <- x %||% setdiff(axes, y)[1]
    y <- y %||% setdiff(axes, x)[1]
  }
  if (is.na(x) || is.na(y) || x == y) {
    stop("'data' needs two numeric columns for the axes; give `x` and `y`.", call. = FALSE)
  }
  check_number(alpha, "alpha", lower = 0, upper = 1, lower_open = TRUE)
  check_flag(ellipse, "ellipse")
  check_flag(robust, "robust")
  check_flag(biplot, "biplot")
  check_count(biplot_top, "biplot_top", lower = 0)
  if (!is.null(aspect_ratio)) check_number(aspect_ratio, "aspect_ratio", lower = 0, lower_open = TRUE)
  ellipse_level <- check_conf_level(conf_level)
  t2_level <- ellipse_level
  distribution <- match.arg(distribution, c("normal", "hotelling"))
  hotelling <- match.arg(hotelling, c("none", "all", "group"))
  t2_method <- match.arg(t2_method, c("f", "beta"))

  colour_info <- embedding_values(colour_quo, df, "colour")
  colour_values <- colour_info$values
  colour_name <- colour_info$name
  size_info <- embedding_size(size_quo, df)
  label_values <- embedding_values(rlang::enquo(label), df, "label")$values
  plot_df <- data.frame(.x = df[[x]], .y = df[[y]])
  if (!is.null(colour_values)) plot_df$.colour <- colour_values
  if (!is.null(size_info$values)) plot_df$.size <- size_info$values
  grouped <- !is.null(colour_values) && !is.numeric(colour_values)
  if (hotelling == "group" && !grouped) {
    stop("`hotelling = \"group\"` needs a discrete `colour` to define the groups.", call. = FALSE)
  }

  # ellipses first: their insides are white, the rest of the panel grey
  layers <- list()
  inside <- list()
  if (ellipse) {
    ellipses <- embedding_ellipses(plot_df, grouped, ellipse_level, robust, distribution)
    if (!is.null(ellipses)) {
      outer <- ellipses[ellipses$.level == max(ellipse_level), , drop = FALSE]
      inside <- c(inside, list(outer_shapes(outer, if (grouped) ".colour" else NULL)))
      fill_alpha <- 0.12 / length(ellipse_level)
      layers <- c(layers, list(if (grouped) {
        ggplot2::geom_polygon(data = ellipses, ggplot2::aes(.data$x, .data$y, colour = .data$.colour,
                                                            fill = .data$.colour,
                                                            group = interaction(.data$.colour, .data$.level)),
                              alpha = fill_alpha, linewidth = 0.5, inherit.aes = FALSE,
                              show.legend = FALSE)
      } else {
        ggplot2::geom_polygon(data = ellipses, ggplot2::aes(.data$x, .data$y, group = .data$.level),
                              colour = "grey40", fill = "grey60", alpha = fill_alpha, linewidth = 0.5,
                              inherit.aes = FALSE)
      }))
    }
  }
  t2 <- NULL
  if (hotelling != "none") {
    check_count(k, "k", lower = 2)
    t2_cols <- unique(c(x, y, embedding_components(df, sum(vapply(df, is.numeric, logical(1))))))
    if (length(t2_cols) < k) {
      stop("'data' has fewer than ", k, " numeric columns for the T-squared.", call. = FALSE)
    }
    by_group <- hotelling == "group"
    t2 <- hotelling_t2(df, columns = t2_cols[seq_len(k)],
                       group = if (by_group) colour_values else NULL, conf_level = t2_level,
                       method = t2_method)
    t2$.flag <- t2[[paste0("outlier_", conf_label(max(t2_level)))]]
    limits <- hotelling_ellipses(plot_df, by_group, t2_level, t2_method)
    if (!is.null(limits)) {
      outer <- limits[limits$limit == t2_names(max(t2_level)), , drop = FALSE]
      inside <- c(inside, list(outer_shapes(outer, if (by_group) ".colour" else NULL)))
      if (by_group) {
        # the ellipses of the groups filled with their color
        layers <- c(layers, list(ggplot2::geom_polygon(
          data = limits, ggplot2::aes(.data$x, .data$y, fill = .data$.colour,
                                      group = interaction(.data$.colour, .data$limit)),
          alpha = 0.12 / length(t2_level), colour = NA, inherit.aes = FALSE, show.legend = FALSE
        )))
      }
      layers <- c(layers, list(if (by_group) {
        ggplot2::geom_path(data = limits, ggplot2::aes(.data$x, .data$y, colour = .data$.colour,
                                                       linetype = .data$limit,
                                                       group = interaction(.data$.colour, .data$limit)),
                           linewidth = 0.5, inherit.aes = FALSE, show.legend = FALSE)
      } else {
        ggplot2::geom_path(data = limits, ggplot2::aes(.data$x, .data$y, linetype = .data$limit),
                           colour = "grey30", linewidth = 0.5, inherit.aes = FALSE)
      }, ggplot2::scale_linetype_manual(values = t2_linetypes(t2_level), guide = "none")))
      # the limits are labeled on the ellipses (at their top), not in a legend
      tops <- t2_label_positions(limits, by_group)
      layers <- c(layers, list(if (by_group) {
        ggplot2::geom_text(data = tops, ggplot2::aes(.data$x, .data$y, label = .data$limit,
                                                     colour = .data$.colour),
                           hjust = -0.1, vjust = -0.3, size = 2.5, inherit.aes = FALSE,
                           show.legend = FALSE)
      } else {
        ggplot2::geom_text(data = tops, ggplot2::aes(.data$x, .data$y, label = .data$limit),
                           hjust = -0.1, vjust = -0.3, size = 2.5, colour = "grey30",
                           inherit.aes = FALSE)
      }))
    }
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(.data$.x, .data$.y))
  shapes <- do.call(rbind, inside)
  if (!is.null(shapes)) {
    p <- p + ggplot2::geom_polygon(data = shapes, ggplot2::aes(.data$x, .data$y, group = .data$.shape),
                                   fill = "white", colour = NA, inherit.aes = FALSE)
  }
  for (layer in layers) p <- p + layer
  # the axes through the origin, when it lies in the range of the samples
  # (they would otherwise stretch the axes)
  if (min(plot_df$.y, na.rm = TRUE) <= 0 && max(plot_df$.y, na.rm = TRUE) >= 0) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.3)
  }
  if (min(plot_df$.x, na.rm = TRUE) <= 0 && max(plot_df$.x, na.rm = TRUE) >= 0) {
    p <- p + ggplot2::geom_vline(xintercept = 0, colour = "black", linewidth = 0.3)
  }

  loadings <- NULL
  if (biplot) {
    loadings <- embedding_loadings(data, x, y, plot_df, biplot_top)
    p <- p + ggplot2::geom_point(data = loadings$all, ggplot2::aes(.data$x, .data$y),
                                 colour = "grey45", size = 0.6, alpha = 0.35, inherit.aes = FALSE)
  }
  point_aes <- ggplot2::aes()
  if (!is.null(colour_values)) point_aes$colour <- quote(.data$.colour)
  if (!is.null(size_info$values)) point_aes$size <- quote(.data$.size)
  point_args <- list(mapping = point_aes, alpha = alpha)
  if (is.null(colour_values)) point_args$colour <- "#1f4e79"
  if (is.null(size_info$values)) point_args$size <- size_info$constant
  p <- p + do.call(ggplot2::geom_point, point_args)
  if (!is.null(colour_values)) {
    p <- p + if (is.numeric(colour_values)) {
      ggplot2::scale_colour_viridis_c(name = colour_name)
    } else if (length(unique(colour_values)) <= 8) {
      ggplot2::scale_colour_brewer(name = colour_name, palette = "Dark2",
                                   aesthetics = c("colour", "fill"))
    } else {
      ggplot2::scale_colour_discrete(name = colour_name, aesthetics = c("colour", "fill"))
    }
    if (!is.numeric(colour_values)) {
      # keys of one size, whatever the size of the points
      p <- p + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2.5)),
                               fill = "none")
    }
  }
  if (!is.null(size_info$values)) {
    p <- p + ggplot2::scale_size_continuous(range = c(1, 6), name = size_info$name,
                                            breaks = three_breaks)
  }
  if (!is.null(loadings)) {
    top <- loadings$top
    if (nrow(top) > 0) {
      p <- p +
        ggplot2::geom_segment(data = top, ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
                              colour = "#7f0000", linewidth = 0.4, inherit.aes = FALSE,
                              arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"))) +
        ggplot2::geom_segment(data = top[top$moved, , drop = FALSE],
                              ggplot2::aes(x = .data$x, y = .data$y, xend = .data$lx, yend = .data$ly),
                              colour = "#7f000080", linewidth = 0.2, inherit.aes = FALSE) +
        ggplot2::geom_text(data = top, ggplot2::aes(.data$lx, .data$ly, label = .data$label),
                           colour = "#7f0000", size = 2.6, inherit.aes = FALSE)
    }
  }
  if (!is.null(t2) && any(t2$.flag)) {
    out <- plot_df[t2$sample[t2$.flag], , drop = FALSE]
    out$.label <- if (is.null(label_values)) t2$sample[t2$.flag] else
      label_values[t2$sample[t2$.flag]]
    circle <- if (is.null(size_info$values)) size_info$constant + 2.5 else 7
    p <- p +
      ggplot2::geom_point(data = out, ggplot2::aes(.data$.x, .data$.y), shape = 21, size = circle,
                          colour = "#c0392b", stroke = 0.8, inherit.aes = FALSE) +
      ggplot2::geom_text(data = out, ggplot2::aes(.data$.x, .data$.y, label = .data$.label),
                         vjust = -1.1, size = 3, colour = "#c0392b", inherit.aes = FALSE)
  }
  subtitle <- if (!is.null(t2)) {
    sprintf("Hotelling T² (%d components%s): %d sample(s) beyond the %s%% limit",
            k, if (hotelling == "group") ", within groups" else "", sum(t2$.flag),
            conf_label(max(t2_level)))
  }
  if (!is.null(loadings)) {
    fx <- loadings$factor[["x"]]
    fy <- loadings$factor[["y"]]
    p <- p +
      ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis(~ . / fx, name = paste(x, "loading"))) +
      ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . / fy, name = paste(y, "loading")))
  }
  # SIMCA style: grey outside the ellipses, white inside
  p +
    ggplot2::labs(x = axis_title(x, variance), y = axis_title(y, variance), title = title,
                  subtitle = subtitle) +
    ggplot2::theme_grey() +
    ggplot2::theme(
      aspect.ratio = aspect_ratio,
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = if (is.null(shapes)) "white" else "grey90",
                                               colour = "black", linewidth = 0.3),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "right"
    ) +
    compact_legend()
}

# ---- internals ---------------------------------------------------------------

# Legend labels and line types of the T-squared ellipses: the highest level
# solid, the others dashed, dotted, ...
t2_names <- function(levels) paste0("T\u00b2 ", conf_label(levels), "%")
t2_linetypes <- function(levels) {
  types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  stats::setNames(types[rev(seq_along(levels))], t2_names(levels))
}

# Hotelling's T-squared ellipses of the groups or of all samples, at each level.
hotelling_ellipses <- function(plot_df, by_group, levels, method) {
  rlang::check_installed("HotellingEllipse", version = "1.3.0",
                         reason = "to draw Hotelling's T-squared ellipses.")
  df <- plot_df[stats::complete.cases(plot_df[c(".x", ".y")]), , drop = FALSE]
  labels <- t2_names(levels)
  # contours T2 = limit, rotated to the covariance of the two components
  ellipse_of <- function(d) {
    do.call(rbind, lapply(seq_along(levels), function(i) {
      e <- as.data.frame(HotellingEllipse::ellipseCoord(d[c(".x", ".y")], conf.limit = levels[i],
                                                        method = method))
      e$limit <- factor(labels[i], levels = labels)
      e
    }))
  }
  if (!by_group) {
    return(if (nrow(df) > 3) ellipse_of(df))
  }
  groups <- split(df, as.character(df$.colour))
  groups <- groups[vapply(groups, nrow, integer(1)) > 3]
  if (length(groups) == 0) return(NULL)
  out <- do.call(rbind, lapply(names(groups), function(g) {
    e <- ellipse_of(groups[[g]])
    e$.colour <- g
    e
  }))
  if (is.factor(df$.colour)) out$.colour <- factor(out$.colour, levels = levels(df$.colour))
  out
}

# Share of the variance of each component of a PCA fit (named like the
# score columns), or NULL: of the total variance for a prcomp fit, of the
# variance of the k components for a robust fit.
embedding_variance <- function(data) {
  if (inherits(data, "prcomp")) {
    v <- data$sdev^2
    return(stats::setNames(v / sum(v), colnames(data$x) %||% paste0("PC", seq_along(v)))[seq_len(ncol(data$x))])
  }
  if (inherits(data, "specproc_robpca")) {
    v <- data$eigenvalues
    if (length(v) == 0 || (!is.null(data$scores) && length(v) != ncol(data$scores))) return(NULL)
    return(stats::setNames(v / sum(v), colnames(data$scores) %||% paste0("PC", seq_along(v))))
  }
  NULL
}

axis_title <- function(axis, variance) {
  if (is.null(variance) || !axis %in% names(variance)) return(axis)
  sprintf("%s (%.1f%%)", axis, 100 * variance[[axis]])
}

# Upper-right point of each T-squared ellipse (per level, and per group),
# for its label: away from the vertical line through the origin.
t2_label_positions <- function(limits, by_group) {
  keys <- if (by_group) interaction(limits$.colour, limits$limit, drop = TRUE) else limits$limit
  span_x <- diff(range(limits$x))
  span_y <- diff(range(limits$y))
  do.call(rbind, lapply(split(limits, keys, drop = TRUE), function(e) {
    e[which.max(e$x / span_x + e$y / span_y), , drop = FALSE]
  }))
}

# Three rounded breaks (minimum, middle, maximum) for a size legend.
three_breaks <- function(limits) {
  unique(signif(seq(limits[1], limits[2], length.out = 3), 2))
}

# Smaller legend text, titles and keys, closer together.
compact_legend <- function() {
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 8),
    legend.title = ggplot2::element_text(size = 9),
    legend.key.size = ggplot2::unit(0.35, "cm"),
    legend.spacing.y = ggplot2::unit(0.15, "cm"),
    legend.margin = ggplot2::margin(2, 2, 2, 2)
  )
}

# The outermost ellipses (one per group), as polygons to fill in white.
outer_shapes <- function(ellipses, group) {
  key <- if (is.null(group)) "all" else as.character(ellipses[[group]])
  data.frame(x = ellipses$x, y = ellipses$y, .shape = paste(key, "outer", sep = "_"))
}

# A constant point size, or one value per sample to map.
embedding_size <- function(quo, df) {
  expr <- rlang::quo_get_expr(quo)
  if (is.numeric(expr) && length(expr) == 1) {
    check_number(expr, "size", lower = 0, lower_open = TRUE)
    return(list(constant = expr, values = NULL, name = NULL))
  }
  info <- embedding_values(quo, df, "size")
  if (is.null(info$values)) return(list(constant = 2, values = NULL, name = NULL))
  if (length(info$values) == 1 && is.numeric(info$values)) {
    check_number(info$values, "size", lower = 0, lower_open = TRUE)
    return(list(constant = info$values, values = NULL, name = NULL))
  }
  if (!is.numeric(info$values)) {
    stop("`size` must be a number, or a numeric column or vector with one value per sample.",
         call. = FALSE)
  }
  list(constant = NULL, values = info$values, name = info$name)
}

# Loadings of the two components, each scaled to the range of its scores:
# all the variables, and the `top` longest in the plane of the plot (for
# spectra, the peaks of the length along the wavelength, one per line).
embedding_loadings <- function(data, x, y, plot_df, top) {
  p <- if (inherits(data, "prcomp")) data$rotation else if (inherits(data, "specproc_robpca")) data$loadings
  if (is.null(p)) {
    stop("`biplot = TRUE` needs a prcomp fit or a robust PCA object (robpca(), rospca(), ",
         "macropca()).", call. = FALSE)
  }
  if (!all(c(x, y) %in% colnames(p))) {
    stop("`biplot = TRUE` needs axes that are components of the model (", x, ", ", y, ").",
         call. = FALSE)
  }
  load <- data.frame(x = p[, x], y = p[, y])
  variables <- rownames(p) %||% as.character(seq_len(nrow(p)))
  wavelength <- names_to_wavelength(variables)
  spectral <- length(wavelength) == length(variables)
  load$label <- if (spectral) format_wavelength(wavelength, wavelength_digits(wavelength)) else variables
  factor <- c(
    x = 0.8 * max(abs(plot_df$.x), na.rm = TRUE) / max(abs(load$x), .Machine$double.eps),
    y = 0.8 * max(abs(plot_df$.y), na.rm = TRUE) / max(abs(load$y), .Machine$double.eps)
  )
  load$x <- load$x * factor[["x"]]
  load$y <- load$y * factor[["y"]]
  norm <- sqrt((load$x / factor[["x"]])^2 + (load$y / factor[["y"]])^2)
  candidates <- if (spectral && length(variables) > 20) {
    local_maxima(norm, wavelength_segments(wavelength), span = 5)
  } else {
    seq_along(norm)
  }
  chosen <- candidates[order(norm[candidates], decreasing = TRUE)][seq_len(min(top, length(candidates)))]
  top_load <- biplot_label_positions(load[chosen, , drop = FALSE], plot_df)
  list(all = load, top = top_load, factor = factor)
}

# Label positions beyond the arrow tips, pushed outward along the arrow until
# they do not overlap the labels already placed (in units of the panel).
biplot_label_positions <- function(top, plot_df) {
  span_x <- diff(range(c(plot_df$.x, top$x), na.rm = TRUE))
  span_y <- diff(range(c(plot_df$.y, top$y), na.rm = TRUE))
  top$lx <- top$x * 1.08
  top$ly <- top$y * 1.08
  top$moved <- FALSE
  for (i in seq_len(nrow(top))[-1]) {
    for (step in 0:10) {
      m <- 1.08 + 0.12 * step
      lx <- top$x[i] * m
      ly <- top$y[i] * m
      width <- 0.012 * (nchar(top$label[i]) + nchar(top$label[seq_len(i - 1)])) / 2
      clash <- abs(lx - top$lx[seq_len(i - 1)]) / span_x < width &
        abs(ly - top$ly[seq_len(i - 1)]) / span_y < 0.045
      if (!any(clash)) break
    }
    top$lx[i] <- lx
    top$ly[i] <- ly
    top$moved[i] <- step > 0
  }
  top
}

# Coordinates of the confidence ellipses of the groups (or of all samples).
embedding_ellipses <- function(plot_df, grouped, levels, robust, distribution) {
  rlang::check_installed("ConfidenceEllipse", reason = "to draw confidence ellipses.")
  df <- plot_df[stats::complete.cases(plot_df), , drop = FALSE]
  ellipse_of <- function(d) {
    do.call(rbind, lapply(levels, function(l) {
      e <- as.data.frame(ConfidenceEllipse::confidence_ellipse(
        d, ".x", ".y", conf_level = l, robust = robust, distribution = distribution
      ))
      e$.level <- l
      e
    }))
  }
  if (!grouped) {
    if (nrow(df) < 4) return(NULL)
    return(ellipse_of(df))
  }
  groups <- split(df, as.character(df$.colour))
  small <- names(groups)[vapply(groups, nrow, integer(1)) < 4]
  if (length(small) > 0) {
    warning("No ellipse for the group(s) with fewer than 4 samples: ",
            paste(small, collapse = ", "), ".", call. = FALSE)
  }
  groups <- groups[setdiff(names(groups), small)]
  if (length(groups) == 0) return(NULL)
  out <- do.call(rbind, lapply(names(groups), function(g) {
    e <- as.data.frame(ellipse_of(groups[[g]]))
    e$.colour <- g
    e
  }))
  if (is.factor(df$.colour)) out$.colour <- factor(out$.colour, levels = levels(df$.colour))
  out
}

embedding_data <- function(data) {
  if (inherits(data, "prcomp")) return(as.data.frame(data$x))
  if (is.list(data) && !is.data.frame(data) && is.matrix(data$scores)) {
    scores <- data$scores
    if (is.null(colnames(scores))) colnames(scores) <- paste0("PC", seq_len(ncol(scores)))
    return(as.data.frame(scores))
  }
  if (is.matrix(data)) {
    data <- as.data.frame(data)
    if (all(grepl("^V[0-9]+$", names(data)))) names(data) <- paste0("Dim", seq_along(data))
    return(data)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame, a matrix, a prcomp fit or a robust PCA object.",
         call. = FALSE)
  }
  as.data.frame(data)
}

embedding_column <- function(quo, df, arg) {
  if (rlang::quo_is_null(quo)) return(NULL)
  name <- rlang::as_name(quo)
  if (!name %in% names(df) || !is.numeric(df[[name]])) {
    stop("`", arg, "` must be a numeric column of 'data'.", call. = FALSE)
  }
  name
}

# The first two embedding coordinates, or else the first two numeric columns.
embedding_axes <- function(df) {
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  coords <- grep("^(UMAP|PC|Comp|Dim|tSNE|TSNE|IC|LV)_?[0-9]+$", numeric_cols, value = TRUE,
                 ignore.case = TRUE)
  axes <- if (length(coords) >= 2) coords else numeric_cols
  c(axes, NA_character_, NA_character_)[1:2]
}
