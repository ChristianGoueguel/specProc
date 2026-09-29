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
#' [ConfidenceEllipse::confidence_ellipse()]. It covers the region expected
#' to hold `conf_level` of the samples of the group if they follow a
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
#' level of `t2_level` (95% and 99% by default) are drawn for all the
#' samples (contours of \eqn{T^2} on the two components shown, from their
#' mean and covariance, with [HotellingEllipse::ellipseCoord()]), and the
#' samples beyond the limit at the highest level of \eqn{T^2} on `k`
#' components (see [hotelling_t2()]) are circled and labeled: the classical
#' outlier limits of a score plot. With `hotelling
#' = "group"`, each group of a discrete `colour` gets its own ellipses and
#' limits, which flags the samples atypical of their own group. The
#' ellipses are drawn for the two components shown, while the limits use
#' `k` components, so with `k > 2` a flagged sample can lie inside the
#' ellipses. \eqn{T^2} suits linear scores such as PCA or PLS; on a UMAP
#' map, whose distances are not meaningful, prefer `ellipse`.
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
#' @param size,alpha The size and opacity of the points.
#' @param ellipse A logical: draw confidence ellipses (`FALSE`, default).
#'   Needs the ConfidenceEllipse package.
#' @param conf_level The confidence level of the ellipses. Default is 0.95.
#' @param robust A logical: robust ellipses (`FALSE`, default).
#' @param distribution The quantile of the ellipses: `"normal"` (default,
#'   chi-square) or `"hotelling"`.
#' @param hotelling Hotelling's \eqn{T^2} ellipses and outliers: `"none"`
#'   (default), `"all"` (all samples) or `"group"` (within the groups of a
#'   discrete `colour`). Needs the HotellingEllipse package (1.3.0 or
#'   later).
#' @param k The number of components of \eqn{T^2}: the two axes, then the
#'   next embedding coordinates. Default is 2.
#' @param t2_level The confidence level(s) of the \eqn{T^2} ellipses: one or
#'   more values between 0 and 1. Default is `c(0.95, 0.99)`. The samples
#'   are flagged at the highest level.
#' @param t2_method The distribution of the \eqn{T^2} limits: `"f"`
#'   (default) or `"beta"` (see [hotelling_t2()]).
#' @param label The labels of the outlying samples: a column of `data` or a
#'   vector with one value per sample. By default, their row numbers.
#' @param title The plot title.
#'
#' @return A ggplot object.
#' @seealso [hotelling_t2()], [plot_outlier_map()], [robpca()]
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
                           ellipse = FALSE, conf_level = 0.95, robust = FALSE,
                           distribution = "normal", hotelling = "none", k = 2,
                           t2_level = c(0.95, 0.99), t2_method = "f", label = NULL,
                           title = NULL) {
  df <- embedding_data(data)
  colour_quo <- rlang::enquo(colour)
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
  check_number(size, "size", lower = 0, lower_open = TRUE)
  check_number(alpha, "alpha", lower = 0, upper = 1, lower_open = TRUE)
  check_flag(ellipse, "ellipse")
  check_flag(robust, "robust")
  check_number(conf_level, "conf_level", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  distribution <- match.arg(distribution, c("normal", "hotelling"))
  hotelling <- match.arg(hotelling, c("none", "all", "group"))
  t2_level <- check_conf_level(t2_level)
  t2_method <- match.arg(t2_method, c("f", "beta"))

  colour_info <- embedding_values(colour_quo, df, "colour")
  colour_values <- colour_info$values
  colour_name <- colour_info$name
  label_values <- embedding_values(rlang::enquo(label), df, "label")$values
  plot_df <- data.frame(.x = df[[x]], .y = df[[y]])
  if (!is.null(colour_values)) plot_df$.colour <- colour_values
  grouped <- !is.null(colour_values) && !is.numeric(colour_values)
  if (hotelling == "group" && !grouped) {
    stop("`hotelling = \"group\"` needs a discrete `colour` to define the groups.", call. = FALSE)
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(.data$.x, .data$.y))
  if (ellipse) {
    ellipses <- embedding_ellipses(plot_df, grouped, conf_level, robust, distribution)
    if (!is.null(ellipses)) {
      p <- p + if (grouped) {
        ggplot2::geom_polygon(data = ellipses, ggplot2::aes(.data$x, .data$y, colour = .data$.colour,
                                                            fill = .data$.colour),
                              alpha = 0.12, linewidth = 0.5, inherit.aes = FALSE, show.legend = FALSE)
      } else {
        ggplot2::geom_polygon(data = ellipses, ggplot2::aes(.data$x, .data$y), colour = "grey40",
                              fill = "grey60", alpha = 0.12, linewidth = 0.5, inherit.aes = FALSE)
      }
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
      p <- p + if (by_group) {
        ggplot2::geom_path(data = limits, ggplot2::aes(.data$x, .data$y, colour = .data$.colour,
                                                       linetype = .data$limit,
                                                       group = interaction(.data$.colour, .data$limit)),
                           linewidth = 0.5, inherit.aes = FALSE)
      } else {
        ggplot2::geom_path(data = limits, ggplot2::aes(.data$x, .data$y, linetype = .data$limit),
                           colour = "grey30", linewidth = 0.5, inherit.aes = FALSE)
      }
      p <- p + ggplot2::scale_linetype_manual(values = t2_linetypes(t2_level), name = NULL)
    }
  }
  if (is.null(colour_values)) {
    p <- p + ggplot2::geom_point(size = size, alpha = alpha, colour = "#1f4e79")
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = .data$.colour), size = size, alpha = alpha)
    p <- p + if (is.numeric(colour_values)) {
      ggplot2::scale_colour_viridis_c(name = colour_name)
    } else if (length(unique(colour_values)) <= 8) {
      ggplot2::scale_colour_brewer(name = colour_name, palette = "Dark2",
                                   aesthetics = c("colour", "fill"))
    } else {
      ggplot2::scale_colour_discrete(name = colour_name, aesthetics = c("colour", "fill"))
    }
  }
  if (!is.null(t2) && any(t2$.flag)) {
    out <- plot_df[t2$sample[t2$.flag], , drop = FALSE]
    out$.label <- if (is.null(label_values)) t2$sample[t2$.flag] else
      label_values[t2$sample[t2$.flag]]
    p <- p +
      ggplot2::geom_point(data = out, ggplot2::aes(.data$.x, .data$.y), shape = 21, size = size + 2.5,
                          colour = "#c0392b", stroke = 0.8, inherit.aes = FALSE) +
      ggplot2::geom_text(data = out, ggplot2::aes(.data$.x, .data$.y, label = .data$.label),
                         vjust = -1.1, size = 3, colour = "#c0392b", inherit.aes = FALSE)
  }
  subtitle <- if (!is.null(t2)) {
    sprintf("Hotelling T\u00b2 (%d components%s): %d sample(s) beyond the %s%% limit",
            k, if (hotelling == "group") ", within groups" else "", sum(t2$.flag),
            conf_label(max(t2_level)))
  }
  p +
    ggplot2::labs(x = x, y = y, title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right")
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

# Coordinates of the confidence ellipses of the groups (or of all samples).
embedding_ellipses <- function(plot_df, grouped, conf_level, robust, distribution) {
  rlang::check_installed("ConfidenceEllipse", reason = "to draw confidence ellipses.")
  df <- plot_df[stats::complete.cases(plot_df), , drop = FALSE]
  ellipse_of <- function(d) {
    ConfidenceEllipse::confidence_ellipse(d, ".x", ".y", conf_level = conf_level, robust = robust,
                                          distribution = distribution)
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
