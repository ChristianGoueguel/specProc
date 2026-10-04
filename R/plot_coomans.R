#' @title Coomans Plot of a Robust SIMCA Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Plots the distance of each observation to one class of a model fitted by
#' [rsimca()] against its distance to another class (Coomans plot), with the
#' class boundaries and the classification boundary. It shows which
#' observations belong to one class, to both, or to neither, and which are
#' misclassified.
#'
#' @details
#' The distances are the combined distances \eqn{D_j} of the classification
#' rule of [rsimca()], built from the score and orthogonal distances to the
#' robust PCA model of each class, divided by their cut-offs. The dashed
#' lines are at 1: an observation beyond 1 for a class is beyond at least one
#' of the cut-offs of this class, and an observation within both cut-offs of
#' a class is within 1. The diagonal is the classification boundary: the
#' observations above it are assigned to the class of the horizontal axis,
#' those below it to the class of the vertical axis.
#'
#' The four corners of the plot hold the observations close to the class of
#' the horizontal axis only (top left), to the class of the vertical axis
#' only (bottom right), to both (bottom left, where the classes overlap), and
#' to neither (top right, outliers for both classes, or, with more than two
#' classes, members of another class).
#'
#' The points are filled by their class: the training class for the training
#' observations, and for new observations (`newdata`, triangles) the classes
#' given in `group`, or `"unknown"`. Observations of a class on the wrong
#' side of the diagonal are misclassified.
#'
#' @param object An object returned by [rsimca()].
#' @param newdata Optional new observations to add to the plot (a numeric
#'   matrix or data frame with the training variables).
#' @param group Optional classes of `newdata`, to fill their points.
#' @param classes The two classes to plot, on the horizontal and vertical
#'   axes. Default is the first two classes of the model.
#' @param labels The number of observations farthest from both classes to
#'   label, among those beyond 1 for both (by their row names, or row
#'   numbers). Default is 3; use 0 for no labels.
#' @param log If `TRUE`, use logarithmic axes, which spread out the
#'   observations close to the classes when others are far away. Default is
#'   `FALSE`.
#' @param shade If `TRUE`, shade the regions of the observations close to
#'   both classes and to neither, and name the four regions. Default is
#'   `FALSE`.
#' @param title The plot title.
#' @param ... Further arguments passed to [ggplot2::geom_point()] to style
#'   the points, such as `alpha` (default 0.85), `size` (2.2), `stroke`
#'   (0.4) or `colour` (the outline, `"black"`). `size` can also be a numeric
#'   vector with one value per observation (the training observations, then
#'   those of `newdata`), to vary the size of the points, with a legend.
#'
#' @return A ggplot object.
#'
#' @references
#'  - Vanden Branden, K., Hubert, M. (2005). Robust classification in high
#'    dimensions based on the SIMCA method. Chemometrics and Intelligent
#'    Laboratory Systems, 79(1-2):10-21.
#'
#' @seealso [rsimca()], [plot_outlier_map()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]
#' level <- cut(forageLIBS$Ca, c(-Inf, 0.6, Inf), labels = c("low", "high"))
#' set.seed(1)
#' fit <- rsimca(spectra[1:300, ], level[1:300], ncomp = 3)
#' plot_coomans(fit)
#' plot_coomans(fit, newdata = spectra[301:368, ], group = level[301:368],
#'              log = TRUE, shade = TRUE)
plot_coomans <- function(object, newdata = NULL, group = NULL, classes = NULL, labels = 3,
                         log = FALSE, shade = FALSE, title = NULL, ...) {
  if (!inherits(object, "specproc_rsimca")) {
    stop("'object' must be returned by rsimca().", call. = FALSE)
  }
  check_count(labels, "labels", lower = 0)
  check_flag(log, "log")
  check_flag(shade, "shade")
  lev <- object$levels
  classes <- as.character(classes %||% lev[1:2])
  if (length(classes) != 2 || !all(classes %in% lev) || classes[1] == classes[2]) {
    stop("'classes' must be two different classes of the model: ",
         paste0("`", lev, "`", collapse = ", "), ".", call. = FALSE)
  }
  if (is.null(newdata) && !is.null(group)) {
    stop("'group' gives the classes of 'newdata'.", call. = FALSE)
  }

  df <- data.frame(id = as.character(seq_along(object$fitted)),
                   x = object$distances[[classes[1]]], y = object$distances[[classes[2]]],
                   class = as.character(object$group), set = "calibration",
                   stringsAsFactors = FALSE)
  if (!is.null(newdata)) {
    d <- stats::predict(object, newdata, type = "distances")
    new_class <- if (is.null(group)) {
      rep("unknown", nrow(d))
    } else {
      if (length(group) != nrow(d)) {
        stop("'group' must have one class per row of 'newdata'.", call. = FALSE)
      }
      as.character(group)
    }
    df <- rbind(df, data.frame(id = rownames(newdata) %||% paste0("new", seq_len(nrow(d))),
                               x = d[[classes[1]]], y = d[[classes[2]]], class = new_class,
                               set = "new", stringsAsFactors = FALSE))
  }
  point <- map_point_args(rlang::enquos(...), nrow(df),
                          if (!is.null(newdata)) ", training then new observations" else "")
  coomans_plot(df, classes, lev, labels, log, shade, title %||%
                 paste0("RSIMCA Coomans plot (", classes[1], " and ", classes[2], ")"), point,
               show_set = !is.null(newdata))
}

coomans_plot <- function(df, classes, lev, labels, log, shade, title, point, show_set) {
  # farthest from both classes: the distance to the closer one
  df$radius <- pmin(df$x, df$y)
  df$label <- ""
  if (labels > 0) {
    most <- order(df$radius, decreasing = TRUE)[seq_len(min(labels, nrow(df)))]
    most <- most[df$radius[most] > 1]
    df$label[most] <- df$id[most]
  }
  if (!is.null(point$sizes)) df$.size <- point$sizes
  # the model classes first, then the other classes of `group`, then unknown
  others <- setdiff(unique(df$class), c(lev, "unknown"))
  levels_fill <- c(lev, others, if ("unknown" %in% df$class) "unknown")
  df$class <- factor(df$class, levels = levels_fill)
  df$set <- factor(df$set, levels = c("calibration", "new"))
  n_col <- length(levels_fill) - ("unknown" %in% levels_fill)
  palette <- if (n_col <= 8) {
    scales::brewer_pal(palette = "Dark2")(8)[seq_len(n_col)]
  } else {
    scales::hue_pal()(n_col)
  }
  palette <- stats::setNames(c(palette, if ("unknown" %in% levels_fill) "grey65"), levels_fill)

  cut <- 1
  if (log) {
    # log10 distances on linear axes labelled in the original units; zeros
    # are drawn at the smallest positive distance
    positive <- c(df$x, df$y)[c(df$x, df$y) > 0]
    floor_v <- if (length(positive)) min(positive) else 1
    df$x <- log10(pmax(df$x, floor_v))
    df$y <- log10(pmax(df$y, floor_v))
    cut <- 0
  }
  # the same range on both axes, so that the diagonal is at 45 degrees
  limits <- range(c(df$x, df$y, cut), finite = TRUE)
  # labels on the right of the points, or on their left near the right edge
  df$.hjust <- ifelse(df$x > limits[1] + 0.85 * diff(limits), 1.2, -0.2)

  point_args <- utils::modifyList(list(show.legend = c(fill = TRUE)), point$args)
  point_args$mapping <- ggplot2::aes(fill = .data$class, shape = .data$set)
  if (!is.null(point$size_name)) point_args$mapping$size <- quote(.data$.size)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y))
  if (shade) {
    rects <- data.frame(xmin = c(-Inf, cut), xmax = c(cut, Inf), ymin = c(-Inf, cut),
                        ymax = c(cut, Inf), alpha = c(0.06, 0.18))
    p <- p +
      ggplot2::geom_rect(data = rects, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                                    ymin = .data$ymin, ymax = .data$ymax),
                         alpha = rects$alpha, fill = "grey20", inherit.aes = FALSE) +
      ggplot2::annotate("text", x = c(-Inf, Inf, -Inf, Inf), y = c(Inf, -Inf, -Inf, Inf),
                        label = c(paste(classes[1], "only"), paste(classes[2], "only"), "both",
                                  "neither"),
                        hjust = c(-0.05, 1.05, -0.05, 1.05), vjust = c(1.6, -0.6, -0.6, 1.6),
                        size = 3, fontface = "italic", colour = "grey35")
  }
  p <- p +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey45", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = cut, linetype = "dashed", colour = "grey30") +
    ggplot2::geom_hline(yintercept = cut, linetype = "dashed", colour = "grey30") +
    do.call(ggplot2::geom_point, point_args) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label, hjust = .data$.hjust), size = 3,
                       vjust = -0.4, colour = "grey20", na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = palette, name = "Class", drop = TRUE,
                               guide = ggplot2::guide_legend(order = 1,
                                                             override.aes = list(shape = 21, size = 2.5))) +
    ggplot2::scale_shape_manual(values = c(calibration = 21, new = 24), name = NULL, drop = TRUE,
                                labels = c(calibration = "training", new = "new"),
                                guide = if (show_set) {
                                  ggplot2::guide_legend(order = 2, override.aes = list(size = 2.5))
                                } else "none") +
    ggplot2::labs(x = paste("Distance to", classes[1]), y = paste("Distance to", classes[2]),
                  title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    compact_legend()
  if (!is.null(point$size_name)) {
    p <- p + ggplot2::theme(legend.box = "vertical") +
      ggplot2::scale_size_continuous(range = c(1.2, 6), name = point$size_name, breaks = three_breaks,
                                     guide = ggplot2::guide_legend(order = 3))
  }
  x_args <- list(limits = limits, expand = ggplot2::expansion(mult = 0.05))
  y_args <- list(limits = limits,
                 expand = ggplot2::expansion(mult = c(0.05, if (any(df$label != "")) 0.08 else 0.05)))
  if (log) {
    log_breaks <- function(lim) log10(scales::breaks_log(n = 6)(10^lim))
    log_labels <- function(breaks) vapply(10^breaks, format, character(1), digits = 3, scientific = FALSE)
    log_args <- list(breaks = log_breaks, labels = log_labels, minor_breaks = NULL)
    x_args <- c(x_args, log_args)
    y_args <- c(y_args, log_args)
  }
  finish_title(p +
    do.call(ggplot2::scale_x_continuous, x_args) +
    do.call(ggplot2::scale_y_continuous, y_args))
}
