#' @title Univariate Representation of Multivariate Outliers
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function creates a visual representation of multivariate outliers using
#' a univariate plot. It uses robust covariance estimation methods to identify
#' outliers, and shows them on each variable, or by their robust distance.
#'
#' @details
#' The robust location and scatter of the data are estimated by the Minimum
#' Covariance Determinant (MCD, Rousseeuw and Van Driessen, 1999), from which
#' the robust Mahalanobis distance of each sample is computed. The outliers
#' are the samples whose squared distance exceeds the **adaptive cutoff** of
#' Filzmoser, Garrett and Reimann (2005): the tail of the distances is
#' compared with the \eqn{\chi^2_p} distribution beyond its \eqn{1 - \alpha}
#' quantile, and the cutoff is moved out to where they depart from it. When
#' they do not depart from it, no sample is an outlier: unlike a fixed
#' quantile, which flags about \eqn{\alpha} of clean data, the adaptive
#' cutoff flags (nearly) none. `cutoff = "quantile"` uses the fixed
#' \eqn{\chi^2_{p, 1-\alpha}} quantile instead. The method follows the
#' functions `arw()` and `aq.plot()` of the mvoutlier package (its
#' `uni.plot()`, which this plot follows, flags the samples beyond the fixed
#' quantile).
#'
#' The adaptive cutoff exists only when more distances exceed the quantile
#' than clean data would give (a proportion of about \eqn{0.24/\sqrt{n}}):
#' a single outlier in a small sample, however far, is then not flagged. It
#' stands out above the dashed quantile in the distance plot; use
#' `cutoff = "quantile"` to flag it.
#'
#' **Robust z-scores** (`type = "scores"`, the default). Each variable is
#' drawn in its own panel, standardized by the adaptive reweighted location
#' and scale (the mean and standard deviation of the samples that are not
#' outliers). Each point is a sample, at the same horizontal position in
#' every panel, so that a sample can be followed from one variable to the
#' next. The outliers are flagged on all the variables jointly: they need
#' not be extreme in any single variable, and a sample beyond the dashed
#' univariate limits (\eqn{\pm 2.5}) need not be a multivariate outlier.
#'
#' **Robust distances** (`type = "distance"`). The robust distance of each
#' sample, in the order of the rows, with the cutoff (solid) and the
#' \eqn{\chi^2_{p, 1-\alpha}} quantile (dashed).
#'
#' Rows with missing values are left out, with a message.
#'
#' @references
#' - Filzmoser, P., Garrett, R. G., Reimann, C. (2005). Multivariate outlier
#'   detection in exploration geochemistry. Computers & Geosciences,
#'   31(5):579-587
#' - Rousseeuw, P. J., Van Driessen, K. (1999). A fast algorithm for the
#'   minimum covariance determinant estimator. Technometrics, 41(3):212-223
#'
#' @param x A matrix or data frame of numeric variables, with optionally the
#'   column `id`.
#' @param quan A numeric value, between 0.5 and 1, the proportion of the
#'   observations used for the MCD estimates. Default is 0.5 (the largest
#'   breakdown point).
#' @param alpha A numeric value between 0 and 1: the quantile
#'   \eqn{\chi^2_{p, 1-\alpha}} from which the adaptive cutoff is searched
#'   (or the cutoff itself with `cutoff = "quantile"`). Default is 0.025.
#' @param plot A logical: plot (`TRUE`, default) or return the table of the
#'   samples.
#' @param id Optional column of `x` (unquoted or as a string) identifying the
#'   samples, such as sample names: it names the outliers in the table and
#'   with `label_outliers`.
#' @param cutoff `"adaptive"` (default), the adaptive cutoff of Filzmoser et
#'   al. (2005), or `"quantile"`, the fixed \eqn{\chi^2_{p, 1-\alpha}}
#'   quantile.
#' @param type `"scores"` (default), the robust z-scores of each variable, or
#'   `"distance"`, the robust distance of each sample.
#' @param color_by The colors of the points: `"outlier"` (default), the
#'   outliers in red and the others in grey; `"distance"`, the robust
#'   distance; or `"both"`, the distance, with the outliers as triangles.
#' @param label_outliers A logical: label the outliers with their `id`, else
#'   their row number (`FALSE`, default). In the panels of robust z-scores,
#'   only the outliers beyond the univariate limits of the panel are labeled;
#'   the distance plot names them all.
#' @param ylab The title of the value axis. Default is "Robust z-score", or
#'   "Robust distance" with `type = "distance"`.
#' @param title The plot title. A long title is split into a title and a
#'   subtitle.
#' @param caption `TRUE` (default), a caption saying how the outliers are
#'   flagged and how to read the plot; `FALSE`, no caption; or a caption of
#'   your own.
#' @param base_size The size of the text, in points. Default is 11; use the
#'   text size of the journal (often 7 to 9 points) for a figure saved at its
#'   printed size.
#' @param show.outlier,show.mahal `r lifecycle::badge("deprecated")` Use
#'   `color_by` (`"outlier"`, `"distance"` or `"both"`), and `plot = FALSE`
#'   for the table.
#'
#' @return A `ggplot` object, or with `plot = FALSE` a tibble with one row per
#'   sample (without missing values): its `row` in `x`, its `id`, its robust
#'   Mahalanobis distance `mahalanobis`, the `cutoff` (on the same scale),
#'   `outlier`, its `weight` in the adaptive reweighted estimates (0 for the
#'   samples at or beyond the adaptive cutoff, 1 for the others), and its
#'   robust z-score on each variable.
#'
#' @seealso [adjusted_boxplot()] and [generalized_boxplot()] for univariate
#'   outliers.
#'
#' @export plot_outliers
#'
#' @examples
#' data(forageLIBS)
#' # mineral contents (%) of the forage samples
#' contents <- forageLIBS[c("Measurement", "Ca", "Mg", "P", "K")]
#' plot_outliers(contents, id = Measurement)
#'
#' # the robust distance of each sample, the outliers named
#' plot_outliers(contents, id = Measurement, type = "distance", label_outliers = TRUE)
#'
#' # colored by robust distance, the outliers as triangles
#' plot_outliers(contents, id = Measurement, color_by = "both")
#'
#' # the table of the samples
#' res <- plot_outliers(contents, id = Measurement, plot = FALSE)
#' res[res$outlier, ]
plot_outliers <- function(x, quan = 1/2, alpha = 0.025, plot = TRUE, id = NULL,
                          cutoff = c("adaptive", "quantile"), type = c("scores", "distance"),
                          color_by = c("outlier", "distance", "both"), label_outliers = FALSE,
                          ylab = NULL, title = NULL, caption = TRUE, base_size = 11,
                          show.outlier = deprecated(), show.mahal = deprecated()) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("'x' must be matrix or data.frame")
  }
  if (!is.numeric(quan) || length(quan) != 1 || is.na(quan) || quan < 0.5 || quan > 1) {
    stop("'quan' must be a numeric value between 0.5 and 1")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a numeric value between 0 and 1")
  }
  cutoff <- match.arg(cutoff)
  type <- match.arg(type)
  color_by <- match.arg(color_by)
  # the former show.outlier and show.mahal
  if (lifecycle::is_present(show.outlier) || lifecycle::is_present(show.mahal)) {
    lifecycle::deprecate_warn(
      "0.9.0", I("`plot_outliers(show.outlier, show.mahal)`"),
      details = "Use `color_by` (\"outlier\", \"distance\" or \"both\"), and `plot = FALSE` for the table."
    )
    outlier_flag <- if (lifecycle::is_present(show.outlier)) show.outlier else TRUE
    mahal_flag <- if (lifecycle::is_present(show.mahal)) show.mahal else FALSE
    if (!is.logical(outlier_flag) || length(outlier_flag) != 1 || is.na(outlier_flag)) {
      stop("'show.outlier' must be of type boolean (TRUE or FALSE)")
    }
    if (!is.logical(mahal_flag) || length(mahal_flag) != 1 || is.na(mahal_flag)) {
      stop("'show.mahal' must be of type boolean (TRUE or FALSE)")
    }
    if (!outlier_flag && !mahal_flag) {
      plot <- FALSE
    } else {
      color_by <- if (outlier_flag && mahal_flag) "both" else if (mahal_flag) "distance" else "outlier"
    }
  }
  check_flag(plot, "plot")
  check_flag(label_outliers, "label_outliers")
  check_number(base_size, "base_size", lower = 0, lower_open = TRUE)
  if (!(isTRUE(caption) || isFALSE(caption) ||
        (is.character(caption) && length(caption) == 1 && !is.na(caption)))) {
    stop("'caption' must be TRUE, FALSE or a character string.", call. = FALSE)
  }

  x <- as.data.frame(x)
  id_name <- boxplot_column(rlang::enquo(id), x, "id")
  vars <- setdiff(names(x), id_name)
  if (length(vars) < 2) {
    stop("'x' must be at least two-dimensional")
  }
  values <- as_numeric_matrix(x[vars], "x")
  complete <- stats::complete.cases(values)
  if (!all(complete)) {
    n_missing <- sum(!complete)
    message(n_missing, if (n_missing == 1) " row" else " rows", " with missing values ",
            if (n_missing == 1) "is" else "are", " left out.")
  }
  rows <- which(complete)
  values <- values[rows, , drop = FALSE]
  res <- outlier_scores(values, quan, alpha, cutoff)

  tbl <- tibble::tibble(row = rows)
  if (!is.null(id_name)) tbl$id <- x[[id_name]][rows]
  tbl$mahalanobis <- res$distance
  tbl$cutoff <- res$cutoff
  tbl$outlier <- res$outlier
  tbl$weight <- as.numeric(res$weight)
  tbl <- cbind(tbl, as_tbl(res$scores, vars))
  tbl <- tibble::as_tibble(tbl)
  if (!plot) {
    return(tbl)
  }
  caption_text <- if (isTRUE(caption)) {
    outlier_caption(res, cutoff, alpha, type, sum(!complete))
  } else if (is.character(caption)) {
    caption
  }
  p <- if (type == "scores") {
    outlier_scores_plot(tbl, vars, color_by, label_outliers, base_size)
  } else {
    outlier_distance_plot(tbl, res, color_by, label_outliers, base_size)
  }
  p <- p +
    ggplot2::labs(y = ylab %||% (if (type == "scores") "Robust z-score" else "Robust distance"),
                  title = title, caption = caption_text) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.line = ggplot2::element_line(colour = "#4b4b4b", linewidth = base_size / 16),
      axis.ticks = ggplot2::element_line(colour = "#4b4b4b"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30", size = ggplot2::rel(0.8)),
      plot.caption.position = "plot"
    )
  finish_title(p)
}

# ---- internals ---------------------------------------------------------------

# Robust distances (MCD), the cutoff, the outliers and the robust z-scores of
# the samples (rows) of `x`.
outlier_scores <- function(x, quan, alpha, cutoff) {
  # covMcd draws random subsets: fix the seed for reproducible results while
  # leaving the caller's random number stream untouched.
  rob <- with_seed(123, robustbase::covMcd(x, alpha = quan))
  xarw <- covARW(x, rob$center, rob$cov, alpha = alpha)
  d2 <- stats::mahalanobis(x, center = rob$center, cov = rob$cov)
  quantile <- stats::qchisq(1 - alpha, ncol(x))
  # beyond the adaptive cutoff, as aq.plot() of mvoutlier; none when the
  # distances follow the chi-squared tail (no adaptive cutoff)
  limit <- if (cutoff == "adaptive") xarw$cn else quantile
  outlier <- d2 > limit
  scores <- sweep(sweep(x, 2, xarw$m), 2, sqrt(diag(xarw$c)), "/")
  list(distance = sqrt(d2), cutoff = sqrt(limit), quantile = sqrt(quantile),
       adaptive = sqrt(xarw$cn), outlier = outlier, weight = xarw$w, scores = scores)
}

outlier_colours <- c(Regular = "grey65", Outlier = "#b2182b")

# The robust z-scores, one panel per variable, the samples at the same
# horizontal position in every panel.
outlier_scores_plot <- function(tbl, vars, color_by, label_outliers, base_size) {
  # a fixed seed keeps the positions reproducible without altering the
  # caller's random number stream
  tbl$.x <- with_seed(123, stats::runif(nrow(tbl), min = -1, max = 1))
  long <- tidyr::pivot_longer(tbl, cols = dplyr::all_of(vars), names_to = "variable",
                              values_to = "score")
  long$variable <- factor(long$variable, levels = vars)
  long$.status <- factor(ifelse(long$outlier, "Outlier", "Regular"), levels = c("Regular", "Outlier"))
  long <- long[order(long$outlier), ] # outliers on top

  .x <- score <- NULL
  size <- 1.4 * base_size / 11
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .x, y = score)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = c(-2.5, 2.5), colour = "grey70", linewidth = 0.3,
                        linetype = "dashed")
  p <- p + outlier_points(color_by, size)
  if (label_outliers) {
    # in each panel, the outliers beyond the univariate limits (all of them
    # are named in the distance plot), moved apart by ggrepel, away from all
    # the points (unlabeled ones have an empty label)
    shown <- long$outlier & abs(long$score) > 2.5
    long$.label <- ""
    long$.label[shown] <- as.character(if ("id" %in% names(long)) long$id[shown] else long$row[shown])
    p <- p + outlier_labels(long, base_size)
  }
  n_vars <- length(vars)
  p +
    ggplot2::facet_wrap(ggplot2::vars(.data$variable), nrow = if (n_vars <= 6) 1) +
    ggplot2::scale_x_continuous(limits = c(-1.5, 1.5), breaks = NULL) +
    ggplot2::labs(x = NULL) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(axis.line.x = ggplot2::element_blank())
}

# Labels of the outliers (column `.label`, empty for the other samples),
# moved apart by ggrepel so that they overlap neither each other nor the
# points, with a fixed seed for the same layout at each drawing.
outlier_labels <- function(data, base_size) {
  ggrepel::geom_text_repel(data = data, ggplot2::aes(label = .data$.label), colour = "grey25",
                           size = 0.65 * base_size / ggplot2::.pt, segment.colour = "grey60",
                           min.segment.length = 0.2, max.overlaps = Inf, seed = 1)
}

# The robust distance of each sample, in the order of the rows, with the
# cutoff and the chi-squared quantile.
outlier_distance_plot <- function(tbl, res, color_by, label_outliers, base_size) {
  tbl$.status <- factor(ifelse(tbl$outlier, "Outlier", "Regular"), levels = c("Regular", "Outlier"))
  tbl$score <- tbl$mahalanobis
  tbl$.x <- tbl$row
  size <- 1.4 * base_size / 11
  .x <- score <- NULL
  p <- ggplot2::ggplot(tbl, ggplot2::aes(x = .x, y = score)) +
    ggplot2::geom_hline(yintercept = res$quantile, colour = "grey55", linewidth = 0.4,
                        linetype = "dashed")
  if (is.finite(res$cutoff)) {
    p <- p + ggplot2::geom_hline(yintercept = res$cutoff, colour = "grey25", linewidth = 0.5)
  }
  p <- p + outlier_points(color_by, size)
  top <- 0.08
  if (label_outliers) {
    tbl$.label <- ""
    tbl$.label[tbl$outlier] <- as.character(if ("id" %in% names(tbl)) tbl$id[tbl$outlier] else
      tbl$row[tbl$outlier])
    p <- p + outlier_labels(tbl, base_size)
  }
  p +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.03, top))) +
    ggplot2::labs(x = "Sample (row)") +
    ggplot2::theme_classic(base_size = base_size)
}

# The points: by status (regular or outlier), by robust distance, or both.
outlier_points <- function(color_by, size) {
  .status <- mahalanobis <- NULL
  if (color_by == "outlier") {
    list(
      ggplot2::geom_point(ggplot2::aes(colour = .status, shape = .status), size = size),
      ggplot2::scale_colour_manual(values = outlier_colours, name = NULL),
      ggplot2::scale_shape_manual(values = c(Regular = 16, Outlier = 17), name = NULL)
    )
  } else {
    mapping <- if (color_by == "both") {
      ggplot2::aes(colour = mahalanobis, shape = .status)
    } else {
      ggplot2::aes(colour = mahalanobis)
    }
    list(
      ggplot2::geom_point(mapping, size = size),
      ggplot2::scale_colour_viridis_c(option = "plasma", end = 0.85, name = "Robust\ndistance"),
      if (color_by == "both") {
        ggplot2::scale_shape_manual(values = c(Regular = 1, Outlier = 17), name = NULL)
      }
    )
  }
}

# How the outliers are flagged and how to read the plot.
outlier_caption <- function(res, cutoff, alpha, type, n_missing) {
  n_out <- sum(res$outlier)
  quantile <- paste0("the ", format(100 * (1 - alpha)), "% chi-squared quantile")
  text <- if (cutoff == "adaptive" && !is.finite(res$cutoff)) {
    paste0("No outliers among ", length(res$outlier), " samples: too few robust distances ",
           "(MCD) exceed ", quantile, " for an adaptive cutoff (Filzmoser et al., 2005).")
  } else {
    rule <- if (cutoff == "adaptive") {
      paste0("robust distance (MCD) above ", format(signif(res$cutoff, 3)),
             ", the adaptive cutoff (Filzmoser et al., 2005)")
    } else {
      paste0("robust distance (MCD) above ", format(signif(res$cutoff, 3)), ", ", quantile)
    }
    paste0(n_out, if (n_out == 1) " outlier" else " outliers", " among ", length(res$outlier),
           " samples: ", rule, ".")
  }
  if (type == "scores") {
    text <- paste(text, "Each point is a sample, at the same horizontal position in every panel;",
                  "the outliers are flagged on all the variables jointly. Dashed: z = -2.5 and 2.5.")
  } else {
    text <- paste0(text, if (is.finite(res$cutoff) && cutoff == "adaptive") {
      " Solid: the adaptive cutoff; dashed: "
    } else {
      " Dashed: "
    }, quantile, ".")
  }
  if (n_missing > 0) {
    text <- paste(text, n_missing, if (n_missing == 1) "row" else "rows",
                  "with missing values left out.")
  }
  paste(strwrap(text, width = 70), collapse = "\n")
}

# Adaptive reweighted estimator of multivariate location and scatter, with
# hard-rejection weights (function arw() of the mvoutlier package, Filzmoser
# et al., 2005): `cn` is the adaptive cutoff of the squared distances (Inf
# when the distances follow the chi-squared tail), `w` the weights.
covARW <- function(x, m0, c0, alpha, pcrit){
  n <- nrow(x)
  p <- ncol(x)

  if (missing(pcrit)){
    if (p <= 10) pcrit <- (0.24 - 0.003 * p) / sqrt(n)
    if (p > 10) pcrit <- (0.252 - 0.0018 * p) / sqrt(n)
  }

  if (missing(alpha)) {
    delta <- stats::qchisq(0.975, p)
    } else {
    delta <- stats::qchisq(1 - alpha, p)
    }

  d2 <- stats::mahalanobis(x, m0, c0)
  d2ord <- sort(d2)
  dif <- stats::pchisq(d2ord, p) - (0.5:n) / n
  i <- (d2ord >= delta) & (dif > 0)

  if (sum(i) == 0) {
    alfan <- 0
    } else {
      alfan <- max(dif[i])
      }

  if (alfan < pcrit) {
    alfan <- 0
    }
  if (alfan > 0) {
    cn <- max(d2ord[n - ceiling(n * alfan)], delta)
    } else {
      cn <- Inf
      }

  w <- d2 < cn

  if(sum(w) == 0) {
    m <- m0
    c <- c0
    } else {
    m <- colMeans(x[w, , drop = FALSE])
    c1 <- as.matrix(x - tcrossprod(rep(1, n), m))
    c <- crossprod(c1 * w, c1) / sum(w)
    }

  list(
    m = m,
    c = c,
    cn = cn,
    w = w
    )
}
