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
#' @param object An object returned by [robpca()], [rospca()] or
#'   [macropca()].
#' @param newdata Optional new observations to add to the map (a numeric
#'   matrix or data frame with the calibration variables).
#' @param labels The number of most outlying observations to label (by
#'   their row names, or row numbers). Default is 3; use 0 for no labels.
#' @param title The plot title.
#'
#' @return A ggplot object.
#'
#' @seealso [robpca()], [rospca()], [macropca()], [plot_cell_map()]
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 10), 100, 10) %*% diag(10:1)
#' x[1:4, ] <- x[1:4, ] + 25                          # bad leverage
#' x[5:8, 9:10] <- x[5:8, 9:10] + 15                  # orthogonal outliers
#' fit <- robpca(x[-(90:100), ], k = 3)
#' plot_outlier_map(fit)
#' plot_outlier_map(fit, newdata = x[90:100, ])
#'
plot_outlier_map <- function(object, newdata = NULL, labels = 3, title = NULL) {
  if (!inherits(object, "specproc_robpca")) {
    stop("'object' must be returned by robpca(), rospca() or macropca().", call. = FALSE)
  }
  check_count(labels, "labels", lower = 0)
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
  df$label <- ""
  if (labels > 0) {
    top <- order(severity, decreasing = TRUE)[seq_len(min(labels, nrow(df)))]
    top <- top[severity[top] > 1]
    df$label[top] <- df$id[top]
  }

  palette <- c(regular = "grey55", `good leverage` = "#1b9e77",
               `orthogonal outlier` = "#d95f02", `bad leverage` = "#e7298a")
  ggplot2::ggplot(df, ggplot2::aes(x = .data$sd, y = .data$od)) +
    ggplot2::geom_vline(xintercept = object$cutoff_sd, linetype = "dashed", colour = "grey30") +
    ggplot2::geom_hline(yintercept = object$cutoff_od, linetype = "dashed", colour = "grey30") +
    ggplot2::geom_point(ggplot2::aes(colour = .data$type, shape = .data$set), size = 2, alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 3, hjust = -0.2, vjust = -0.4,
                       colour = "grey20", na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = palette, drop = FALSE, name = NULL) +
    ggplot2::scale_shape_manual(values = c(calibration = 16, new = 17), name = NULL, drop = TRUE) +
    ggplot2::labs(x = "Score distance", y = "Orthogonal distance", title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}

#' @title Cell Map of a MacroPCA Fit
#'
#' @description
#' Shows which cells of the data deviate from a [macropca()] fit, with
#' [cellWise::cellMap()]. Each cell is colored by its standardized residual:
#' red when the observed value is much higher than the fit, blue when it is
#' much lower. Rows of outlying observations are marked.
#'
#' @details
#' Spectra have many more variables than can be shown one by one. Use
#' `columns` to select a spectral region, and `ncolumnsinblock` (and
#' `nrowsinblock`) to combine adjacent cells into blocks; the color of a
#' block then summarizes its cells. By default, the columns are grouped
#' into blocks when there are more than 60 of them.
#'
#' @param object An object returned by [macropca()].
#' @param rows,columns Optional indices or names of the rows and columns to
#'   show. Default is all.
#' @param nrowsinblock,ncolumnsinblock Optional numbers of rows and columns
#'   combined into one block.
#' @param title The plot title.
#' @param ... Further arguments passed to [cellWise::cellMap()], such as
#'   `rowlabels`, `columnlabels` or `columnangle`.
#'
#' @return A ggplot object.
#'
#' @seealso [macropca()], [plot_outlier_map()]
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(40 * 8), 40, 8) %*% diag(8:1)
#' x[1:2, ] <- x[1:2, ] + 20
#' x[10, 2] <- 40
#' fit <- macropca(x, k = 2)
#' plot_cell_map(fit)
#'
plot_cell_map <- function(object, rows = NULL, columns = NULL, nrowsinblock = NULL,
                          ncolumnsinblock = NULL, title = "MacroPCA cell map", ...) {
  if (!inherits(object, "specproc_macropca")) {
    stop("'object' must be returned by macropca().", call. = FALSE)
  }
  resid <- object$std_resid
  flagged <- object$flagged_cells
  if (is.null(rownames(resid))) rownames(resid) <- seq_len(nrow(resid))
  if (is.null(colnames(resid))) colnames(resid) <- attr(object, "variables")
  outlying_rows <- object$od > object$cutoff_od
  if (!is.null(rows)) {
    resid <- resid[rows, , drop = FALSE]
    flagged <- flagged[rows, , drop = FALSE]
    outlying_rows <- outlying_rows[if (is.character(rows)) match(rows, rownames(object$std_resid)) else rows]
  }
  if (!is.null(columns)) {
    resid <- resid[, columns, drop = FALSE]
    flagged <- flagged[, columns, drop = FALSE]
  }
  if (is.null(ncolumnsinblock) && ncol(resid) > 60) {
    ncolumnsinblock <- ceiling(ncol(resid) / 60)
  }
  # cellMap needs the numbers of rows and columns to be multiples of the block sizes
  if (!is.null(nrowsinblock)) {
    keep <- seq_len(nrow(resid) - nrow(resid) %% nrowsinblock)
    resid <- resid[keep, , drop = FALSE]
    flagged <- flagged[keep, , drop = FALSE]
    outlying_rows <- outlying_rows[keep]
  }
  if (!is.null(ncolumnsinblock)) {
    keep <- seq_len(ncol(resid) - ncol(resid) %% ncolumnsinblock)
    resid <- resid[, keep, drop = FALSE]
    flagged <- flagged[, keep, drop = FALSE]
  }
  # cellMap reports that it constructs block labels; that is expected here
  suppressMessages(cellWise::cellMap(
    R = resid,
    indcells = which(flagged),
    indrows = which(outlying_rows),
    nrowsinblock = nrowsinblock,
    ncolumnsinblock = ncolumnsinblock,
    mTitle = title,
    ...
  ))
}
