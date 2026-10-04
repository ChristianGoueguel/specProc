#' @title Self-Organizing Map of Spectra
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Maps spectra onto a two-dimensional grid of units with a self-organizing
#' map (SOM; Kohonen, 1982), so that similar spectra land on the same or on
#' neighboring units. Each unit holds a prototype (codebook vector), which is
#' itself a spectrum. The map is useful for sorting samples quickly (scrap
#' metals, mineral phases), and it follows nonlinear relations between
#' spectra (matrix effects, self-absorption, changes of the plasma) that a
#' linear projection such as PCA can miss. New spectra are placed on their
#' best-matching unit with [predict()][predict.specproc_som], and a large
#' quantization error flags a spectrum that fits no known group.
#'
#' @details
#' **Algorithm.** The batch SOM (Kohonen, 2013) is computed in C++. At each
#' epoch, every spectrum is assigned to its best-matching unit (BMU), the unit
#' with the closest prototype, and every prototype becomes the mean of the
#' spectra weighted by a Gaussian kernel of the grid distance between its unit
#' and their BMU. The radius of the kernel decreases linearly over the
#' `epochs`, from `radius[1]` (a global ordering of the map) to `radius[2]`
#' (local fine-tuning). The distances between spectra and prototypes are
#' computed in one matrix product per epoch, so thousands of channels are
#' handled quickly.
#'
#' **Initialization.** With `init = "pca"` (default), the prototypes start
#' on the plane of the first two principal components, spread over two
#' standard deviations of the scores, with the long side of the grid along
#' the first component. The training is then deterministic, and the map is
#' oriented like a PCA score plot. `init = "random"` starts from randomly
#' chosen spectra; use [set.seed()] for reproducibility.
#'
#' **Grid.** By default, the map has about \eqn{5\sqrt{n}} units (Vesanto et
#' al., 2000), with the ratio of its sides set to the square root of the ratio
#' of the first two eigenvalues of the data. The `"hexagonal"` topology gives
#' each unit six equidistant neighbors, which displays the clusters better
#' than the four of `"rectangular"`.
#'
#' **Robust SOM.** With `robust = TRUE`, the spectra are weighted at each
#' epoch by Huber weights of their quantization errors (robustly
#' standardized, with cut-off \eqn{\sqrt{\chi^2_{1, 0.99}}}): the outlying
#' spectra (a misfired shot, a contaminated sample) pull the prototypes
#' less, as in the robust SOMs of Allende et al. (2004).
#'
#' **Quality and novelty.** The mean quantization error measures how well
#' the prototypes represent the spectra, and the topographic error (the share
#' of spectra whose two best units are not neighbors) how well the map keeps
#' the neighborhoods. The quantization errors of the training spectra give a
#' robust cut-off (the Wilson-Hilferty transformation with the univariate MCD,
#' as for the orthogonal distances of [robpca()]): spectra beyond it fit no
#' unit well, and new spectra beyond it are flagged as novel by
#' [predict()][predict.specproc_som].
#'
#' **Stability.** The map depends on its grid, radius and initialization. Use
#' [som_stability()] to check that the neighborhoods are stable over
#' resampled data.
#'
#' @references
#'  - Kohonen, T. (1982). Self-organized formation of topologically correct
#'    feature maps. Biological Cybernetics, 43(1):59-69.
#'  - Kohonen, T. (2013). Essentials of the self-organizing map. Neural
#'    Networks, 37:52-65.
#'  - Vesanto, J., Himberg, J., Alhoniemi, E., Parhankangas, J. (2000). SOM
#'    Toolbox for Matlab 5. Report A57, Helsinki University of Technology.
#'  - Allende, H., Moreno, S., Rogel, C., Salas, R. (2004). Robust
#'    self-organizing maps. In Progress in Pattern Recognition, Image
#'    Analysis and Applications (CIARP 2004), Lecture Notes in Computer
#'    Science 3287:179-186.
#'
#' @param x A numeric matrix or data frame of spectra, one per row.
#' @param grid The numbers of units along the two sides of the map,
#'   `c(xdim, ydim)`. If `NULL` (default), chosen from the number of spectra
#'   (see details).
#' @param topology `"hexagonal"` (default) or `"rectangular"`.
#' @param epochs The number of training epochs. Default is 50.
#' @param radius The radius of the neighborhood kernel (in units) at the first
#'   and the last epoch. If `NULL` (default), `c(max(grid) / 3, 1)`.
#' @param init `"pca"` (default) or `"random"`.
#' @param center,scale Logical values: center the variables (`TRUE`, default)
#'   and scale them to unit variance (`FALSE`, default) before training.
#' @param robust A logical: down-weight the outlying spectra (`FALSE`,
#'   default).
#'
#' @return An object of class `specproc_som`, a list with
#'  - `codebook`: the prototypes, one row per unit, in the units of `x`;
#'  - `grid`: a tibble with the `unit`, its `row` and `col`, and its
#'    coordinates `x` and `y` on the map;
#'  - `unit`, `qe`: the best-matching unit and the quantization error of each
#'    training spectrum, and `weights` (with `robust = TRUE`);
#'  - `quantization_error`, `topographic_error`: the quality of the map;
#'  - `cutoff`: the quantization error beyond which a spectrum is novel;
#'  - `center`, `scale` and the training settings.
#'
#' @seealso [predict.specproc_som()], [plot_som()], [som_stability()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' # the Na I and K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which((wl > 585 & wl < 595) | (wl > 760 & wl < 780))]
#' fit <- som(spectra)
#' fit
#' # where the potassium-rich samples are, and how many samples per unit
#' plot_som(fit, type = "mapping", colour = forageLIBS$K)
#' plot_som(fit, type = "counts")
som <- function(x, grid = NULL, topology = c("hexagonal", "rectangular"), epochs = 50,
                radius = NULL, init = c("pca", "random"), center = TRUE, scale = FALSE,
                robust = FALSE) {
  x <- as_numeric_matrix(x, "x")
  if (anyNA(x)) {
    stop("'x' contains missing values; impute them first.", call. = FALSE)
  }
  n <- nrow(x)
  if (n < 4 || ncol(x) < 2) {
    stop("At least 4 spectra and 2 variables are needed.", call. = FALSE)
  }
  if (is.null(colnames(x))) colnames(x) <- paste0("V", seq_len(ncol(x)))
  topology <- match.arg(topology)
  init <- match.arg(init)
  check_count(epochs, "epochs")
  check_flag(center, "center")
  check_flag(scale, "scale")
  check_flag(robust, "robust")
  pp <- preprocess(x, center, scale)
  xs <- pp$x

  # first two principal components: initialization and default grid
  s <- svd(sweep(xs, 2, colMeans(xs)), nu = 2, nv = 2)
  eig <- s$d[1:2]^2 / (n - 1)
  if (is.null(grid)) {
    units <- max(4, round(5 * sqrt(n)))
    ratio <- if (eig[2] > 0) min(3, sqrt(eig[1] / eig[2])) else 1
    ydim <- max(2, round(sqrt(units / ratio)))
    xdim <- max(2, round(units / ydim))
    grid <- c(xdim, ydim)
  }
  if (!is.numeric(grid) || length(grid) != 2 || any(grid < 1) || any(grid %% 1 != 0) || prod(grid) < 2) {
    stop("'grid' must be two positive integers c(xdim, ydim), with at least 2 units.", call. = FALSE)
  }
  layout <- som_grid(grid[1], grid[2], topology)
  radius <- radius %||% c(max(grid) / 3, 1)
  if (!is.numeric(radius) || length(radius) != 2 || any(radius <= 0)) {
    stop("'radius' must be two positive numbers (first and last epoch).", call. = FALSE)
  }

  start <- if (init == "pca") {
    # the long side of the grid along the first component
    gx <- scales_to_unit(layout$x)
    gy <- scales_to_unit(layout$y)
    mean_x <- colMeans(xs)
    sweep(outer(gx, 2 * sqrt(eig[1]) * s$v[, 1]) + outer(gy, 2 * sqrt(eig[2]) * s$v[, 2]),
          2, mean_x, "+")
  } else {
    xs[sample.int(n, nrow(layout), replace = nrow(layout) > n), , drop = FALSE]
  }
  grid_dist2 <- as.matrix(stats::dist(layout[c("x", "y")]))^2
  sigma <- seq(radius[1], radius[2], length.out = epochs)
  fit <- som_batch_cpp(xs, start, grid_dist2, sigma, robust, sqrt(stats::qchisq(0.99, 1)))

  codebook <- sweep(sweep(fit$codebook, 2, pp$scale, "*"), 2, pp$center, "+")
  dimnames(codebook) <- list(paste0("unit", layout$unit), colnames(x))
  neighbors <- sqrt(grid_dist2) <= 1.01
  te <- mean(!neighbors[cbind(fit$unit, fit$second)])
  res <- list(
    codebook = codebook,
    grid = tibble::as_tibble(layout),
    unit = fit$unit,
    qe = fit$qe,
    weights = if (robust) fit$weights else NULL,
    quantization_error = mean(fit$qe),
    topographic_error = te,
    cutoff = od_cutoff(fit$qe, ceiling(0.75 * n)),
    center = pp$center, scale = pp$scale,
    topology = topology, epochs = epochs, radius = radius, init = init, robust = robust,
    codebook_scaled = fit$codebook
  )
  structure(res, variables = colnames(x), nvar = ncol(x), class = "specproc_som")
}

# Units of a SOM grid, row by row, with their coordinates: on a hexagonal
# grid, odd rows are shifted by half a unit and rows are sqrt(3)/2 apart, so
# that every unit is at distance 1 from its six neighbors.
som_grid <- function(xdim, ydim, topology) {
  layout <- expand.grid(col = seq_len(xdim), row = seq_len(ydim))
  if (topology == "hexagonal") {
    layout$x <- layout$col + 0.5 * ((layout$row - 1) %% 2)
    layout$y <- (layout$row - 1) * sqrt(3) / 2 + 1
  } else {
    layout$x <- layout$col
    layout$y <- layout$row
  }
  data.frame(unit = seq_len(nrow(layout)), row = layout$row, col = layout$col,
             x = layout$x, y = layout$y)
}

scales_to_unit <- function(v) {
  r <- range(v)
  if (diff(r) == 0) return(rep(0, length(v)))
  2 * (v - r[1]) / diff(r) - 1
}

#' @export
print.specproc_som <- function(x, ...) {
  dims <- c(max(x$grid$col), max(x$grid$row))
  cat("Self-organizing map", if (x$robust) " (robust)", "\n\n", sep = "")
  cat("Spectra:             ", length(x$unit), "\n", sep = "")
  cat("Variables:           ", attr(x, "nvar"), "\n", sep = "")
  cat("Grid:                ", dims[1], " x ", dims[2], " (", x$topology, ", ",
      nrow(x$grid), " units)\n", sep = "")
  cat("Units with spectra:  ", length(unique(x$unit)), "\n", sep = "")
  cat("Quantization error:  ", format(x$quantization_error, digits = 4), "\n", sep = "")
  cat("Topographic error:   ", format(x$topographic_error, digits = 3), "\n", sep = "")
  cat("Beyond the cut-off:  ", sum(x$qe > x$cutoff), "\n", sep = "")
  invisible(x)
}

#' @title Map New Spectra onto a Self-Organizing Map
#'
#' @description
#' Places new spectra on the best-matching unit of a map fitted by [som()],
#' with their quantization error, and flags the spectra that fit no unit.
#'
#' @param object A map fitted by [som()].
#' @param newdata A numeric matrix or data frame of spectra, with the
#'   variables of the training data.
#' @param ... Not used.
#'
#' @return A tibble with one row per spectrum: its best-matching `unit`, the
#'   coordinates `x` and `y` of the unit on the map, the quantization error
#'   `qe`, and `novel`, `TRUE` when the error exceeds the cut-off of the
#'   training spectra (the spectrum fits no known group).
#'
#' @seealso [som()], [plot_som()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' # the Na I and K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which((wl > 585 & wl < 595) | (wl > 760 & wl < 780))]
#' fit <- som(spectra[1:300, ])
#' head(predict(fit, spectra[301:368, ]))
predict.specproc_som <- function(object, newdata, ...) {
  x <- filter_newdata(object, newdata)
  if (anyNA(x)) {
    stop("'newdata' contains missing values.", call. = FALSE)
  }
  xs <- sweep(sweep(x, 2, object$center), 2, object$scale, "/")
  mapped <- som_map_cpp(xs, object$codebook_scaled)
  tibble::tibble(
    unit = mapped$unit,
    x = object$grid$x[mapped$unit],
    y = object$grid$y[mapped$unit],
    qe = mapped$qe,
    novel = mapped$qe > object$cutoff
  )
}

#' @title Stability of a Self-Organizing Map
#'
#' @description
#' Fits a self-organizing map with [som()] on resampled data several times,
#' maps all the spectra on each map, and measures how consistently the
#' spectra keep their neighbors.
#'
#' @details
#' Each run fits the map on a bootstrap sample of the spectra (with the same
#' grid), and maps all of them. For each run, the grid distances between the
#' units of every pair of spectra form a matrix; the stability is the mean
#' correlation between these matrices over the pairs of runs. Values close
#' to 1 mean that the same spectra are neighbors on every map, whatever the
#' sample: the structure of the map is not an artifact of the training data.
#' Low values suggest a smaller grid, a larger final radius or more epochs.
#'
#' @param x A numeric matrix or data frame of spectra, one per row.
#' @param runs The number of resampled fits. Default is 10.
#' @param ... Further arguments passed to [som()], such as `grid` or
#'   `robust`.
#'
#' @return A list with `runs`, a tibble with the quantization and topographic
#'   errors of each run, `stability`, the mean correlation between the runs,
#'   and `correlations`, the matrix of the correlations between runs.
#'
#' @seealso [som()]
#' @export
#'
#' @examples
#' \donttest{
#' data(forageLIBS)
#' # the 380-430 nm window (Ca II H and K lines), faster than all the channels
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]
#' set.seed(1)
#' som_stability(spectra, runs = 5)$stability
#' }
som_stability <- function(x, runs = 10, ...) {
  x <- as_numeric_matrix(x, "x")
  check_count(runs, "runs", lower = 2)
  args <- list(...)
  if (is.null(args$grid)) {
    reference <- som(x, ...)
    args$grid <- c(max(reference$grid$col), max(reference$grid$row))
  }
  n <- nrow(x)
  fits <- lapply(seq_len(runs), function(r) {
    fit <- do.call(som, c(list(x[sample.int(n, n, replace = TRUE), , drop = FALSE]), args))
    mapped <- stats::predict(fit, x)
    list(qe = mean(mapped$qe), te = fit$topographic_error,
         d = stats::dist(cbind(mapped$x, mapped$y)))
  })
  correlations <- matrix(1, runs, runs)
  for (a in seq_len(runs - 1)) {
    for (b in (a + 1):runs) {
      r <- suppressWarnings(stats::cor(fits[[a]]$d, fits[[b]]$d))
      correlations[a, b] <- correlations[b, a] <- r
    }
  }
  list(
    runs = tibble::tibble(run = seq_len(runs),
                          quantization_error = vapply(fits, `[[`, numeric(1), "qe"),
                          topographic_error = vapply(fits, `[[`, numeric(1), "te")),
    stability = mean(correlations[upper.tri(correlations)], na.rm = TRUE),
    correlations = correlations
  )
}

#' @title Plot a Self-Organizing Map
#'
#' @description
#' Draws a map fitted by [som()]: the number of spectra per unit, the
#' distances between neighboring prototypes, the component planes, the
#' spectra on the map, or the prototype spectra.
#'
#' @details
#' The types of plot are:
#'  - `"counts"`: the number of training spectra on each unit (empty units
#'    in white).
#'  - `"umatrix"`: the unified distance matrix, the mean distance of each
#'    prototype to those of its neighbors: high values (light) are the
#'    boundaries between groups of similar spectra, low values (dark) the
#'    groups themselves.
#'  - `"quality"`: the mean quantization error of the spectra of each unit.
#'  - `"component"`: the component planes, the value of the prototypes at
#'    each of the `variables` (one panel per variable): which units have
#'    intense emission lines. Each plane is scaled from its lowest (0) to its
#'    highest (1) unit, so that weak lines are as readable as strong ones.
#'  - `"mapping"`: the training spectra on their units (spread within the
#'    unit), colored by `colour`, and, with `newdata`, the new spectra as
#'    triangles, the novel ones circled in red.
#'  - `"prototypes"`: the prototype spectra of the `units` against
#'    wavelength. By default, six prototypes spread over the map: the unit
#'    with the most spectra, then, in turn, the unit whose prototype is the
#'    farthest from those already chosen.
#'
#' @param object A map fitted by [som()].
#' @param type The plot: `"counts"` (default), `"umatrix"`, `"quality"`,
#'   `"component"`, `"mapping"` or `"prototypes"`.
#' @param variables For `"component"`: the variables, as wavelengths (the
#'   nearest channel is used) or column names.
#' @param colour For `"mapping"`: a vector with one value per training
#'   spectrum, such as a concentration or a class.
#' @param newdata For `"mapping"`: optional new spectra to place on the map.
#' @param units For `"prototypes"`: the units whose prototypes are drawn.
#' @param title The plot title.
#'
#' @return A ggplot object.
#' @seealso [som()], [predict.specproc_som()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' # the Na I and K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which((wl > 585 & wl < 595) | (wl > 760 & wl < 780))]
#' fit <- som(spectra)
#' plot_som(fit, type = "umatrix")
#' # component planes of the K I and Na I lines
#' plot_som(fit, type = "component", variables = c(769.90, 589.59))
#' plot_som(fit, type = "mapping", colour = forageLIBS$K)
#' plot_som(fit, type = "prototypes")
plot_som <- function(object, type = c("counts", "umatrix", "quality", "component", "mapping", "prototypes"),
                     variables = NULL, colour = NULL, newdata = NULL, units = NULL, title = NULL) {
  if (!inherits(object, "specproc_som")) {
    stop("'object' must be returned by som().", call. = FALSE)
  }
  type <- match.arg(type)
  grid <- as.data.frame(object$grid)
  n_units <- nrow(grid)
  if (type == "prototypes") {
    return(som_prototype_plot(object, units, title))
  }
  hexagonal <- object$topology == "hexagonal"
  polygons <- som_polygons(grid, hexagonal)

  if (type %in% c("counts", "umatrix", "quality")) {
    value <- switch(
      type,
      counts = tabulate(object$unit, n_units),
      umatrix = som_umatrix(object),
      quality = vapply(seq_len(n_units), function(u) {
        e <- object$qe[object$unit == u]
        if (length(e)) mean(e) else NA_real_
      }, numeric(1))
    )
    if (type == "counts") value[value == 0] <- NA
    polygons$value <- value[polygons$unit]
    legend <- switch(type, counts = "Spectra", umatrix = "Distance", quality = "Quantization\nerror")
    title <- title %||% switch(type, counts = "Spectra per unit", umatrix = "U-matrix",
                               quality = "Quantization error per unit")
    p <- som_base_plot(polygons) +
      ggplot2::scale_fill_viridis_c(name = legend, na.value = "white",
                                    option = if (type == "umatrix") "magma" else "viridis")
  } else if (type == "component") {
    if (is.null(variables)) {
      stop("`type = \"component\"` needs `variables`.", call. = FALSE)
    }
    columns <- som_columns(object, variables)
    # each plane on its own scale, from its lowest (0) to its highest (1) unit
    planes <- do.call(rbind, lapply(seq_along(columns), function(i) {
      d <- polygons
      d$value <- scales_to_unit(object$codebook[d$unit, columns[i]]) / 2 + 0.5
      d$variable <- names(columns)[i]
      d
    }))
    planes$variable <- factor(planes$variable, levels = names(columns))
    title <- title %||% "Component planes"
    p <- som_base_plot(planes) +
      ggplot2::facet_wrap(ggplot2::vars(.data$variable)) +
      ggplot2::scale_fill_viridis_c(name = "Relative\nvalue", breaks = c(0, 0.5, 1))
  } else {
    # spectra spread within their unit, on a sunflower pattern
    spread <- function(units) {
      out <- data.frame(unit = units, x = grid$x[units], y = grid$y[units])
      k <- stats::ave(seq_along(units), units, FUN = seq_along)
      m <- stats::ave(seq_along(units), units, FUN = length)
      r <- 0.32 * sqrt((k - 0.5) / m)
      a <- k * 2.39996
      out$x <- out$x + r * cos(a)
      out$y <- out$y + r * sin(a)
      out
    }
    points <- spread(object$unit)
    title <- title %||% "Spectra on the map"
    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = polygons, ggplot2::aes(.data$px, .data$py, group = .data$unit),
                            fill = "grey97", colour = "grey75", linewidth = 0.3)
    if (is.null(colour)) {
      p <- p + ggplot2::geom_point(data = points, ggplot2::aes(.data$x, .data$y),
                                   colour = "#1f4e79", size = 1.6, alpha = 0.8)
    } else {
      if (length(colour) != length(object$unit)) {
        stop("'colour' must have one value per training spectrum (", length(object$unit), ").",
             call. = FALSE)
      }
      points$colour <- colour
      p <- p + ggplot2::geom_point(data = points, ggplot2::aes(.data$x, .data$y, colour = .data$colour),
                                   size = 1.6, alpha = 0.85) +
        (if (is.numeric(colour)) ggplot2::scale_colour_viridis_c(name = NULL) else
          ggplot2::scale_colour_brewer(palette = "Dark2", name = NULL))
    }
    if (!is.null(newdata)) {
      mapped <- stats::predict(object, newdata)
      new_points <- spread(mapped$unit)
      new_points$novel <- mapped$novel
      p <- p + ggplot2::geom_point(data = new_points, ggplot2::aes(.data$x, .data$y), shape = 24,
                                   fill = "white", colour = "black", size = 2.2) +
        ggplot2::geom_point(data = new_points[new_points$novel, , drop = FALSE],
                            ggplot2::aes(.data$x, .data$y), shape = 21, colour = "#c0392b",
                            size = 4.5, stroke = 0.8)
    }
  }
  p <- p +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "right", plot.margin = ggplot2::margin(6, 6, 6, 6))
  finish_title(p)
}

# Vertices of the unit cells: pointy-topped hexagons of width 1, or squares.
som_polygons <- function(grid, hexagonal) {
  if (hexagonal) {
    angles <- pi / 6 + (0:5) * pi / 3
    r <- 1 / sqrt(3)
  } else {
    angles <- pi / 4 + (0:3) * pi / 2
    r <- sqrt(2) / 2
  }
  data.frame(unit = rep(grid$unit, each = length(angles)),
             px = rep(grid$x, each = length(angles)) + r * cos(angles),
             py = rep(grid$y, each = length(angles)) + r * sin(angles))
}

som_base_plot <- function(polygons) {
  ggplot2::ggplot(polygons, ggplot2::aes(.data$px, .data$py, group = .data$unit, fill = .data$value)) +
    ggplot2::geom_polygon(colour = "white", linewidth = 0.3)
}

# Mean distance of each prototype to those of its grid neighbors.
som_umatrix <- function(object) {
  grid <- object$grid
  neighbors <- as.matrix(stats::dist(grid[c("x", "y")])) <= 1.01
  diag(neighbors) <- FALSE
  w <- object$codebook_scaled
  vapply(seq_len(nrow(grid)), function(u) {
    nb <- which(neighbors[u, ])
    mean(sqrt(colSums((t(w[nb, , drop = FALSE]) - w[u, ])^2)))
  }, numeric(1))
}

# Columns of the codebook for wavelengths (nearest channel) or names.
som_columns <- function(object, variables) {
  names_x <- colnames(object$codebook)
  if (is.numeric(variables)) {
    wl <- suppressWarnings(as.numeric(names_x))
    if (anyNA(wl)) {
      stop("The variables are not wavelengths: give their names in `variables`.", call. = FALSE)
    }
    idx <- vapply(variables, function(v) which.min(abs(wl - v)), integer(1))
    stats::setNames(idx, paste(format(variables), "nm"))
  } else {
    idx <- match(variables, names_x)
    if (anyNA(idx)) {
      stop("Unknown variable(s) in `variables`: ", paste(variables[is.na(idx)], collapse = ", "),
           call. = FALSE)
    }
    stats::setNames(idx, variables)
  }
}

som_prototype_plot <- function(object, units, title) {
  counts <- tabulate(object$unit, nrow(object$grid))
  if (is.null(units)) {
    # farthest-point selection among the units with spectra
    w <- object$codebook_scaled
    candidates <- which(counts > 0)
    units <- candidates[which.max(counts[candidates])]
    while (length(units) < min(6, length(candidates))) {
      d <- vapply(candidates, function(u) min(colSums((t(w[units, , drop = FALSE]) - w[u, ])^2)),
                  numeric(1))
      units <- c(units, candidates[which.max(d)])
    }
  }
  if (!is.numeric(units) || any(!units %in% object$grid$unit)) {
    stop("'units' must be units of the map (1 to ", nrow(object$grid), ").", call. = FALSE)
  }
  wl <- suppressWarnings(as.numeric(colnames(object$codebook)))
  spectral <- !anyNA(wl)
  if (!spectral) wl <- seq_len(ncol(object$codebook))
  d <- do.call(rbind, lapply(units, function(u) {
    data.frame(unit = paste0("unit ", u, " (", counts[u], ")"), x = wl,
               value = object$codebook[u, ])
  }))
  d$unit <- factor(d$unit, levels = unique(d$unit))
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$value, colour = .data$unit)) +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::scale_colour_brewer(palette = "Dark2", name = "Unit (spectra)") +
    ggplot2::labs(x = if (spectral) "Wavelength (nm)" else "Variable", y = "Prototype",
                  title = title %||% "Prototype spectra") +
    ggplot2::theme_bw()
  finish_title(p)
}
