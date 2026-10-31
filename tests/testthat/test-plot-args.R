# The arguments shared by the plots: names, checks, wavelength windows and
# the former names.

layer_with <- function(p, geom) {
  Filter(function(l) inherits(l$geom, geom), p$layers)
}

# The number of the first layer drawn with `geom`.
layer_index <- function(p, geom) {
  which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))[1]
}

test_that("the shared arguments are checked", {
  expect_null(wavelength_window(NULL))
  expect_equal(wavelength_window(c(400, 390)), c(390, 400))
  expect_error(wavelength_window(390), "range")
  expect_error(wavelength_window(c(390, 390)), "range")
  expect_equal(in_window(c(389, 390, 395, 401), c(390, 400)), c(FALSE, TRUE, TRUE, FALSE))
  expect_error(check_label(1, "xlab"), "xlab")
  expect_silent(check_label(quote(T^2), "xlab"))
  expect_error(check_alpha(0, "point_alpha"), "point_alpha")
  expect_error(check_colour("nocolor", "fit_color"), "color")
  expect_equal(plotly_width(25.4 / 96), 1)
  expect_equal(plotly_colour("grey35"), "#595959")
})

test_that("plot_lines draws a wavelength window, with the markers scaled within it", {
  d <- make_spectra(n = 5)
  spectrum <- colMeans(d$x)
  lines <- data.frame(species = c("Xx I", "Xx II", "Xx I"), stage = c(1L, 2L, 1L),
                      wavelength = c(393.4, 396.8, 399.5), relative_intensity = c(1, 1, 0.5))
  p <- plot_lines(spectrum, lines, wavelength = c(392, 395), interactive = FALSE,
                  linewidth = 0.6, spectrum_color = "black", marker_alpha = 0.4,
                  xlab = "lambda", base_size = 8)
  spec <- ggplot2::layer_data(p, 1)
  expect_true(all(spec$x >= 392 & spec$x <= 395))
  expect_equal(unique(spec$linewidth), 0.6)
  expect_equal(unique(spec$colour), "black")
  markers <- ggplot2::layer_data(p, 2)
  expect_equal(markers$x, 393.4)
  expect_equal(markers$yend, max(spectrum[d$wl >= 392 & d$wl <= 395]))
  expect_equal(unique(markers$alpha), 0.4)
  expect_equal(p$labels$x, "lambda")
  expect_equal(p$theme$text$size, 8)
  expect_error(plot_lines(spectrum, lines, wavelength = c(500, 510)), "fewer than 2")
  expect_error(plot_lines(spectrum, lines, marker_alpha = 2), "marker_alpha")
  skip_if_not_installed("plotly")
  pi <- plotly::plotly_build(plot_lines(spectrum, lines, wavelength = c(392, 395),
                                        linewidth = 0.6, marker_alpha = 0.4, ylab = "counts"))
  traces <- pi$x$data
  names <- vapply(traces, function(t) t$name %||% "", character(1))
  expect_equal(traces[[which(names == "spectrum")]]$line$width, plotly_width(0.6))
  expect_equal(traces[[which(names == "spectrum")]]$line$color, "#595959")
  expect_equal(traces[[which(names == "stage I")]]$opacity, 0.4)
  expect_false("stage II" %in% names)
  y_title <- pi$x$layout$yaxis$title
  expect_equal(if (is.list(y_title)) y_title$text else y_title, "counts")
})

test_that("plot_spectra takes `interactive`; `.interactive` is deprecated", {
  d <- make_spectra(n = 3)
  df <- as.data.frame(d$x, check.names = FALSE)
  expect_s3_class(plot_spectra(df, interactive = FALSE), "ggplot")
  lifecycle::expect_deprecated(p <- plot_spectra(df, .interactive = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit styles the points and the curve", {
  d <- make_spectra(n = 1)
  x <- tibble::as_tibble(as.data.frame(d$x, check.names = FALSE))
  fit <- peak_fit(x, profile = "gaussian", wavelength = c(392.8, 394))
  p <- plot_fit(fit, linewidth = 1.2, point_alpha = 0.5)
  top <- p[[1]][[1]]
  expect_equal(unique(ggplot2::layer_data(top, 2)$alpha), 0.5)
  expect_equal(unique(ggplot2::layer_data(top, 3)$linewidth), 1.2)
  lifecycle::expect_deprecated(old <- plot_fit(fit, line.size = 1.5))
  expect_equal(unique(ggplot2::layer_data(old[[1]][[1]], 3)$linewidth), 1.5)
})

test_that("plot_loadings and plot_contributions draw a wavelength window", {
  d <- make_spectra(n = 20)
  pca <- stats::prcomp(d$x)
  p <- plot_loadings(pca, components = 1, wavelength = c(395, 398), top = 3, spectra = d$x,
                     xlab = "nm", ylab = "p", base_size = 9, linewidth = 0.8)
  bars <- ggplot2::layer_data(p, 2)
  expect_true(all(bars$x >= 395 & bars$x <= 398))
  expect_equal(unique(bars$linewidth), 0.8)
  # the peak of the 396.8 nm line, not the edges of the window
  peaks <- ggplot2::layer_data(p, 4)
  expect_true(any(abs(peaks$x - 396.8) < 0.1))
  background <- ggplot2::layer_data(p, 1)
  expect_true(all(background$x >= 395 & background$x <= 398))
  expect_equal(max(abs(background$y)), max(abs(bars$ymax)))
  expect_equal(p$labels$x, "nm")
  expect_equal(p$labels$y, "p")
  expect_equal(p$theme$text$size, 9)
  expect_error(plot_loadings(pca, wavelength = c(500, 510)), "Fewer than 2")
  unnamed <- stats::prcomp(unname(d$x))
  expect_error(plot_loadings(unnamed, wavelength = c(395, 398)), "named by wavelength")

  q <- contributions(pca, k = 2)
  pc <- plot_contributions(q, samples = 1, wavelength = c(392, 395), ylab = "Q")
  expect_true(all(ggplot2::layer_data(pc, 1)$x >= 392))
  expect_equal(pc$labels$y, "Q")
  skip_if_not_installed("plotly")
  pi <- plotly::plotly_build(plot_loadings(pca, components = 1, wavelength = c(395, 398),
                                           interactive = TRUE, xlab = "nm"))
  expect_true(all(pi$x$data[[1]]$x >= 395, na.rm = TRUE))
})

test_that("plot_cell_map shows the columns of a wavelength window", {
  skip_on_cran()
  d <- make_spectra(n = 20, p = 60)
  set.seed(1)
  fit <- macropca(d$x, k = 2)
  skip_if_not_installed("patchwork")
  p <- plot_cell_map(fit, wavelength = c(392, 395), xlab = "nm", base_size = 9)
  map <- p[[3]]
  expect_true(all(ggplot2::layer_data(map, 2)$x >= 392 - 0.2))
  expect_equal(map$labels$x, "nm")
  expect_equal(map$theme$text$size, 9)
  expect_error(plot_cell_map(fit, wavelength = c(500, 510)), "Fewer than 2")
})

test_that("the distance maps take color_by and the point arguments", {
  d <- make_spectra(n = 30)
  set.seed(1)
  fit <- robpca(d$x, k = 2)
  p <- plot_outlier_map(fit, color_by = "distance", point_size = 3, point_alpha = 0.4,
                        xlab = "SD", ylab = "OD", base_size = 9)
  points <- ggplot2::layer_data(p, layer_index(p, "GeomPoint"))
  expect_equal(unique(points$size), 3)
  expect_equal(unique(points$alpha), 0.4)
  expect_equal(p$labels$x, "SD")
  expect_equal(p$labels$y, "OD")
  expect_equal(p$theme$text$size, 9)
  # a size per sample, named after its expression
  conc <- seq_len(30)
  sized <- plot_outlier_map(fit, point_size = conc)
  expect_equal(sized$scales$get_scales("size")$name, "conc")
  expect_error(plot_outlier_map(fit, point_size = 1:3), "one value per sample")
  expect_error(plot_outlier_map(fit, point_alpha = 0), "point_alpha")
  # geom_point() arguments in `...` still apply
  styled <- plot_outlier_map(fit, alpha = 0.3)
  expect_equal(unique(ggplot2::layer_data(styled, layer_index(styled, "GeomPoint"))$alpha), 0.3)
  lifecycle::expect_deprecated(old <- plot_outlier_map(fit, colour_by = "distance"))
  expect_s3_class(old$scales$get_scales("fill"), "ScaleContinuous")

  s <- make_simca()
  sim <- rsimca(s$train$x, s$train$g, ncomp = 2)
  cp <- plot_coomans(sim, point_alpha = 0.5, xlab = "d(a)", base_size = 9)
  expect_equal(cp$labels$x, "d(a)")
  expect_equal(cp$theme$text$size, 9)
})

test_that("plot_influence takes color_by and the point arguments", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  d <- make_spectra(n = 30)
  res <- q_residuals(stats::prcomp(d$x), k = 2)
  p <- plot_influence(res, point_alpha = 0.5, ylab = "Q", base_size = 9)
  expect_equal(p$labels$y, "Q")
  expect_equal(p$theme$text$size, 9)
  lifecycle::expect_deprecated(plot_influence(res, colour_by = "distance"))
  # `color` in `...` is the outline of the points, not a partial `color_by`
  outlined <- plot_influence(res, color = "red")
  expect_equal(outlined$layers[[layer_index(outlined, "GeomPoint")]]$aes_params$colour, "red")
})

test_that("plot_embedding takes color_by, point_size and point_alpha", {
  set.seed(2)
  df <- data.frame(PC1 = stats::rnorm(20), PC2 = stats::rnorm(20), g = rep(c("a", "b"), 10),
                   conc = seq_len(20))
  p <- plot_embedding(df, color_by = g, point_size = 3, point_alpha = 0.5, xlab = "first",
                      base_size = 8)
  points <- ggplot2::layer_data(p, layer_index(p, "GeomPoint"))
  expect_equal(unique(points$size), 3)
  expect_equal(unique(points$alpha), 0.5)
  expect_equal(p$labels$x, "first")
  # the default size follows the text
  small <- plot_embedding(df, base_size = 5.5)
  expect_equal(unique(ggplot2::layer_data(small, layer_index(small, "GeomPoint"))$size), 1)
  lifecycle::expect_deprecated(old <- plot_embedding(df, colour = g))
  expect_equal(old$scales$get_scales("colour")$name, "g")
  lifecycle::expect_deprecated(sized <- plot_embedding(df, size = conc))
  expect_equal(sized$scales$get_scales("size")$name, "conc")
  # a number in the former `size` scales with the text, as it did
  lifecycle::expect_deprecated(scaled <- plot_embedding(df, size = 4, base_size = 5.5))
  expect_equal(unique(ggplot2::layer_data(scaled, layer_index(scaled, "GeomPoint"))$size), 2)
  lifecycle::expect_deprecated(plot_embedding(df, alpha = 0.3))
})

test_that("plot_som takes color_by and draws the prototypes in a window", {
  d <- make_spectra(n = 30, p = 60)
  set.seed(1)
  fit <- som(d$x, grid = c(4, 3))
  p <- plot_som(fit, type = "mapping", color_by = seq_len(30), point_size = 2.5)
  expect_equal(unique(ggplot2::layer_data(p, layer_index(p, "GeomPoint"))$size), 2.5)
  lifecycle::expect_deprecated(plot_som(fit, type = "mapping", colour = seq_len(30)))
  proto <- plot_som(fit, type = "prototypes", wavelength = c(392, 395), linewidth = 0.8,
                    ylab = "Intensity")
  lines <- ggplot2::layer_data(proto, 1)
  expect_true(all(lines$x >= 392 & lines$x <= 395))
  expect_equal(unique(lines$linewidth), 0.8)
  expect_equal(proto$labels$y, "Intensity")
})

test_that("the boxplots and plot_outliers take the point arguments", {
  set.seed(3)
  df <- data.frame(a = c(stats::rnorm(40), 8), b = c(stats::rnorm(40), -7))
  p <- adjusted_boxplot(df, point_size = 3, point_alpha = 0.5)
  out <- layer_with(p, "GeomPoint")
  expect_equal(out[[1]]$aes_params$size, 3)
  expect_equal(out[[1]]$aes_params$alpha, 0.5)
  g <- generalized_boxplot(df, points = "all", point_alpha = 0.6)
  expect_equal(layer_with(g, "GeomPoint")[[1]]$aes_params$alpha, 0.4)
  expect_error(adjusted_boxplot(df, point_alpha = 2), "point_alpha")

  o <- plot_outliers(df, type = "distance", xlab = "Row", point_size = 2, point_alpha = 0.5)
  expect_equal(o$labels$x, "Row")
  expect_equal(layer_with(o, "GeomPoint")[[1]]$aes_params$alpha, 0.5)
})

test_that("plot_shots takes the axis titles, point and line arguments", {
  x <- reject_shots(shots_data(), sample = Sample, drop = FALSE)
  crit <- plot_shots(x, type = "criteria", xlab = "z1", point_alpha = 0.5)
  expect_equal(crit$labels$x, "z1")
  expect_equal(layer_with(crit, "GeomPoint")[[1]]$aes_params$alpha, 0.5)
  spec <- plot_shots(x, type = "spectra", wavelength = c(395, 392), linewidth = 0.6)
  expect_equal(layer_with(spec, "GeomLine")[[1]]$aes_params$linewidth, 0.6)
  expect_true(all(ggplot2::layer_data(spec, 1)$x >= 392))
})
