# Spectra of two emitters: lines at 400 and 420 nm vary together, the line
# at 450 nm varies against them.
make_line_spectra <- function(n = 40, seed = 1) {
  set.seed(seed)
  wl <- seq(390, 460, by = 0.1)
  line <- function(center) exp(-(wl - center)^2 / (2 * 0.15^2))
  a <- stats::rnorm(n, 10, 2)
  b <- stats::rnorm(n, 5, 1)
  x <- outer(a, 100 * line(400) + 60 * line(420)) + outer(b, 40 * line(435)) -
    outer(a, 30 * line(450)) + matrix(stats::rnorm(n * length(wl), sd = 0.5), n)
  colnames(x) <- format(wl, nsmall = 1, trim = TRUE)
  x
}

test_lines <- function() {
  tibble::tibble(species = c("Ca II", "Ca II", "K I", "Na I", "Na I"),
                 stage = c(2L, 2L, 1L, 1L, 1L),
                 wavelength = c(400.03, 420.05, 435.02, 449.98, 450.3),
                 relative_intensity = c(1, 0.5, 1, 0.2, 1))
}

test_that("loading_peaks finds the lines with their sign and matches them", {
  x <- make_line_spectra()
  pca <- stats::prcomp(x)
  pk <- loading_peaks(pca, components = 1, top = 3, lines = test_lines(), tol = 0.1)
  expect_s3_class(pk, "tbl_df")
  expect_equal(nrow(pk), 3)
  expect_equal(sort(pk$wavelength), c(400, 420, 450))
  # oriented so that the largest absolute loading (400 nm) is positive
  expect_equal(pk$sign[pk$wavelength == 400], "positive")
  expect_equal(pk$sign[pk$wavelength == 450], "negative")
  expect_equal(pk$sign[pk$wavelength == 420], "positive")
  # the nearest line within tol, and all the candidates from the nearest
  expect_equal(pk$species[pk$wavelength == 450], "Na I")
  expect_equal(pk$line_wavelength[pk$wavelength == 450], 449.98)
  expect_equal(pk$candidates[pk$wavelength == 400], "Ca II 400.03")
  expect_false(any(pk$derivative))

  pk0 <- loading_peaks(pca, top = 0)
  expect_equal(nrow(pk0), 0)
  expect_named(loading_peaks(pca, top = 2), c("component", "variable", "wavelength", "value",
                                              "sign", "derivative"))
})

test_that("contribution is the variance explained by wavelength", {
  x <- make_line_spectra()
  pca <- stats::prcomp(x)
  pk <- loading_peaks(pca, components = 1:2, type = "contribution", top = 2)
  expect_equal(unique(pk$component), "PC1-2")
  expect_true(all(pk$sign == "positive"))
  expect_false("derivative" %in% names(pk))
  # with all the components, the explained variance is the variance
  full <- loading_curves(loading_parts(pca), seq_len(ncol(pca$rotation)), "contribution")
  expect_equal(full$value, unname(apply(x, 2, stats::var)), tolerance = 1e-8)
  # on the scale of the data when the PCA is autoscaled
  scaled <- stats::prcomp(x, scale. = TRUE)
  full <- loading_curves(loading_parts(scaled), seq_len(ncol(scaled$rotation)), "contribution")
  expect_equal(full$value, unname(apply(x, 2, stats::var)), tolerance = 1e-8)
})

test_that("a derivative shape is flagged", {
  set.seed(2)
  wl <- seq(390, 410, by = 0.1)
  shift <- stats::rnorm(30, sd = 0.05)
  x <- t(vapply(shift, function(s) 100 * exp(-(wl - 400 - s)^2 / (2 * 0.2^2)), numeric(length(wl))))
  colnames(x) <- wl
  pk <- loading_peaks(stats::prcomp(x), components = 1, top = 2)
  expect_true(all(pk$derivative))
  expect_setequal(pk$sign, c("positive", "negative"))
})

test_that("plot_loadings works with prcomp and robust fits", {
  x <- make_line_spectra()
  pca <- stats::prcomp(x)
  p <- plot_loadings(pca, lines = test_lines(), spectra = x)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(nlevels(p$data$panel), 3)
  expect_match(levels(p$data$panel)[1], "^PC1 \\([0-9.]+%\\)$")
  expect_s3_class(plot_loadings(pca, type = "contribution", top = 0), "ggplot")
  # labels: bold when matched to a line
  labels <- built$data[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))]]
  expect_true(any(labels$fontface == "bold"))

  set.seed(3)
  fit <- robpca(x, k = 2)
  p <- plot_loadings(fit, spectra = x[1, ])
  expect_s3_class(p, "ggplot")
  expect_equal(nlevels(p$data$panel), 2)
  # all the components of a robust fit: no percentage
  expect_equal(levels(plot_loadings(fit, type = "contribution")$data$panel), "PC1-2")
  expect_s3_class(plot_loadings(rospca(x, k = 2), top = 3), "ggplot")
  expect_s3_class(plot_loadings(macropca(x, k = 2), top = 3), "ggplot")
})

test_that("plot_loadings can be interactive", {
  skip_if_not_installed("plotly")
  x <- make_line_spectra()
  p <- plot_loadings(stats::prcomp(x), components = 1:2, lines = test_lines(), spectra = x,
                     interactive = TRUE)
  expect_s3_class(p, "plotly")
  built <- plotly::plotly_build(p)
  expect_equal(length(built$x$data), 6)
})

test_that("variables without names are numbered", {
  x <- unname(make_line_spectra())
  p <- plot_loadings(stats::prcomp(x), components = 1)
  expect_equal(p$labels$x, "Variable")
  expect_error(plot_loadings(stats::prcomp(x), lines = test_lines()), "wavelength")
})

test_that("plot_loadings checks its arguments", {
  x <- make_line_spectra()
  pca <- stats::prcomp(x)
  expect_error(plot_loadings(list()), "prcomp")
  expect_error(plot_loadings(pca, components = 0), "components")
  expect_error(plot_loadings(pca, components = c(1, 1)), "components")
  expect_error(plot_loadings(pca, top = -1), "top")
  expect_error(plot_loadings(pca, span = 0), "span")
  expect_error(plot_loadings(pca, tol = -1), "tol")
  expect_error(plot_loadings(pca, lines = data.frame(a = 1)), "libs_lines")
  expect_error(plot_loadings(pca, spectra = x[, 1:5]), "variables")
  expect_error(plot_loadings(pca, spectra = "a"), "spectra")
  expect_error(plot_loadings(pca, type = "scores"), "arg")
})

test_that("spread_positions keeps labels apart and close to their peaks", {
  pos <- spread_positions(c(10, 10.5, 11, 50), gap = 2)
  expect_true(all(diff(sort(pos)) >= 2 - 1e-12))
  expect_equal(pos[4], 50)
  expect_equal(mean(pos[1:3]), 10.5)
  expect_equal(spread_positions(c(5, 1), gap = 1), c(5, 1))
})

test_that("default variable names are not wavelengths", {
  expect_null(names_to_wavelength(paste0("V", 1:3)))
  expect_null(names_to_wavelength(c("a", "b")))
  expect_null(names_to_wavelength(NULL))
  expect_equal(names_to_wavelength(c("X200.5", "201")), c(200.5, 201))
  x <- make_line_spectra()
  colnames(x) <- paste0("V", seq_len(ncol(x)))
  expect_equal(plot_loadings(stats::prcomp(x), components = 1)$labels$x, "Variable")
})
