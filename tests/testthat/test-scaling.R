set.seed(1)
m <- matrix(stats::rnorm(40, 10, 3), 8, 5, dimnames = list(NULL, paste0("v", 1:5)))

test_that("msc removes additive and multiplicative effects", {
  ref <- sin(seq(0, pi, length.out = 50))
  a <- stats::runif(10, -1, 1)
  b <- stats::runif(10, 0.5, 2)
  x <- a + b * matrix(ref, 10, 50, byrow = TRUE)
  res <- msc(x, xref = ref)
  expect_named(res, c("correction", "offset", "slope", "reference"))
  expect_equal(as.matrix(res$correction), matrix(ref, 10, 50, byrow = TRUE), ignore_attr = TRUE)
  expect_equal(res$offset, a)
  expect_equal(res$slope, b)
  expect_equal(as.matrix(msc(x, xref = ref, drop.offset = FALSE)$correction), x / b, ignore_attr = TRUE)
  # default reference: corrected spectra all become proportional to the median spectrum
  res2 <- msc(x)
  expect_lt(max(apply(as.matrix(res2$correction), 2, stats::sd)), 1e-10)
  # piecewise MSC
  res3 <- msc(x, xref = ref, window = list(1:25, 26:50))
  expect_equal(dim(res3$slope), c(10L, 2L))
  expect_equal(as.matrix(res3$correction), as.matrix(res$correction), ignore_attr = TRUE)
  expect_error(msc(x, xref = 1:3), "same length")
  expect_error(msc(x, window = list(1)), "window")
})

test_that("snv standardizes each spectrum", {
  res <- snv(m)
  cm <- as.matrix(res$correction)
  expect_equal(rowMeans(cm), rep(0, 8), tolerance = 1e-12)
  expect_equal(apply(cm, 1, stats::sd), rep(1, 8), tolerance = 1e-12)
  expect_equal(names(res$correction), colnames(m))
  expect_equal(res$means, rowMeans(m))
  expect_warning(snv(rbind(m, 1)), "constant")
  expect_error(snv(data.frame(a = "x")), "numeric")
})

test_that("pareto divides by the square root of the standard deviation", {
  res <- pareto_scale(m)
  expect_equal(apply(res, 2, stats::sd), sqrt(apply(m, 2, stats::sd)))
  expect_s3_class(pareto_scale(as.data.frame(m)), "tbl_df")
  expect_warning(pareto_scale(cbind(m, 1)), "zero standard deviation")
  m_na <- m
  m_na[1, 1] <- NA
  expect_false(anyNA(pareto_scale(m_na, drop.na = TRUE)[-1, ]))
})

test_that("minmax rescales to [a, b]", {
  expect_equal(minmax(c(2, 4, 6, 10)), c(0, 0.25, 0.5, 1))
  expect_equal(range(minmax(stats::rnorm(20), a = -1, b = 1)), c(-1, 1))
  expect_equal(minmax(c(1, NA, 3)), c(0, 1))
  expect_equal(minmax(c(1, NA, 3), drop.na = FALSE), c(0, NA, 1))
  expect_equal(minmax(c(5, 5)), c(0, 0))
  expect_error(minmax(1:3, a = 1, b = 0), "greater")
})

test_that("center subtracts column means or medians", {
  expect_equal(colMeans(center(m)), rep(0, 5), tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(apply(center(m, "median"), 2, stats::median), rep(0, 5), ignore_attr = TRUE)
  expect_equal(attr(center(m), "center"), colMeans(m))
  m_na <- m
  m_na[1, 1] <- NA
  expect_equal(sum(is.na(center(m_na, drop.na = TRUE))), 1)
  expect_error(center(m, "mode"), "Invalid method")
  expect_error(center("a"), "numeric")
})

test_that("normalize implements area, background and internal standard", {
  x <- data.frame(`400` = c(1, 2), `401` = c(3, 6), `402` = c(1, 2), check.names = FALSE)
  area <- normalize(x, method = "area")
  expect_equal(rowSums(area), c(1, 1))
  internal <- normalize(x, method = "internal", wlength = "401")
  expect_equal(internal$`401`, c(1, 1))
  bkg <- x / 2
  expect_equal(as.matrix(normalize(x, "background", bkg = bkg)), matrix(2, 2, 3), ignore_attr = TRUE)
  expect_equal(names(normalize(x, "background", bkg = as.matrix(bkg), wlength = "401")), "401")
  expect_error(normalize(x, "internal"), "'wlength' argument is missing")
  expect_error(normalize(x, "background"), "'bkg' argument is missing")
  expect_error(normalize(x, "foo"), "should be one of")
})

test_that("poisson_scale scales by the square root of the means", {
  x <- abs(m)
  res <- poisson_scale(x)
  mu <- colMeans(x)
  sc <- sqrt(mu + max(mu) * 0.03)
  expect_equal(res$sc, sc)
  expect_equal(res$xs, sweep(x, 2, sc, "/"))
  expect_equal(poisson_scale(x, sc = res$sc), res$xs)
  res2 <- poisson_scale(x, options = list(mode = 2, offset = 0))
  expect_equal(res2$sc, sqrt(rowMeans(x)))
  expect_error(poisson_scale(x, sc = 1:2), "one scale per column")
  expect_error(poisson_scale(x, options = list(mode = 3)), "mode")
})

test_that("plot_spectra draws one line per spectrum", {
  spec <- make_spectra(n = 3, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  p <- plot_spectra(df)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(built$data[[1]]$group)), 3)
  df$conc <- 1:3
  df$id <- c("a", "b", "c")
  expect_s3_class(plot_spectra(df, id = id, colvar = conc), "ggplot")
  expect_s3_class(plot_spectra(df, id = "id", colvar = "conc"), "ggplot")
  expect_error(plot_spectra(df), "wavelengths")
  expect_error(plot_spectra(df, id = zz), "does not exist")
})

test_that("plot_spectra offsets successive spectra", {
  spec <- make_spectra(n = 3, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  base <- ggplot2::ggplot_build(plot_spectra(df))$data[[1]]
  shifted <- ggplot2::ggplot_build(plot_spectra(df, offset = c(0.5, 10)))$data[[1]]
  third <- base$group == 3
  expect_equal(shifted$y[third], base$y[third] + 2 * 10)
  expect_equal(shifted$x[third], base$x[third] + 2 * 0.5)
  # a single number is a vertical offset, and the first spectrum stays in place
  vertical <- ggplot2::ggplot_build(plot_spectra(df, offset = 10))$data[[1]]
  expect_equal(vertical$x, base$x)
  expect_equal(vertical$y[base$group == 1], base$y[base$group == 1])
  expect_error(plot_spectra(df, offset = "a"), "offset")
  expect_error(plot_spectra(df, offset = 1:3), "offset")
})

test_that("plot_spectra draws one panel per group, with the offsets restarting in each", {
  spec <- make_spectra(n = 5, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  df$site <- c("a", "b", "a", "b", "a")
  p <- plot_spectra(df, panel = site, offset = 10)
  built <- ggplot2::ggplot_build(p)
  layout <- built$layout$layout
  expect_equal(nrow(layout), 2)
  expect_equal(max(layout$COL), 1) # vertical: one column
  wide <- ggplot2::ggplot_build(plot_spectra(df, panel = "site", layout = "horizontal"))$layout$layout
  expect_equal(max(wide$ROW), 1) # horizontal: one row
  # the third spectrum is the second of panel "a": shifted once, not twice
  base <- ggplot2::ggplot_build(plot_spectra(df[, 1:50]))$data[[1]]
  shifted <- built$data[[1]]
  expect_equal(shifted$y[shifted$group == 3], base$y[base$group == 3] + 10)
  expect_equal(shifted$y[shifted$group == 2], base$y[base$group == 2])
  expect_error(plot_spectra(df, panel = zz), "does not exist")
  expect_error(plot_spectra(df, panel = site, layout = "diagonal"), "should be one of")
})

test_that("plot_spectra draws grid lines on request", {
  spec <- make_spectra(n = 2, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  grid_of <- function(p) ggplot2::calc_element("panel.grid.major", ggplot2::complete_theme(p$theme))
  expect_match(class(grid_of(plot_spectra(df)))[1], "element_blank")
  expect_match(class(grid_of(plot_spectra(df, grid = TRUE)))[1], "element_line")
  expect_error(plot_spectra(df, grid = "yes"), "grid")
})

test_that("plot_spectra fills the area under the spectra with color_as = 'fill'", {
  spec <- make_spectra(n = 3, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  df$conc <- 1:3
  p <- plot_spectra(df, colvar = conc, color_as = "fill", offset = 10)
  expect_s3_class(p$layers[[1]]$geom, "GeomRibbon")
  expect_equal(p$labels$fill, "conc")
  built <- ggplot2::ggplot_build(p)$data[[1]]
  base <- ggplot2::ggplot_build(plot_spectra(df[, 1:50]))$data[[1]]
  # stacked upwards: the top spectrum is drawn first, each filled down to its baseline
  top <- built$group == 1
  expect_equal(built$ymax[top], base$y[base$group == 3] + 20)
  expect_equal(unique(built$ymin[top]), 20)
  expect_equal(unique(built$alpha), 1)
  expect_equal(length(unique(built$fill)), 3)
  # overlaid spectra are translucent; the colors of id fill the areas without colvar
  overlaid <- ggplot2::ggplot_build(plot_spectra(df[, 1:50], color_as = "fill"))$data[[1]]
  expect_true(all(overlaid$alpha < 1))
  df$id <- c("x", "y", "z")
  expect_s3_class(plot_spectra(df[, -51], id = id, color_as = "fill"), "ggplot")
  expect_error(plot_spectra(df, colvar = conc, color_as = "area"), "should be one of")
})

test_that("tukey_gh distribution functions are consistent", {
  u <- c(0.05, 0.3, 0.5, 0.9)
  q <- tukey_gh(u, type = "q", g = 0.3, h = 0.1)
  expect_equal(tukey_gh(q, type = "p", g = 0.3, h = 0.1), u, tolerance = 1e-8)
  expect_equal(tukey_gh(q, type = "p", g = 0.3, h = 0), stats::pnorm(log(1 + 0.3 * q) / 0.3))
  x <- seq(-3, 3, length.out = 7)
  expect_equal(tukey_gh(x, type = "d"), stats::dnorm(x))
  expect_equal(tukey_gh(x, type = "p", location = 1, scale = 2), stats::pnorm(x, 1, 2))
  dens <- stats::integrate(function(x) tukey_gh(x, type = "d", g = 0.3, h = 0.1), -Inf, Inf)$value
  expect_equal(dens, 1, tolerance = 1e-4)
  expect_length(tukey_gh(type = "r", n = 10, g = 0.2, h = 0.1), 10)
  expect_error(tukey_gh(1, type = "r"), "'n' is required")
  expect_error(tukey_gh(1, type = "z"), "Invalid 'type'")
  expect_error(tukey_gh(1, h = -1), "Negative kurtosis")
})

test_that("normalize implements the L1, L2 and maximum norms", {
  x <- data.frame(`400` = c(1, -2), `401` = c(3, 6), `402` = c(4, 2), check.names = FALSE)
  m <- as.matrix(x)
  expect_equal(as.matrix(normalize(x, "l1")), m / rowSums(abs(m)), ignore_attr = TRUE)
  expect_equal(rowSums(as.matrix(normalize(x, "l2"))^2), c(1, 1))
  expect_equal(as.matrix(normalize(x, "max")), m / c(4, 6), ignore_attr = TRUE)
  expect_named(normalize(x, "l2"), names(x))
  # L1 differs from the area when the spectrum changes sign
  expect_false(isTRUE(all.equal(normalize(x, "l1"), normalize(x, "area"))))
  zero <- rbind(x, 0)
  expect_warning(out <- normalize(zero, "l2"), "zero or missing l2 norm")
  expect_true(all(is.na(unlist(out[3, ]))))
})
