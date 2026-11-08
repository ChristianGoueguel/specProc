set.seed(7)
# two detector segments, with a gap between them
mc_wl <- c(seq(400, 401, by = 0.1), seq(410, 411, by = 0.1))
mc_spectra <- matrix(stats::rnorm(60 * length(mc_wl), 100, 5), 60,
                     dimnames = list(NULL, format(mc_wl)))
mc_spectra[, 1] <- 100 + stats::rexp(60)             # right-skewed
mc_spectra[, 2] <- 100 - stats::rexp(60)             # left-skewed
mc_spectra[, 3] <- pmin(mc_spectra[, 3], 98)         # saturated
mc_data <- data.frame(id = 1:60, sample = rep(c("a", "b", "c"), each = 20), mc_spectra,
                      check.names = FALSE)

test_that("plot_medcouple draws the medcouple of each wavelength", {
  p <- plot_medcouple(mc_data)
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$wavelength, mc_wl)
  expect_equal(p$data$medcouple, unname(apply(mc_spectra, 2, medcouple)))
  expect_gt(p$data$medcouple[1], 0)
  expect_lt(p$data$medcouple[2], 0)
  expect_lt(p$data$medcouple[3], -0.5)
  expect_equal(p$labels$title, "Medcouple by wavelength")
  expect_equal(p$labels$subtitle, "60 spectra")
  expect_equal(p$labels$y, "Medcouple")
  # the columns given in any order, and as a matrix
  expect_equal(plot_medcouple(mc_data[c(1, 2, rev(seq_along(mc_wl)) + 2)])$data, p$data)
  expect_equal(plot_medcouple(mc_spectra)$data, p$data)
  w <- plot_medcouple(mc_data, wavelength = c(400, 401), title = "Window", ylab = "MC")
  expect_equal(w$data$wavelength, mc_wl[mc_wl <= 401])
  expect_equal(w$labels$title, "Window")
  expect_equal(w$labels$y, "MC")
})

test_that("plot_medcouple draws a heatmap of the groups", {
  h <- plot_medcouple(mc_data, group = sample)
  expect_s3_class(h, "ggplot")
  expect_equal(levels(h$data$group), c("a", "b", "c"))
  expect_equal(h$labels$subtitle, "One row per sample, 20 spectra each")
  expect_equal(h$labels$y, "sample")
  b <- h$data[h$data$group == "b", ]
  b <- b[order(b$wavelength), ]
  expect_equal(b$medcouple, unname(apply(mc_spectra[21:40, ], 2, medcouple)))
  # the first group on top
  expect_equal(unique(h$data$row[h$data$group == "a"]), 3)
  # tiles halfway to the neighbors, not across the gap between detectors
  expect_equal(max(h$data$xmax[h$data$wavelength < 405]), 401.05)
  expect_equal(min(h$data$xmin[h$data$wavelength > 405]), 409.95)
  expect_equal(plot_medcouple(mc_data, group = "sample")$data, h$data)
  df <- mc_data
  df$sample <- factor(df$sample, levels = c("c", "a", "b"))
  expect_equal(levels(plot_medcouple(df, group = sample)$data$group), c("c", "a", "b"))
  df$sample <- c(rep("a", 10), rep("b", 50))
  expect_equal(plot_medcouple(df, group = sample)$labels$subtitle,
               "One row per sample, 10 to 50 spectra")
})

test_that("plot_medcouple handles missing values and checks its arguments", {
  df <- mc_data
  df[["400.5"]][4] <- NA
  expect_false(400.5 %in% plot_medcouple(df)$data$wavelength)
  expect_true(400.5 %in% plot_medcouple(df, drop_na = TRUE)$data$wavelength)
  df[3:24] <- NA_real_
  expect_error(plot_medcouple(df), "drop_na")
  expect_error(plot_medcouple(mc_data, group = batch), "not found")
  df <- mc_data
  df$sample[2] <- NA
  expect_error(plot_medcouple(df, group = sample), "missing values")
  expect_error(plot_medcouple(data.frame(a = 1:3, b = 4:6)), "spectral columns")
  expect_error(plot_medcouple(mc_data, wavelength = c(500, 600)), "in the wavelength range")
  expect_error(plot_medcouple("x"), "data frame")
  expect_error(plot_medcouple(mc_data, color = "red"), "two colors")
  expect_error(plot_medcouple(mc_data, color = c("red", "nocolor")), "color")
  expect_error(plot_medcouple(mc_data, linewidth = 0), "linewidth")
})

test_that("plot_medcouple scales the colors from -1 to 1", {
  expect_equal(medcouple_colours(c("blue", "red")), c("blue", "white", "red"))
  expect_equal(medcouple_colours(grDevices::rainbow(5)), grDevices::rainbow(5))
  h <- plot_medcouple(mc_data, group = sample, color = c("navy", "gold", "darkred"))
  scale <- h$scales$get_scales("fill")
  expect_equal(scale$get_limits(), c(-1, 1))
  expect_equal(scale$map(c(-1, 0, 1)), unname(grDevices::rgb(t(grDevices::col2rgb(
    c("navy", "gold", "darkred"))), maxColorValue = 255)))
  # the bars in the colors of the ends of the scale, by sign
  p <- plot_medcouple(mc_data, color = c("navy", "gold", "darkred"))
  bars <- ggplot2::layer_data(p, 1)
  expect_equal(bars$colour, ifelse(p$data$medcouple >= 0, "darkred", "navy"))
})

test_that("plot_medcouple has interactive versions", {
  skip_if_not_installed("plotly")
  p <- plot_medcouple(mc_data, interactive = TRUE)
  expect_s3_class(p, "plotly")
  expect_s3_class(plotly::plotly_build(p), "plotly")
  h <- plot_medcouple(mc_data, group = sample, interactive = TRUE)
  expect_s3_class(h, "plotly")
  built <- plotly::plotly_build(h)$x$data[[1]]
  # an empty column in the gap between the detectors
  expect_equal(length(built$x), length(mc_wl) + 1)
  expect_true(all(is.na(built$z[, which(built$x > 401 & built$x < 410)])))
})

test_that("the tiles of a heatmap stop at the gaps between detectors", {
  tiles <- wavelength_tiles(c(1, 2, 3, 10, 11))
  expect_equal(tiles$xmin, c(0.5, 1.5, 2.5, 9.5, 10.5))
  expect_equal(tiles$xmax, c(1.5, 2.5, 3.5, 10.5, 11.5))
  gapped <- heatmap_gaps(c(1, 2, 3, 10, 11), matrix(1, 2, 5))
  expect_equal(gapped$wl, c(1, 2, 3, 6.5, 10, 11))
  expect_true(all(is.na(gapped$z[, 4])))
  expect_equal(heatmap_gaps(1:3, matrix(1, 1, 3))$wl, 1:3)
})
