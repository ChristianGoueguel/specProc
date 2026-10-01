set.seed(1)
test_data <- tibble::tibble(y = stats::rnorm(50))
test_data$a <- 2 * test_data$y + stats::rnorm(50, sd = 0.5)
test_data$b <- -test_data$y + stats::rnorm(50)
test_data$c <- stats::rnorm(50)

test_that("correlation returns one row per other variable, sorted", {
  result <- correlation(test_data, y)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("variable", ".correlation", "method"))
  expect_equal(nrow(result), 3)
  expect_equal(result$method, rep("pearson", 3))
  expect_equal(result$variable[1], "a")
  expect_true(!is.unsorted(rev(result$.correlation)))
  expect_equal(result$.correlation[result$variable == "a"], stats::cor(test_data$a, test_data$y))
  expect_equal(correlation(test_data, "y"), result)
})

test_that("all correlation methods work and match their references", {
  for (m in c("spearman", "kendall")) {
    res <- correlation(test_data, y, method = m)
    expect_equal(res$.correlation[res$variable == "b"], stats::cor(test_data$b, test_data$y, method = m))
  }
  bic <- correlation(test_data, y, method = "bicor")
  expect_equal(bic$.correlation[bic$variable == "a"], biweight_midcorrelation(test_data$a, test_data$y))
  xi <- correlation(test_data, y, method = "chatterjee")
  expect_equal(xi$method, rep("chatterjee", 3))
  expect_true(all(xi$.correlation <= 1))
})

test_that("correlation handles missing values pairwise", {
  d <- test_data
  d$a[1:3] <- NA
  res <- correlation(d, y)
  ok <- !is.na(d$a)
  expect_equal(res$.correlation[res$variable == "a"], stats::cor(d$a[ok], d$y[ok]))
})

test_that("correlation plots", {
  result_plot <- correlation(test_data, y, plot = TRUE)
  expect_named(result_plot, c("correlation", "plot"))
  expect_s3_class(result_plot$plot, "ggplot")
  # sorted, colored by sign, with significance thresholds
  built <- ggplot2::ggplot_build(result_plot$plot)
  expect_equal(levels(result_plot$plot$data$variable),
               result_plot$correlation$variable[order(result_plot$correlation$.correlation)])
  expect_match(result_plot$plot$labels$title, "Pearson correlation with y")
  top <- correlation(test_data, y, plot = TRUE, top = 1)
  expect_equal(nrow(top$plot$data), 1)
  expect_equal(nrow(top$correlation), nrow(result_plot$correlation))
  xi <- correlation(test_data, y, method = "chatterjee", plot = TRUE)
  expect_null(xi$plot$labels$subtitle)   # no t-based threshold for xi
  expect_error(correlation(test_data, y, plot = TRUE, color = 1), "color")
  expect_error(correlation(test_data, y, plot = TRUE, top = 0), "top")
  # variables named by wavelength: a correlation spectrum
  set.seed(2)
  spectra <- as.data.frame(matrix(rnorm(30 * 50), 30, 50, dimnames = list(NULL, seq(400, 449))))
  spectra$y <- spectra[["420"]] + rnorm(30, sd = 0.2)
  spec <- correlation(spectra, y, plot = TRUE)
  expect_equal(spec$plot$labels$x, "Wavelength (nm)")
  expect_equal(spec$plot$data$wavelength, 400:449)
  skip_if_not_installed("plotly")
  result_interactive <- correlation(test_data, y, plot = TRUE, interactive = TRUE)
  expect_s3_class(result_interactive, "plotly")
  spec_interactive <- correlation(spectra, y, plot = TRUE, interactive = TRUE)
  expect_s3_class(spec_interactive, "plotly")
  expect_equal(plotly::plotly_build(spec_interactive)$x$data[[1]]$type, "scattergl")
})

test_that("correlation takes several responses, each with its own observations", {
  d <- test_data
  d$z <- d$a + stats::rnorm(50)
  d$z[1:40] <- NA
  res <- correlation(d, c(y, z))
  expect_named(res, c("outcome", "variable", ".correlation", "method"))
  expect_equal(unique(res$outcome), c("y", "z"))
  # y keeps all its observations although z is mostly missing
  expect_equal(res$.correlation[res$outcome == "y" & res$variable == "a"], stats::cor(d$a, d$y))
  ok <- !is.na(d$z)
  expect_equal(res$.correlation[res$outcome == "z" & res$variable == "b"], stats::cor(d$b[ok], d$z[ok]))
  expect_equal(correlation(d, dplyr::all_of(c("y", "z"))), res)
  expect_equal(nrow(correlation(d, c(y, z), method = "kendall")), nrow(res))
  expect_error(correlation(d[c("y", "z")], c(y, z)), "other than")
})

test_that("several responses give a heatmap", {
  set.seed(3)
  spectra <- as.data.frame(matrix(stats::rnorm(30 * 40), 30, 40,
                                  dimnames = list(NULL, c(seq(400, 419), seq(450, 469)))))
  spectra$y1 <- spectra[["405"]] + stats::rnorm(30, sd = 0.2)
  spectra$y2 <- -spectra[["460"]] + stats::rnorm(30, sd = 0.2)
  res <- correlation(spectra, c(y1, y2), plot = TRUE)
  expect_s3_class(res$plot, "ggplot")
  expect_equal(res$plot$labels$x, "Wavelength (nm)")
  built <- ggplot2::ggplot_build(res$plot)$data[[1]]
  expect_equal(nrow(built), 80)
  # tiles are as wide as the channel spacing, and the detector gap stays empty
  expect_equal(range(built$xmax - built$xmin), c(1, 1))
  expect_false(any(built$xmin < 449.5 & built$xmax > 419.5))
  # a fixed scale, the first response on top
  expect_equal(res$plot$scales$get_scales("fill")$limits, c(-1, 1))
  expect_equal(levels(res$plot$data$outcome), c("y2", "y1"))
  # variables that are not wavelengths: tiles labeled with their value, top ones only
  bars <- correlation(test_data |> dplyr::mutate(w = y^2), c(y, w), plot = TRUE, top = 2)
  expect_equal(nlevels(bars$plot$data$variable), 2)
  xi <- correlation(spectra, c(y1, y2), method = "chatterjee", plot = TRUE)
  expect_equal(xi$plot$scales$get_scales("fill")$limits, c(0, 1))
  skip_if_not_installed("plotly")
  interactive <- correlation(spectra, c(y1, y2), plot = TRUE, interactive = TRUE)
  expect_s3_class(interactive, "plotly")
  expect_equal(plotly::plotly_build(interactive)$x$data[[1]]$type, "heatmap")
})

test_that("cluster orders the responses by their correlation profiles", {
  set.seed(4)
  spectra <- as.data.frame(matrix(stats::rnorm(40 * 30), 40, 30, dimnames = list(NULL, 401:430)))
  # y1 and y3 follow channel 405, y2 and y4 channel 420
  spectra$y1 <- spectra[["405"]] + stats::rnorm(40, sd = 0.3)
  spectra$y2 <- spectra[["420"]] + stats::rnorm(40, sd = 0.3)
  spectra$y3 <- spectra[["405"]] + stats::rnorm(40, sd = 0.3)
  spectra$y4 <- spectra[["420"]] + stats::rnorm(40, sd = 0.3)
  plain <- correlation(spectra, c(y1, y2, y3, y4), plot = TRUE)
  expect_null(plain$clustering)
  skip_if_not_installed("patchwork")
  res <- correlation(spectra, c(y1, y2, y3, y4), plot = TRUE, cluster = TRUE)
  expect_s3_class(res$clustering, "hclust")
  expect_s3_class(res$plot, "patchwork")
  order <- res$clustering$labels[res$clustering$order]
  expect_equal(abs(diff(match(c("y1", "y3"), order))), 1)
  expect_equal(abs(diff(match(c("y2", "y4"), order))), 1)
  # the heatmap rows follow the clustering, the first leaf on top
  expect_equal(rev(levels(res$plot[[2]]$data$outcome)), order)
  # the table is unchanged
  expect_equal(res$correlation, plain$correlation)
  # the dendrogram: 3 segments per merge, leaves at the row positions
  segments <- dendrogram_segments(res$clustering, stats::setNames(4:1, order))
  expect_equal(nrow(segments), 9)
  expect_setequal(segments$y[segments$x == 0], 1:4)
  expect_error(correlation(spectra, c(y1, y2), cluster = NA), "cluster")
})

test_that("correlation validates its inputs", {
  expect_error(correlation(list(1, 2, 3), y), "Input 'x' must be a numeric data frame")
  expect_error(correlation(test_data, w), "'var' not found in the data frame")
  expect_error(correlation(test_data, y, method = "invalid"), "Invalid method specified.")
  expect_error(correlation(test_data, y, plot = 1), "'plot' must be of type boolean \\(TRUE or FALSE\\)")
})
