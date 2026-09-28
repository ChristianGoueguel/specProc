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

test_that("correlation validates its inputs", {
  expect_error(correlation(list(1, 2, 3), y), "Input 'x' must be a numeric data frame")
  expect_error(correlation(test_data, w), "'var' not found in the data frame")
  expect_error(correlation(test_data, y, method = "invalid"), "Invalid method specified.")
  expect_error(correlation(test_data, y, plot = 1), "'plot' must be of type boolean \\(TRUE or FALSE\\)")
})
