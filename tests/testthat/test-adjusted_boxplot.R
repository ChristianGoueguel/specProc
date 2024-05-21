test_data <- data.frame(
  normal = rnorm(100),
  skewed = rexp(100, rate = 0.5),
  heavy_tailed = rt(100, df = 3)
)

test_that("adjboxplot function works as expected", {
  expect_error(adjboxplot(), "Missing 'data' argument.")
  expect_error(adjboxplot(data.frame(test = c("m", "t", "w", 1, 2, 3))), "Input data must be a numeric data frame.")
  expect_error(adjboxplot(test_data, .plot = 1), "Argument '.plot' must be of type boolean \\(TRUE or FALSE\\).")
  expect_error(adjboxplot(test_data, notch = 1), "Argument 'notch' must be of type boolean \\(TRUE or FALSE\\).")
  expect_error(adjboxplot(test_data, xlabels.angle = -10), "Argument 'x_axis_angle' must be a numeric value between 0 and 360.")
  expect_error(adjboxplot(test_data, xlabels.angle = 370), "Argument 'x_axis_angle' must be a numeric value between 0 and 360.")
  expect_error(adjboxplot(test_data, xlabels.vjust = -0.5), "Argument 'xlabels.vjust' must be a numeric value between 0 and 1.")
  expect_error(adjboxplot(test_data, xlabels.vjust = 1.5), "Argument 'xlabels.vjust' must be a numeric value between 0 and 1.")
  expect_error(adjboxplot(test_data, xlabels.hjust = -0.5), "Argument 'xlabels.hjust' must be a numeric value between 0 and 1.")
  expect_error(adjboxplot(test_data, xlabels.hjust = 1.5), "Argument 'xlabels.hjust' must be a numeric value between 0 and 1.")
  expect_error(adjboxplot(test_data, box.width = 0), "Argument 'box.width' must be a positive numeric value.")
  expect_error(adjboxplot(test_data, box.width = -0.5), "Argument 'box.width' must be a positive numeric value.")
  expect_error(adjboxplot(test_data, notchwidth = -0.5), "Argument 'notchwidth' must be a numeric value between 0 and 1.")
  expect_error(adjboxplot(test_data, notchwidth = 1.5), "Argument 'notchwidth' must be a numeric value between 0 and 1.")
  expect_error(adjboxplot(test_data, staplewidth = -0.5), "Argument 'staplewidth' must be a positive numeric value.")
  }
  )


