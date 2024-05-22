test_data <- tibble::tibble(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 6, 8, 10),
  z = c(3, 6, 9, 12, 15)
)

test_that("corr function works as expected", {
  result <- corr(test_data, y)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$method, rep("pearson", 2))

  expect_error(corr(list(1, 2, 3), y), "Input '.data' must be a numeric data frame")
  expect_error(corr(test_data, w), "'response_var' not found in the data frame")
  expect_error(corr(test_data, y, method = "invalid"), "Invalid method specified.")
  expect_error(corr(test_data, y, .plot = 1), "'.plot' must be of type boolean \\(TRUE or FALSE\\)")

  result_spearman <- corr(test_data, y, method = "spearman")
  expect_equal(result_spearman$method, rep("spearman", 2))
  result_kendall <- corr(test_data, y, method = "kendall")
  expect_equal(result_kendall$method, rep("kendall", 2))
  result_chatterjee <- corr(test_data, y, method = "chatterjee")
  expect_equal(result_chatterjee$method, rep("chatterjee", 2))

  result_plot <- corr(test_data, y, .plot = TRUE)
  expect_type(result_plot$plot, "list")

  result_interactive <- corr(test_data, y, .plot = TRUE, .interactive = TRUE)
  expect_s3_class(result_interactive, "plotly")
  }
  )
