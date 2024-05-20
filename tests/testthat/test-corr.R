test_data <- tibble::tibble(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 6, 8, 10),
  z = c(3, 6, 9, 12, 15)
)

# Test cases
test_that("corr function works as expected", {
  # Test with valid input
  result <- corr(test_data, y)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$method, rep("pearson", 2))

  # Test with invalid data frame
  expect_error(corr(list(1, 2, 3), y), "Input must be a numeric data frame")

  # Test with non-existing response variable
  expect_error(corr(test_data, w), "Response variable not found in the data frame")

  # Test with invalid method
  expect_error(corr(test_data, y, method = "invalid"), "Invalid method specified. Available methods are: pearson, spearman, kendall and chatterjee")

  # Test with non-boolean .plot
  expect_error(corr(test_data, y, .plot = 1), "'.plot' must be of type boolean \\(TRUE or FALSE\\)")

  # Test with different methods
  result_spearman <- corr(test_data, y, method = "spearman")
  expect_equal(result_spearman$method, rep("spearman", 2))

  result_kendall <- corr(test_data, y, method = "kendall")
  expect_equal(result_kendall$method, rep("kendall", 2))

  result_chatterjee <- corr(test_data, y, method = "chatterjee")
  expect_equal(result_chatterjee$method, rep("chatterjee", 2))

  # Test with .plot = TRUE
  result_plot <- corr(test_data, y, .plot = TRUE)
  expect_type(result_plot$plot, "list")

  # Test with .interactive = TRUE
  result_interactive <- corr(test_data, y, .plot = TRUE, .interactive = TRUE)
  expect_s3_class(result_interactive, "plotly")
})
