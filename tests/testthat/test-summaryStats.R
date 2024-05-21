test_data <- tibble::tibble(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 6, 8, 10),
  z = c(3, 6, NA, 12, 15)
)

test_that("summaryStats function works as expected", {
  expect_error(summaryStats(NULL), "Data must be provided")
  expect_error(summaryStats(list(1, 2, 3)), "Data must be of class data.frame, tbl_df, or tbl")
  expect_error(summaryStats(test_data, var = list(1, 2, 3)), "'var' must be either a character vector or a numeric vector")
  expect_error(summaryStats(test_data, var = c("x", "w")), "One or more variables specified in 'var' are not present in the data")
  expect_error(summaryStats(test_data, digits = -1), "'digits' must be a non-negative integer")
  expect_error(summaryStats(test_data, digits = 1.5), "'digits' must be a non-negative integer")
  }
  )
