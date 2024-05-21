test_x <- tibble::tibble(
  x1 = c(1, 2, 3, 4, 5),
  x2 = c(2, 4, 6, 8, 10)
)

test_y <- tibble::tibble(
  y1 = c(10, 20, 30, 40, 50)
)

test_that("opls function works as expected", {
  expect_error(opls(NULL, test_y), "X-data must be provided")
  expect_error(opls(test_x, NULL), "Y-data must be provided")
  expect_error(opls(list(1, 2, 3), test_y), "X-data must be of class data.frame, tbl_df, or tbl")
  expect_error(opls(test_x, list(1, 2, 3)), "Y-data must be of class data.frame, tbl_df, or tbl")
  }
  )
