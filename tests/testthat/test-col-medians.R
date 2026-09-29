test_that("col_medians matches stats::median column by column", {
  set.seed(12)
  odd <- matrix(stats::rnorm(7 * 30), 7, dimnames = list(NULL, paste0("v", 1:30)))
  even <- matrix(stats::rnorm(8 * 30), 8)
  expect_equal(col_medians(odd), apply(odd, 2, stats::median))
  expect_equal(unname(col_medians(even)), apply(even, 2, stats::median))
  ties <- matrix(c(1, 1, 2, 2, 3, 3, 3, 3), 4)
  expect_equal(col_medians(ties), c(1.5, 3))
  expect_equal(col_medians(matrix(5, 1, 3)), c(5, 5, 5))
  withna <- odd
  withna[2, 3] <- NA
  withna[, 5] <- NA
  expect_equal(col_medians(withna), apply(withna, 2, stats::median))
  expect_equal(col_medians(withna, na.rm = TRUE),
               suppressWarnings(apply(withna, 2, stats::median, na.rm = TRUE)))
  expect_equal(unname(col_medians(as.data.frame(odd))), unname(col_medians(odd)))
  int <- matrix(1:12, 4)
  expect_equal(col_medians(int), apply(int, 2, stats::median))
})
