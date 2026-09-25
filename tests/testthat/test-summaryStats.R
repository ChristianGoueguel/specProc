test_data <- tibble::tibble(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 6, 8, 10),
  z = c(3, 6, NA, 12, 15),
  label = letters[1:5]
)

test_that("summaryStats computes the classical statistics", {
  res <- summaryStats(test_data)
  expect_equal(res$variable, c("x", "y", "z"))
  expect_equal(res$mean, c(3, 6, 9))
  expect_equal(res$median, c(3, 6, 9))
  expect_equal(res$sd, round(c(stats::sd(1:5), stats::sd(seq(2, 10, 2)), stats::sd(c(3, 6, 12, 15))), 2))
  expect_equal(res$count, c(5, 5, 4))
  expect_equal(res$range, c(4, 8, 12))
  res_na <- summaryStats(test_data, drop.na = FALSE)
  expect_true(is.na(res_na$mean[3]))
  expect_equal(res_na$count[3], 5)
})

test_that("summaryStats computes the robust statistics", {
  set.seed(1)
  d <- data.frame(a = stats::rnorm(200), b = stats::rexp(200))
  res <- summaryStats(d, robust = TRUE, digits = 6)
  expect_equal(res$mad, round(c(stats::mad(d$a), stats::mad(d$b)), 6))
  expect_equal(res$Qn[1], round(rousseeuwCroux(d$a, "Qn"), 6))
  expect_equal(res$biloc[2], round(biweight_location(d$b), 6))
  expect_equal(res$rcv, round(res$mad / res$median * 100, 6), tolerance = 1e-4)
  expect_gt(res$medcouple[2], 0)
})

test_that("summaryStats does not fail on a constant variable", {
  d <- data.frame(a = c(1, 2, 3, 4, 10), k = rep(5, 5))
  res <- summaryStats(d, robust = TRUE)
  expect_true(is.na(res$biscale[2]))
  expect_false(is.na(res$biscale[1]))
})

test_that("summaryStats selects variables by name or position", {
  expect_equal(summaryStats(test_data, var = c("x", "z"))$variable, c("x", "z"))
  expect_equal(summaryStats(test_data, var = 2)$variable, "y")
})

test_that("summaryStats validates its inputs", {
  expect_error(summaryStats(NULL), "Data must be provided")
  expect_error(summaryStats(list(1, 2, 3)), "Data must be of class data.frame, tbl_df, or tbl")
  expect_error(summaryStats(test_data, var = list(1, 2, 3)), "'var' must be either a character vector or a numeric vector")
  expect_error(summaryStats(test_data, var = c("x", "w")), "One or more variables specified in 'var' are not present in the data")
  expect_error(summaryStats(test_data, var = "label"), "must be numeric")
  expect_error(summaryStats(test_data, digits = -1), "'digits' must be a non-negative integer")
  expect_error(summaryStats(test_data, digits = 1.5), "'digits' must be a non-negative integer")
})
