test_that("average computes column means and ignores NA", {
  df <- data.frame(a = c(1, 2, 3, NA), b = c(4, 5, 6, 7))
  res <- average(df)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$a, 2)
  expect_equal(res$b, 5.5)
  expect_true(is.na(average(data.frame(a = c(NA_real_, NA_real_)))$a))
})

test_that("average computes group means with group labels", {
  df <- data.frame(
    grp = c("b", "a", "b", "a", NA),
    x = c(1, 10, 3, 20, 100),
    y = c(2, NA, 4, 40, 100)
  )
  res <- average(df, grp)
  expect_equal(res$grp, c("b", "a"))
  expect_equal(res$x, c(2, 15))
  expect_equal(res$y, c(3, 40))
  expect_equal(average(df, "grp"), res)
})

test_that("average handles factors with unused levels", {
  df <- data.frame(g = factor(c("x", "x", "y"), levels = c("x", "y", "z")), v = c(1, 3, 5))
  res <- average(df, g)
  expect_equal(res$g, c("x", "y", "z"))
  expect_equal(res$v, c(2, 5, NA))
})

test_that("average matches base R on a large dataset", {
  set.seed(1)
  m <- matrix(stats::rnorm(500 * 300), 500)
  df <- as.data.frame(m)
  expect_equal(unlist(average(df)), colMeans(m), ignore_attr = TRUE)
  df$g <- rep(1:5, 100)
  ref <- stats::aggregate(. ~ g, data = df, FUN = mean)
  res <- average(df, g)
  expect_equal(as.matrix(res[-1]), as.matrix(ref[-1]), ignore_attr = TRUE)
})

test_that("average validates its inputs", {
  expect_error(average(), "Missing 'x' argument.")
  expect_error(average(matrix(1:4, 2)), "'x' must be a data frame or tibble.")
  expect_error(average(data.frame(a = 1), zz), "not found")
  expect_error(average(data.frame(a = "x")), "numeric")
})
