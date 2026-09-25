set.seed(7)
x <- c(stats::rnorm(100), 15, -12)

test_that("zscore matches base scale() and flags outliers", {
  res <- zscore(x)
  expect_named(res, c("data", "score", "flag"))
  expect_equal(res$score[match(x, res$data)], as.numeric(scale(x)))
  expect_true(all(res$flag[res$data %in% c(15, -12)]))
  rob <- zscore(x, robust = TRUE)
  expect_equal(rob$score[rob$data == 15], (15 - stats::median(x)) / stats::mad(x))
  expect_equal(sum(is.na(zscore(c(x, NA))$score)), 1)
  expect_error(zscore("a"), "numeric")
  expect_error(zscore(x, cutoff = -1), "cutoff")
})

test_that("iqrMethod flags values outside the fences", {
  res <- iqrMethod(x)
  expect_true(all(res$flag[res$data %in% c(15, -12)]))
  q <- stats::quantile(x, c(0.25, 0.75))
  fence <- q[2] + 1.5 * diff(q)
  expect_equal(sum(res$flag & res$data > 0), sum(x > fence))
  expect_warning(iqrMethod(x, k = 3, skew = TRUE), "only defined for k = 1.5")
  set.seed(1)
  skewed <- stats::rexp(200)
  expect_lte(sum(iqrMethod(skewed, skew = TRUE)$flag), sum(iqrMethod(skewed)$flag))
  expect_equal(nrow(iqrMethod(c(x, NA))), length(x) + 1)
  expect_error(iqrMethod(x, k = 0), "positive")
})

test_that("directOutlyingness flags gross outliers", {
  y <- c(1, 5, 3, 9, 2, 6, 4, 8, 7, 1e3)
  res <- directOutlyingness(y)
  expect_named(res, c("data", "score", "flag"))
  expect_true(res$flag[res$data == 1e3])
  expect_equal(sum(res$flag), 1)
  expect_no_error(res2 <- directOutlyingness(y, maxRatio = 3))
  expect_true(res2$flag[res2$data == 1e3])
  expect_error(directOutlyingness(y, maxRatio = 1), "at least 2")
  expect_error(directOutlyingness("a"), "numeric")
})

test_that("generalized_boxplot estimates g and h and sensible fences", {
  set.seed(3)
  df <- data.frame(normal = stats::rnorm(5000), skewed = stats::rexp(5000))
  res <- generalized_boxplot(df, plot = FALSE)
  st <- res$stats
  expect_lt(abs(st$g[1]), 0.05)
  expect_lt(st$h[1], 0.1)
  expect_gt(st$g[2], 0.05)
  # For normal data, the fences are close to the alpha/2 quantiles.
  expect_equal(st$lower[1], stats::qnorm(0.025), tolerance = 0.1)
  expect_equal(st$upper[1], stats::qnorm(0.975), tolerance = 0.1)
  expect_true(all(res$outliers$out %in% c("lower", "upper")))
  expect_s3_class(generalized_boxplot(df[1:200, ]), "ggplot")
})

test_that("generalized_boxplot handles unequal numbers of outliers per tail", {
  set.seed(4)
  df <- data.frame(a = c(stats::rnorm(100), 20, 25, 30))
  res <- generalized_boxplot(df, plot = FALSE)
  expect_true(all(c(20, 25, 30) %in% res$outliers$value))
  expect_error(generalized_boxplot(df, alpha = 2), "alpha")
  expect_error(generalized_boxplot(df, p = 0.2), "'p'")
})

test_that("outlierplot returns plots and data without touching the RNG", {
  set.seed(5)
  m <- matrix(stats::rnorm(200), 50, 4, dimnames = list(NULL, paste0("v", 1:4)))
  m[1, ] <- 10
  set.seed(99)
  before <- .Random.seed
  p <- outlierplot(m)
  expect_identical(.Random.seed, before)
  expect_s3_class(p, "ggplot")
  expect_s3_class(outlierplot(m, show.outlier = FALSE, show.mahal = TRUE), "ggplot")
  expect_s3_class(outlierplot(m, show.mahal = TRUE), "ggplot")
  res <- outlierplot(m, show.outlier = FALSE)
  expect_named(res, c(paste0("v", 1:4), "outlier", "mahalanobis"))
  expect_true(res$outlier[1])
  expect_error(outlierplot(m[, 1, drop = FALSE]), "two-dimensional")
  expect_error(outlierplot(m, quan = 0.2), "quan")
})
