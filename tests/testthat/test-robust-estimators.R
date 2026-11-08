set.seed(42)
z <- stats::rnorm(20000)

test_that("biweight location is robust and exact for symmetric data", {
  expect_equal(biweight_location(1:100), 50.5)
  x <- c(seq(1, 99), 1e3)
  expect_lt(abs(biweight_location(x) - 50), 1.5)
  expect_lt(abs(biweight_location(z)), 0.03)
  expect_equal(biweight_location(c(1, 2, NA, 4, 5)), NA_real_)
  expect_equal(biweight_location(c(1, 2, NA, 4, 5), drop_na = TRUE), 3)
  expect_error(biweight_location(c(1, 1, 1)), "constant")
  expect_error(biweight_location("a"), "numeric")
})

test_that("biweight scale and midvariance are consistent and robust", {
  expect_equal(biweight_scale(z), 1, tolerance = 0.03)
  expect_equal(biweight_scale(z)^2, biweight_midvariance(z))
  x <- c(seq(1, 99), 1e3)
  expect_lt(biweight_scale(x), 40)
  expect_gt(biweight_scale(x, reduced = TRUE), 0)
  expect_equal(biweight_midvariance(c(1, 2, NA)), NA_real_)
  expect_error(biweight_scale(c(2, 2)), "constant")
  expect_error(biweight_midvariance(1), "less than 2")
})

test_that("biweight midcovariance and midcorrelation behave as expected", {
  set.seed(1)
  x <- stats::rnorm(200)
  y <- 2 * x + stats::rnorm(200, sd = 0.5)
  expect_equal(biweight_midcovariance(x, x), biweight_midvariance(x))
  expect_equal(biweight_midcorrelation(x, 3 * x + 1), 1)
  expect_equal(biweight_midcorrelation(x, -x), -1)
  r <- biweight_midcorrelation(x, y)
  expect_true(r > 0.9 && r <= 1)
  # robust to a gross outlier, unlike Pearson
  y2 <- y
  y2[1] <- 1e4
  expect_gt(biweight_midcorrelation(x, y2), 0.9)
  expect_lt(abs(stats::cor(x, y2)), 0.5)
  expect_error(biweight_midcorrelation(1:3, 1:4), "same length")
  expect_error(biweight_midcovariance(1:3), "must be provided")
})

test_that("Rousseeuw-Croux estimators are consistent at the normal", {
  expect_equal(rousseeuw_croux(z, "Sn"), 1, tolerance = 0.02)
  expect_equal(rousseeuw_croux(z, "Qn"), 1, tolerance = 0.02)
  expect_equal(rousseeuw_croux(c(1, 2, NA, 5)), NA_real_)
  expect_type(rousseeuw_croux(c(1, 2, NA, 5, 9), drop_na = TRUE), "double")
  expect_error(rousseeuw_croux(c(3, 3, 3)), "constant")
})

test_that("umad is unbiased at the normal", {
  expect_equal(umad(z), 1, tolerance = 0.02)
  expect_equal(umad(z, method = "williams"), umad(z), tolerance = 1e-4)
  set.seed(2)
  small <- replicate(4000, umad(stats::rnorm(10)))
  expect_equal(mean(small), 1, tolerance = 0.02)
  expect_error(umad(1), "greater than 1")
  expect_error(umad(z, method = "x"), "hayes")
})

test_that("medcouple measures skewness robustly", {
  expect_equal(medcouple(z), 0, tolerance = 0.02)
  set.seed(4)
  e <- stats::rexp(20000)
  # 1/3 at the exponential distribution (Brys et al. 2004)
  expect_equal(medcouple(e), 1 / 3, tolerance = 0.02)
  expect_equal(medcouple(e), robustbase::mc(e, doScale = FALSE))
  expect_equal(medcouple(-e), -medcouple(e), tolerance = 1e-6)
  expect_equal(medcouple(3 * e + 2), medcouple(e), tolerance = 1e-6)
  expect_equal(medcouple(1:9), 0)
  # a few gross outliers barely change it, unlike the classical skewness
  x <- c(z[1:1000], rep(50, 20))
  expect_lt(abs(medcouple(x)), 0.05)
  expect_gt(moments::skewness(x), 5)
  expect_true(is.na(medcouple(c(1, 5, NA, 2))))
  expect_equal(medcouple(c(1, 5, NA, 2), drop_na = TRUE), medcouple(c(1, 5, 2)))
  expect_true(is.na(medcouple(numeric(0))))
  expect_true(is.na(medcouple(c(NA_real_, NA_real_), drop_na = TRUE)))
  expect_equal(medcouple(tapply(e[1:100], rep(1:50, 2), sum)),
               medcouple(as.vector(tapply(e[1:100], rep(1:50, 2), sum))))
  expect_error(medcouple("a"), "numeric")
  expect_error(medcouple(e, drop_na = "yes"), "drop_na")
  expect_error(medcouple(), "Missing")
})

test_that("medcouple takes a vector only", {
  set.seed(5)
  df <- data.frame(right = stats::rexp(200), left = -stats::rexp(200))
  expect_error(medcouple(df), "sapply\\(x, medcouple\\)")
  expect_error(medcouple(as.matrix(df)), "apply\\(x, 2, medcouple\\)")
  expect_error(medcouple(as.matrix(df[1])), "numeric vector")
  mc <- sapply(df, medcouple)
  expect_gt(mc[["right"]], 0.2)
  expect_lt(mc[["left"]], -0.2)
})

test_that("tail weights match their values at the normal distribution", {
  Q <- stats::qnorm
  lqw <- -(Q(0.4375) + Q(0.0625) - 2 * Q(0.25)) / (Q(0.4375) - Q(0.0625))
  qw <- quantile_weight(z)
  expect_equal(qw$LQW, lqw, tolerance = 0.05)
  expect_equal(qw$RQW, lqw, tolerance = 0.05)
  mw <- medcouple_weight(z)
  expect_equal(mw$LMC, 0.2, tolerance = 0.1)
  expect_equal(mw$RMC, 0.2, tolerance = 0.1)
  # heavier tails give larger weights
  set.seed(3)
  heavy <- stats::rt(20000, df = 2)
  expect_gt(quantile_weight(heavy)$RQW, qw$RQW)
  expect_gt(medcouple_weight(heavy)$RMC, mw$RMC)
  expect_true(is.na(quantile_weight(c(1, NA, 3))$LQW))
  expect_error(quantile_weight(z, p = 0.7), "'p'")
  expect_error(medcouple_weight("a"), "numeric")
})
