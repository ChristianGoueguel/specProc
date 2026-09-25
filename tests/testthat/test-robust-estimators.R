set.seed(42)
z <- stats::rnorm(20000)

test_that("biweight location is robust and exact for symmetric data", {
  expect_equal(biweight_location(1:100), 50.5)
  x <- c(seq(1, 99), 1e3)
  expect_lt(abs(biweight_location(x) - 50), 1.5)
  expect_lt(abs(biweight_location(z)), 0.03)
  expect_equal(biweight_location(c(1, 2, NA, 4, 5)), NA_real_)
  expect_equal(biweight_location(c(1, 2, NA, 4, 5), drop.na = TRUE), 3)
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
  expect_equal(rousseeuwCroux(z, "Sn"), 1, tolerance = 0.02)
  expect_equal(rousseeuwCroux(z, "Qn"), 1, tolerance = 0.02)
  expect_equal(rousseeuwCroux(c(1, 2, NA, 5)), NA_real_)
  expect_type(rousseeuwCroux(c(1, 2, NA, 5, 9), drop.na = TRUE), "double")
  expect_error(rousseeuwCroux(c(3, 3, 3)), "constant")
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
