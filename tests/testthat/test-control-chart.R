test_that("control_chart estimates the center and sigma from the baseline", {
  x <- c(10, 12, 11, 13, 12, 11, 30)
  chart <- control_chart(x, baseline = 6)
  expect_s3_class(chart, "specproc_control_chart")
  expect_equal(attr(chart, "center"), mean(x[1:6]))
  expect_equal(attr(chart, "sigma"), mean(abs(diff(x[1:6]))) / 1.128)
  expect_equal(chart$signal, c(rep(FALSE, 6), TRUE))
  expect_equal(chart$rule[7], 1L)
  expect_equal(chart$upper, rep(attr(chart, "center") + 3 * attr(chart, "sigma"), 7))
  expect_output(print(chart), "first at 7")
  expect_s3_class(plot_control_chart(chart), "ggplot")
  # sorted by time; a logical baseline in the order of `x`
  shuffled <- control_chart(rev(x), order = 7:1, baseline = rev(c(rep(TRUE, 6), FALSE)))
  expect_equal(shuffled$value, x)
  expect_equal(attr(shuffled, "center"), mean(x[1:6]))
  # known center and sigma
  known <- control_chart(x, center = 11, sigma = 2)
  expect_equal(known$lower, rep(5, 7))
})

test_that("control_chart applies the Western Electric rules", {
  rule <- function(x) control_chart(x, center = 0, sigma = 1)$rule
  expect_equal(rule(c(0, 3.5, 0)), c(NA, 1L, NA))
  expect_equal(rule(c(0, 2.5, 0, 2.5)), c(NA, NA, NA, 2L))
  expect_equal(rule(c(0, 1.5, 1.5, 1.5, 1.5)), c(NA, NA, NA, NA, 3L))
  expect_equal(rule(c(rep(0.5, 8), -0.5)), c(rep(NA, 7), 4L, NA))
  expect_true(all(is.na(control_chart(c(0, 2.5, 0, 2.5), center = 0, sigma = 1,
                                      rules = FALSE)$rule)))
})

test_that("the EWMA and CUSUM charts follow their recursions", {
  x <- c(rep(0, 5), rep(1, 20))
  ewma <- control_chart(x, method = "ewma", center = 0, sigma = 1, lambda = 0.2)
  z <- numeric(25)
  prev <- 0
  for (i in 1:25) {
    z[i] <- 0.2 * x[i] + 0.8 * prev
    prev <- z[i]
  }
  expect_equal(ewma$statistic, z)
  expect_equal(ewma$upper, 3 * sqrt(0.2 / 1.8 * (1 - 0.8^(2 * (1:25)))))
  expect_equal(which(ewma$signal)[1], which(z > ewma$upper)[1])
  cusum <- control_chart(x, method = "cusum", center = 0, sigma = 1)
  # the upper sum grows by 1 - 0.5 per step from the 6th value
  expect_equal(cusum$statistic, c(rep(0, 5), 0.5 * (1:20)))
  expect_equal(cusum$lower_sum, rep(0, 25))
  expect_equal(which(cusum$signal)[1], 16)
  expect_s3_class(plot_control_chart(cusum), "ggplot")
  expect_s3_class(plot_control_chart(ewma), "ggplot")
})

test_that("control_chart validates its inputs", {
  expect_error(control_chart(1), "at least 2")
  expect_error(control_chart(c(1, NA, 3)), "missing")
  expect_error(control_chart(1:5, order = 1:3), "order")
  expect_error(control_chart(1:5, baseline = 1), "baseline")
  expect_error(control_chart(1:5, baseline = c(TRUE, FALSE)), "logical")
  expect_error(control_chart(1:5, baseline = c(1, 9)), "between 1 and 5")
  expect_error(control_chart(rep(1, 5)), "sigma")
  expect_error(plot_control_chart(list()), "control_chart")
})
