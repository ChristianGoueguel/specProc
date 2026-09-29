cal_data <- function(curvature = 0, sd = 20, seed = 1) {
  set.seed(seed)
  d <- data.frame(concentration = rep(c(0, 0.5, 1, 2, 4, 8), each = 3))
  d$intensity <- with(d, 50 + 1000 * concentration - curvature * concentration^2 +
                        stats::rnorm(18, sd = sd))
  d
}

test_that("calibration_curve fits a line with its figures of merit", {
  d <- cal_data()
  cal <- calibration_curve(d, intensity, concentration)
  expect_s3_class(cal, "specproc_calibration")
  fit <- stats::lm(intensity ~ concentration, data = d)
  expect_equal(cal$coefficients$estimate, unname(stats::coef(fit)))
  fom <- cal$figures_of_merit
  expect_equal(fom$sensitivity, unname(stats::coef(fit)[2]))
  expect_equal(fom$lod, 3.3 * summary(fit)$sigma / fom$sensitivity)
  expect_equal(fom$loq, 10 * summary(fit)$sigma / fom$sensitivity)
  expect_equal(fom$n, 18)
  # a straight line: no significant curvature
  expect_equal(cal$linearity$test, c("Mandel", "Lack of fit"))
  expect_gt(cal$linearity$p_value[1], 0.01)
  expect_output(print(cal), "Linear calibration curve")
  expect_s3_class(plot_calibration(cal), "ggplot")
  # the LOD from the intercept and from a blank
  expect_equal(calibration_curve(d, "intensity", "concentration", lod_method = "intercept")$figures_of_merit$lod,
               3.3 * summary(fit)$coefficients[1, 2] / fom$sensitivity)
  blank <- c(48, 55, 51, 60)
  b <- calibration_curve(d, intensity, concentration, blank = blank)
  expect_equal(b$lod_method, "blank")
  expect_equal(b$figures_of_merit$lod, 3.3 * stats::sd(blank) / fom$sensitivity)
})

test_that("Mandel's test detects curvature, and a quadratic curve fits it", {
  d <- cal_data(curvature = 15)
  cal <- calibration_curve(d, intensity, concentration)
  expect_lt(cal$linearity$p_value[cal$linearity$test == "Mandel"], 1e-6)
  quad <- calibration_curve(d, intensity, concentration, model = "quadratic")
  expect_equal(nrow(quad$coefficients), 3)
  expect_false("Mandel" %in% quad$linearity$test)
  expect_gt(quad$linearity$p_value, 0.01)
})

test_that("predict inverts the curve with confidence intervals", {
  d <- cal_data()
  cal <- calibration_curve(d, intensity, concentration)
  p <- predict(cal, c(1500, 20))
  b <- stats::coef(cal$fit)
  expect_equal(p$concentration, unname((c(1500, 20) - b[1]) / b[2]))
  # standard error of inverse prediction (Miller and Miller)
  s <- summary(cal$fit)$sigma
  x <- d$concentration
  se <- s / b[[2]] * sqrt(1 + 1 / 18 + (1500 - mean(d$intensity))^2 / (b[[2]]^2 * sum((x - mean(x))^2)))
  expect_equal(p$se[1], se, tolerance = 1e-4)
  expect_equal(p$upper - p$concentration, stats::qt(0.975, 16) * p$se)
  expect_equal(p$below_lod, c(FALSE, TRUE))
  expect_lt(predict(cal, 1500, replicates = 4)$se, p$se[1])
  expect_equal(predict(cal, data.frame(intensity = 1500))$concentration, p$concentration[1])
  # quadratic: the root in the calibration range
  quad <- calibration_curve(cal_data(curvature = 15), intensity, concentration, model = "quadratic")
  bq <- stats::coef(quad$fit)
  pq <- predict(quad, 5000)
  expect_equal(bq[[1]] + bq[[2]] * pq$concentration + bq[[3]] * pq$concentration^2, 5000)
  expect_true(pq$concentration > 0 && pq$concentration < 8)
  expect_true(is.finite(pq$se))
})

test_that("calibration_curve handles weights and checks its inputs", {
  d <- cal_data()
  pos <- d[d$concentration > 0, ]
  w <- calibration_curve(pos, intensity, concentration, weights = "1/x")
  ref <- stats::lm(intensity ~ concentration, data = pos, weights = 1 / pos$concentration)
  expect_equal(w$coefficients$estimate, unname(stats::coef(ref)))
  expect_output(print(w), "weighted")
  expect_true(is.finite(predict(w, 3000)$se))
  expect_error(calibration_curve(d, intensity, concentration, weights = "1/x2"), "positive")
  expect_error(calibration_curve(d, intensity, concentration, weights = 1:3), "one per row")
  expect_error(calibration_curve(as.matrix(d), intensity, concentration), "data frame")
  expect_error(calibration_curve(d, foo, concentration), "not found")
  expect_error(calibration_curve(d[d$concentration < 1, ], intensity, concentration, model = "quadratic"),
               "distinct concentrations")
  expect_error(calibration_curve(d, intensity, concentration, lod_method = "blank"), "blank")
  expect_error(predict(calibration_curve(d, intensity, concentration), "a"), "numeric")
  expect_error(plot_calibration(1), "calibration_curve")
})

test_that("confidence and prediction intervals of the signal match lm", {
  d <- cal_data(sd = 200)
  cal <- calibration_curve(d, intensity, concentration)
  expect_equal(c(cal$coefficients$lower, cal$coefficients$upper),
               as.vector(stats::confint(cal$fit)))
  new <- data.frame(concentration = c(0.7, 5))
  for (type in c("prediction", "confidence")) {
    ours <- predict(cal, c(0.7, 5), type = "signal", interval = type)
    ref <- stats::predict(stats::lm(intensity ~ concentration, data = d), new, interval = type)
    expect_equal(ours$signal, unname(ref[, "fit"]))
    expect_equal(ours$lower, unname(ref[, "lwr"]))
    expect_equal(ours$upper, unname(ref[, "upr"]))
  }
  # a mean of replicates narrows the prediction interval, not the confidence interval
  expect_lt(predict(cal, 5, type = "signal", replicates = 4)$se, predict(cal, 5, type = "signal")$se)
  expect_equal(predict(cal, 5, type = "signal", interval = "confidence", replicates = 4)$se,
               predict(cal, 5, type = "signal", interval = "confidence")$se)
  expect_equal(predict(cal, data.frame(concentration = 5), type = "signal")$concentration, 5)
  # weighted: the new signal follows the weight at its concentration
  pos <- d[d$concentration > 0, ]
  w <- calibration_curve(pos, intensity, concentration, weights = "1/x")
  ref <- stats::predict(w$fit, new, interval = "prediction", weights = 1 / new$concentration)
  expect_equal(predict(w, new$concentration, type = "signal")$lower, unname(ref[, "lwr"]))
  expect_error(predict(cal, "a", type = "signal"), "numeric concentrations")
})

test_that("plot_calibration draws the bands and the new samples", {
  d <- cal_data(sd = 200)
  cal <- calibration_curve(d, intensity, concentration)
  ribbons <- function(p) sum(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)))
  expect_equal(ribbons(plot_calibration(cal)), 2)
  expect_equal(ribbons(plot_calibration(cal, interval = "confidence")), 1)
  expect_equal(ribbons(plot_calibration(cal, interval = "none")), 0)
  p <- plot_calibration(cal, newdata = c(1500, 5000), level = 0.9)
  bars <- p$layers[vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1))][[1]]$data
  expect_equal(bars$lower, predict(cal, c(1500, 5000), level = 0.9)$lower)
  expect_match(p$labels$subtitle, "90%")
  expect_error(plot_calibration(cal, interval = "foo"))
})
