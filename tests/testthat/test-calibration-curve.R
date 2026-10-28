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

test_that("York regression reproduces the Pearson-York benchmark", {
  # Pearson (1901) data with the weights of York (1966); reference values of
  # the bfsl package (York et al., 2004)
  d <- data.frame(x = c(0.0, 0.9, 1.8, 2.6, 3.3, 4.4, 5.2, 6.1, 6.5, 7.4),
                  y = c(5.9, 5.4, 4.4, 4.6, 3.5, 3.7, 2.8, 2.8, 2.4, 1.5),
                  wx = c(1000, 1000, 500, 800, 200, 80, 60, 20, 1.8, 1.0),
                  wy = c(1, 1.8, 4, 8, 20, 20, 70, 70, 100, 500))
  d$sx <- 1 / sqrt(d$wx)
  d$sy <- 1 / sqrt(d$wy)
  fit <- york_fit(d$x, d$y, d$sx, d$sy)
  expect_equal(fit$coefficients, c(5.47991, -0.48053), tolerance = 1e-5)
  expect_equal(sqrt(diag(fit$vcov)), c(0.29497, 0.05799), tolerance = 1e-4)
  expect_equal(fit$chi_squared, 1.483, tolerance = 1e-3)
  # calibration_curve() scales the standard errors by sqrt(chi-squared) > 1
  cal <- calibration_curve(d, y, x, regression = "york", concentration_se = sx, signal_se = sy)
  expect_equal(cal$coefficients$estimate, fit$coefficients)
  expect_equal(cal$coefficients$std_error, sqrt(diag(fit$vcov) * fit$chi_squared))
  expect_equal(cal$eiv$chi_squared, fit$chi_squared)
  expect_output(print(cal), "York regression")
  # exact concentrations: weighted least squares
  d$sx0 <- 0
  wls <- calibration_curve(d, y, x, regression = "york", concentration_se = sx0, signal_se = sy)
  expect_equal(wls$coefficients$estimate,
               unname(stats::coef(stats::lm(y ~ x, data = d, weights = wy))), tolerance = 1e-8)
})

test_that("Deming regression matches its closed form", {
  set.seed(2)
  x_true <- seq(1, 10, length.out = 15)
  d <- data.frame(x = x_true + stats::rnorm(15, sd = 0.3),
                  y = 2 + 0.5 * x_true + stats::rnorm(15, sd = 0.2))
  delta <- (0.2 / 0.3)^2
  cal <- calibration_curve(d, y, x, regression = "deming", variance_ratio = delta)
  sxx <- stats::var(d$x)
  syy <- stats::var(d$y)
  sxy <- stats::cov(d$x, d$y)
  b <- (syy - delta * sxx + sqrt((syy - delta * sxx)^2 + 4 * delta * sxy^2)) / (2 * sxy)
  expect_equal(cal$coefficients$estimate, c(mean(d$y) - b * mean(d$x), b), tolerance = 1e-8)
  expect_true(all(cal$coefficients$std_error > 0))
  # exact concentrations (a huge ratio): least squares
  ls <- calibration_curve(d, y, x, regression = "deming", variance_ratio = 1e12)
  expect_equal(ls$coefficients$estimate, unname(stats::coef(stats::lm(y ~ x, d))), tolerance = 1e-6)
  # the ratio from the uncertainties
  d$sx <- 0.3
  d$sy <- 0.2
  from_se <- calibration_curve(d, y, x, regression = "deming", concentration_se = sx, signal_se = sy)
  expect_equal(from_se$coefficients$estimate, cal$coefficients$estimate)
  expect_equal(from_se$eiv$variance_ratio, delta)
  expect_output(print(cal), "Deming regression")
  # inverse prediction and bands use the Deming line
  p <- predict(cal, c(3, 5))
  expect_equal(p$concentration, (c(3, 5) - cal$coefficients$estimate[1]) / b)
  s <- predict(cal, 4, type = "signal", interval = "confidence")
  v <- cal$eiv$vcov
  expect_equal(s$se, sqrt(v[1, 1] + 16 * v[2, 2] + 8 * v[1, 2]))
  expect_s3_class(plot_calibration(cal), "ggplot")
})

test_that("errors-in-variables fits validate their inputs", {
  d <- cal_data()
  d$se <- 0.01
  expect_error(calibration_curve(d, intensity, concentration, regression = "york"),
               "concentration_se")
  expect_error(calibration_curve(d, intensity, concentration, regression = "deming"),
               "variance_ratio")
  expect_error(calibration_curve(d, intensity, concentration, regression = "york", model = "quadratic",
                                 concentration_se = se, signal_se = se), "straight lines")
  expect_error(calibration_curve(d, intensity, concentration, regression = "deming",
                                 variance_ratio = 1, weights = rep(1, 18)), "weights")
  d$bad <- -1
  expect_error(calibration_curve(d, intensity, concentration, regression = "york",
                                 concentration_se = bad, signal_se = se), "non-negative")
  d$zero <- 0
  expect_error(calibration_curve(d, intensity, concentration, regression = "york",
                                 concentration_se = zero, signal_se = zero), "positive uncertainty")
})

test_that("ISO 11843 limits follow the noncentral t distribution", {
  d <- cal_data()
  cal <- calibration_curve(d, intensity, concentration, lod_convention = "iso11843")
  fit <- stats::lm(intensity ~ concentration, d)
  s <- summary(fit)$sigma
  sa <- sqrt(stats::vcov(fit)[1, 1])
  b <- stats::coef(fit)[[2]]
  sd_net <- sqrt(s^2 + sa^2)
  t_c <- stats::qt(0.95, 16)
  fom <- cal$figures_of_merit
  expect_equal(fom$critical_value, t_c * sd_net / b)
  delta <- fom$lod * b / sd_net
  expect_equal(stats::pt(t_c, 16, ncp = delta), 0.05, tolerance = 1e-6)
  expect_equal(delta, 2 * t_c, tolerance = 0.05)   # about twice t for alpha = beta
  expect_equal(fom$loq, 10 * sd_net / b)
  # the ICH limits have no critical value
  expect_true(is.na(calibration_curve(d, intensity, concentration)$figures_of_merit$critical_value))
  # replicates of the sample lower the limits
  four <- calibration_curve(d, intensity, concentration, lod_convention = "iso11843", replicates = 4)
  expect_equal(four$figures_of_merit$critical_value, t_c * sqrt(s^2 / 4 + sa^2) / b)
  # blanks give the standard deviation and its degrees of freedom
  blank <- c(48, 52, 47, 55, 50, 49)
  with_blank <- calibration_curve(d, intensity, concentration, blank = blank,
                                  lod_convention = "iso11843")
  expect_equal(with_blank$figures_of_merit$critical_value,
               stats::qt(0.95, 5) * sqrt(stats::sd(blank)^2 + sa^2) / b)
  # predictions above the critical value are detected
  p <- predict(cal, stats::coef(fit)[[1]] + b * c(0.5, 2) * fom$critical_value)
  expect_equal(p$detected, c(FALSE, TRUE))
  expect_output(print(cal), "ISO 11843")
  expect_s3_class(plot_calibration(cal), "ggplot")
  expect_error(calibration_curve(d, intensity, concentration, lod_convention = "iso11843",
                                 lod_method = "intercept"), "residual")
  positive <- d[d$concentration > 0, ]
  expect_error(calibration_curve(positive, intensity, concentration, weights = "1/x",
                                 lod_convention = "iso11843"), "blank")
})

test_that("predict flags extrapolations", {
  cal <- calibration_curve(cal_data(), intensity, concentration)
  p <- predict(cal, c(2000, 1e5, -500))
  expect_equal(p$outside_range, c(FALSE, TRUE, TRUE))
  expect_false("detected" %in% names(p))
})
