kb_tidy <- 8.617333262e-5

tidy_lines <- function(te = 9000, noise = 0.05) {
  set.seed(2)
  lines <- data.frame(species = "Fe I", wavelength = c(400, 410, 420, 430, 440),
                      Aki = c(1e8, 5e7, 2e7, 1e7, 5e6), gk = c(3, 5, 7, 5, 3),
                      Ek = c(3, 3.5, 4.2, 5, 5.6))
  lines$intensity <- with(lines, gk * Aki / wavelength * exp(-Ek / (kb_tidy * te))) *
    exp(stats::rnorm(5, sd = noise))
  lines
}

test_that("tidy and glance are re-exported from generics", {
  expect_identical(specProc::tidy, generics::tidy)
  expect_identical(specProc::glance, generics::glance)
})

test_that("tidy and glance summarize a Boltzmann plot", {
  fit <- boltzmann(tidy_lines())
  td <- tidy(fit)
  expect_named(td, c("term", "estimate", "std.error"))
  expect_equal(td$term, c("temperature", "intercept"))
  expect_equal(td$estimate, c(fit$temperature, unname(stats::coef(fit$fit)[1])))
  expect_equal(td$std.error[1], fit$temperature_se)
  ci <- tidy(fit, conf.int = TRUE, conf.level = 0.9)
  # the interval of the temperature is that of the slope, transformed
  slope_ci <- stats::confint(fit$fit, "x", level = 0.9)
  expect_equal(c(ci$conf.low[1], ci$conf.high[1]), sort(-1 / (kb_tidy * slope_ci[1, ])),
               ignore_attr = TRUE)
  expect_equal(c(ci$conf.low[2], ci$conf.high[2]), stats::confint(fit$fit, 1, level = 0.9)[1, ],
               ignore_attr = TRUE)
  gl <- glance(fit)
  expect_equal(nrow(gl), 1)
  expect_equal(gl$r.squared, fit$r_squared)
  expect_equal(gl$sigma, summary(fit$fit)$sigma)
  expect_equal(gl$nobs, 5)
  expect_equal(gl$df.residual, 3)
  expect_equal(gl$energy.range, 2.6)
  expect_true(is.na(gl$electron.density))
  expect_equal(gl$method, "Boltzmann")
  expect_error(tidy(fit, conf.int = "yes"), "conf.int")
  expect_error(tidy(fit, conf.int = TRUE, conf.level = 2), "conf.level")
  # an interval of the slope that reaches zero has no upper temperature
  expect_equal(temperature_interval(-0.1, 1e5, 1 / (kb_tidy * 0.1), 0.95, 3)[2], Inf)
  expect_true(all(is.na(temperature_interval(-0.1, NA, 1e4, 0.95, 3))))
})

test_that("glance reports the electron density of a Saha-Boltzmann plot", {
  data(forageLIBS, package = "specProc", envir = environment())
  atomic <- data.frame(
    species = c(rep("Ca I", 4), rep("Ca II", 2)), stage = c(rep(1, 4), rep(2, 2)),
    wavelength = c(428.301, 430.253, 612.222, 616.217, 315.887, 317.933),
    Aki = c(4.34e7, 1.36e8, 2.87e7, 4.77e7, 3.10e8, 3.60e8), gk = c(5, 5, 3, 3, 4, 6),
    Ek = c(4.780, 4.780, 3.910, 3.910, 7.047, 7.050)
  )
  lines <- line_intensities(colMeans(forageLIBS[-(1:14)]), atomic, baseline = TRUE)
  fit <- saha_boltzmann(lines, ionization_energy = 6.113, electron_density = 1.9e17)
  gl <- glance(fit)
  expect_equal(gl$electron.density, 1.9e17)
  expect_equal(gl$iterations, fit$iterations)
  expect_equal(gl$method, "Saha-Boltzmann")
  expect_equal(tidy(fit)$estimate[1], fit$temperature)
})

test_that("tidy and glance summarize CF-LIBS and its uncertainty", {
  partitions <- c("Fe I" = 30, "Fe II" = 40, "Ca I" = 3, "Ca II" = 2)
  u <- function(species, temperature) partitions[species]
  n <- c("Fe I" = 0.14, "Fe II" = 0.56, "Ca I" = 0.015, "Ca II" = 0.285)
  lines <- data.frame(
    species = c("Fe I", "Fe I", "Fe I", "Fe II", "Fe II", "Ca I", "Ca I", "Ca II"),
    wavelength = c(371, 404, 438, 259, 275, 422, 445, 393),
    Aki = c(1.6e7, 8.6e7, 5.0e7, 2.2e8, 2.1e8, 2.2e8, 8.7e7, 1.5e8),
    gk = c(11, 9, 11, 10, 8, 3, 7, 4), Ek = c(3.33, 4.55, 4.31, 4.77, 5.55, 2.93, 4.68, 3.15)
  )
  set.seed(3)
  lines$intensity <- with(lines, n[species] / partitions[species] * gk * Aki / wavelength *
                            exp(-Ek / (kb_tidy * 9000))) * exp(stats::rnorm(8, sd = 0.02))
  fit <- cf_libs(lines, partition = u)
  td <- tidy(fit, conf.int = TRUE)
  expect_equal(td$term, c("temperature", "atomic_fraction", "atomic_fraction", "mass_fraction",
                          "mass_fraction"))
  expect_equal(td$element, c(NA, "Fe", "Ca", "Fe", "Ca"))
  expect_equal(td$estimate[-1], c(fit$composition$atomic_fraction, fit$composition$mass_fraction))
  expect_true(all(is.na(td$std.error[-1])))
  expect_lt(td$conf.low[1], fit$temperature)
  expect_gt(td$conf.high[1], fit$temperature)
  gl <- glance(fit)
  y <- fit$points$y
  expect_equal(gl$r.squared, 1 - sum(stats::residuals(fit$fit)^2) / sum((y - mean(y))^2))
  expect_equal(gl$n.species, 4)
  expect_equal(gl$n.elements, 2)
  expect_equal(gl$nobs, 8)
  # a given temperature: no fit
  given <- glance(cf_libs(lines, partition = u, temperature = 9000))
  expect_true(is.na(given$r.squared))
  expect_true(is.na(given$df.residual))
  expect_true(all(is.na(tidy(cf_libs(lines, partition = u, temperature = 9000), conf.int = TRUE)$conf.low)))

  set.seed(4)
  unc <- plasma_uncertainty(fit, n = 50, aki_rsd = 0.1)
  tu <- tidy(unc)
  expect_named(tu, c("term", "element", "estimate", "std.error", "conf.low", "conf.high"))
  expect_equal(tu$std.error, unc$estimates$se)
  expect_equal(glance(unc)$n.draws, 50)
  expect_equal(glance(unc)$level, 0.95)
  # a Boltzmann plot has no element column
  set.seed(5)
  ub <- plasma_uncertainty(boltzmann(tidy_lines()), n = 50, aki_rsd = 0.1)
  expect_named(tidy(ub), c("term", "estimate", "std.error", "conf.low", "conf.high"))
})

test_that("tidy and glance summarize a calibration curve", {
  set.seed(6)
  d <- data.frame(conc = rep(0:5, each = 2))
  d$signal <- 2 + 0.5 * d$conc + stats::rnorm(12, sd = 0.1)
  cal <- calibration_curve(d, signal, conc)
  ref <- stats::lm(signal ~ conc, data = d)
  td <- tidy(cal, conf.int = TRUE)
  expect_named(td, c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"))
  expect_equal(td$term, c("intercept", "slope"))
  expect_equal(td$estimate, unname(stats::coef(ref)))
  expect_equal(td$p.value, unname(summary(ref)$coefficients[, 4]))
  expect_equal(cbind(td$conf.low, td$conf.high), unname(stats::confint(ref)))
  gl <- glance(cal)
  expect_equal(gl$nobs, 12)
  expect_equal(gl$r.squared, summary(ref)$r.squared)
  expect_equal(gl$lod, cal$figures_of_merit$lod)
  expect_equal(gl$regression, "ols")
  expect_true(is.na(gl$chi.squared))
  expect_false(is.na(gl$mandel.p.value))
  d$sx <- 0.02
  d$sy <- 0.1
  york <- glance(calibration_curve(d, signal, conc, regression = "york", concentration_se = sx,
                                   signal_se = sy))
  expect_false(is.na(york$chi.squared))
  expect_equal(york$regression, "york")
})

test_that("tidy and glance summarize a wavelength calibration and a slope/bias correction", {
  wl <- seq(300, 500, by = 0.08)
  true <- c(320, 350, 380, 410, 440, 470)
  y <- 100 + rowSums(vapply(true, function(t) 5000 * exp(-(wl - (t - 0.05))^2 / (2 * 0.04^2)),
                            numeric(length(wl))))
  set.seed(7)
  x <- rbind(y, y) + matrix(stats::rnorm(2 * length(wl), sd = 2), 2)
  colnames(x) <- wl
  cal <- wavelength_calibration(x, true, degree = 1, segments = FALSE)
  td <- tidy(cal)
  expect_equal(td$segment, c(1L, 1L))
  expect_equal(td$term, c("intercept", "slope"))
  expect_equal(td$estimate[1], 0.05, tolerance = 0.05)
  gl <- glance(cal)
  expect_equal(gl$n.lines, 6)
  expect_equal(gl$nobs, 6)
  expect_equal(gl$n.segments, 1)
  expect_lt(gl$rmse, 0.01)
  expect_equal(power_terms(6), c("intercept", "slope", "quadratic", "cubic", "power_4", "power_5"))

  set.seed(8)
  predicted <- 1:10 + stats::rnorm(10, sd = 0.1)
  reference <- 0.5 + 1.1 * predicted + stats::rnorm(10, sd = 0.05)
  sb <- slope_bias(predicted, reference)
  ts <- tidy(sb, conf.int = TRUE)
  expect_equal(ts$term, c("bias", "slope"))
  expect_equal(ts$statistic, (ts$estimate - c(0, 1)) / ts$std.error)
  expect_equal(cbind(ts$conf.low, ts$conf.high), unname(stats::confint(stats::lm(reference ~ predicted))))
  gs <- glance(sb)
  expect_equal(gs$nobs, 10)
  expect_lt(gs$rmse.after, gs$rmse.before)
  expect_true(gs$slope.fitted)
  bias_only <- tidy(slope_bias(predicted, reference, slope = FALSE), conf.int = TRUE)
  expect_equal(bias_only$conf.high[1] - bias_only$estimate[1],
               stats::qt(0.975, 9) * bias_only$std.error[1])
})

test_that("tidy and glance summarize a peak fit", {
  wl <- seq(395, 397, by = 0.01)
  set.seed(9)
  y <- 10 + 400 * exp(-(wl - 396.15)^2 / (2 * 0.05^2)) + stats::rnorm(length(wl))
  res <- peak_fit(wide_spectrum(wl, y), profile = "gaussian")
  fit <- res$fit[[1]]
  expect_s3_class(fit, c("specproc_peak_fit", "nls"))
  expect_equal(tidy(fit), res$tidied[[1]])
  ci <- tidy(fit, conf.int = TRUE)
  q <- stats::qt(0.975, stats::df.residual(fit))
  expect_equal(ci$conf.high - ci$estimate, q * ci$std.error, ignore_attr = TRUE)
  gl <- glance(fit)
  expect_equal(gl$nobs, length(wl))
  expect_equal(gl$rmse, sqrt(mean(stats::residuals(fit)^2)))
  expect_true(gl$isConv)
  expect_equal(gl$AIC, stats::AIC(fit))
})

test_that("tidy and glance summarize a curve of growth", {
  lines <- data.frame(
    species = "Ca I", wavelength = c(400, 405, 520, 560, 610, 650),
    Aki = c(1.5e8, 1.2e8, 3e7, 5e7, 4e7, 2e7), gk = c(4, 2, 6, 4, 3, 5),
    Ei = c(0, 0, 2.0, 2.1, 2.2, 2.4), stark_width = 0.02
  )
  lines$Ek <- lines$Ei + 1239.842 / lines$wavelength
  lines$intensity <- curve_of_growth_model(lines, 9000, scale = 1, optical_depth = 3)$intensity
  cog <- curve_of_growth(lines, temperature = 9000)
  td <- tidy(cog)
  expect_equal(td$term, "Ca I")
  expect_equal(td$estimate, attr(cog, "column_density")$column_density)
  expect_equal(td$n.lines, 6)
  gl <- glance(cog)
  expect_equal(gl$temperature, 9000)
  expect_equal(gl$nobs, 6)
  attr(cog, "column_density") <- NULL
  expect_error(tidy(cog), "lost the attributes")
})
