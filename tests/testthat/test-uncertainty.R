kb_u <- 8.617333262e-5

# Lines of one species on the Boltzmann plot of a plasma at `temperature`
# (energy units), and of its ion with the Saha equation when `ne` is given.
u_lines <- function(temperature = 9000, ne = NULL, ionization_energy = 6.11) {
  lines <- data.frame(
    stage = c(1, 1, 1, 1, 2, 2, 2),
    wavelength = c(400, 420, 450, 500, 390, 395, 850),
    Aki = c(1e8, 5e7, 2e7, 3e7, 1.5e8, 1.4e8, 1e7),
    gk = c(3, 5, 7, 9, 4, 2, 6),
    Ek = c(3.1, 3.9, 4.6, 6.0, 3.1, 3.2, 3.2)
  )
  saha <- if (is.null(ne)) 1 else saha_factor(temperature, ne)
  energy <- lines$Ek + (lines$stage == 2) * ionization_energy
  lines$intensity <- with(lines, gk * Aki / wavelength * exp(-energy / (kb_u * temperature)) *
                            ifelse(stage == 2, saha, 1))
  lines
}

# Two elements at a known temperature and composition, for CF-LIBS
u_partitions <- c("Fe I" = 30, "Fe II" = 40, "Ca I" = 3, "Ca II" = 2)
u_partition <- function(species, temperature) u_partitions[species]
u_cf_lines <- function(temperature = 9000) {
  n <- c("Fe I" = 0.14, "Fe II" = 0.56, "Ca I" = 0.015, "Ca II" = 0.285)
  lines <- data.frame(
    species = c("Fe I", "Fe I", "Fe I", "Fe II", "Fe II", "Ca I", "Ca I", "Ca II"),
    wavelength = c(371, 404, 438, 259, 275, 422, 445, 393),
    Aki = c(1.6e7, 8.6e7, 5.0e7, 2.2e8, 2.1e8, 2.2e8, 8.7e7, 1.5e8),
    gk = c(11, 9, 11, 10, 8, 3, 7, 4),
    Ek = c(3.33, 4.55, 4.31, 4.77, 5.55, 2.93, 4.68, 3.15)
  )
  lines$intensity <- with(lines, n[species] / u_partitions[species] * gk * Aki / wavelength *
                            exp(-Ek / (kb_u * temperature)))
  lines
}

test_that("aki_uncertainty converts the NIST accuracy grades", {
  expect_equal(aki_uncertainty(c("AAA", "AA", "A+", "A", "B+", "B", "C+", "C", "D+", "D")),
               c(0.003, 0.01, 0.02, 0.03, 0.07, 0.10, 0.18, 0.25, 0.40, 0.50))
  expect_equal(aki_uncertainty(c(" b+", "c")), c(0.07, 0.25))
  expect_true(all(is.na(aki_uncertainty(c("E", "", NA, "X")))))
  expect_length(aki_uncertainty(character()), 0)
})

test_that("electron_density_uncertainty propagates the width and the reference", {
  z <- stats::qnorm(0.975)
  # H-alpha: the relative uncertainty of the width is amplified 1 / 0.67823 times
  h <- electron_density_uncertainty(c(0.4, 0.5), width_se = 0.02, method = "halpha")
  expect_s3_class(h, "tbl_df")
  expect_equal(h$electron_density, electron_density(c(0.4, 0.5), method = "halpha"))
  sigma <- sqrt(log1p((0.02 / c(0.4, 0.5))^2)) / 0.67823
  expect_equal(h$rsd, sqrt(expm1(sigma^2)))
  expect_equal(h$se, h$electron_density * h$rsd)
  expect_equal(h$lower, h$electron_density * exp(-z * sigma))
  expect_equal(h$upper, h$electron_density * exp(z * sigma))
  # a line with a reference width: the two sources add in quadrature
  s <- electron_density_uncertainty(0.024, width_se = 0.0024, reference_width = 0.012,
                                    reference_rsd = 0.2, level = 0.9)
  expect_equal(s$electron_density, 2e17)
  sigma <- sqrt(log1p(0.1^2) + log1p(0.2^2))
  expect_equal(s$rsd, sqrt(expm1(sigma^2)))
  expect_equal(s$upper, 2e17 * exp(stats::qnorm(0.95) * sigma))
  # exact inputs
  expect_equal(electron_density_uncertainty(0.5, method = "halpha")$se, 0)
  expect_error(electron_density_uncertainty(c(0.4, 0.5), width_se = c(1, 2, 3), method = "halpha"),
               "width_se")
  expect_error(electron_density_uncertainty(0.5, width_se = -1, method = "halpha"), "width_se")
  expect_error(electron_density_uncertainty(0.5, method = "halpha", reference_rsd = -0.1),
               "reference_rsd")
})

test_that("plasma_uncertainty of a Boltzmann plot matches the analytic standard error", {
  lines <- u_lines()[1:4, ]
  fit <- boltzmann(lines)
  set.seed(1)
  u <- plasma_uncertainty(fit, n = 5000, aki_rsd = 0.1)
  expect_s3_class(u, "specproc_uncertainty")
  expect_equal(u$estimates$quantity, "temperature")
  expect_equal(u$estimates$estimate, 9000)
  expect_equal(nrow(u$draws), 5000)
  expect_equal(u$inputs$aki_rsd, rep(0.1, 4))
  expect_equal(u$inputs$intensity_rsd, rep(0, 4))
  # linear error propagation: sd(slope) = sigma_y / sqrt(Sxx), T = -1 / (k slope)
  sigma_y <- sqrt(log1p(0.1^2))
  slope_se <- sigma_y / sqrt(sum((lines$Ek - mean(lines$Ek))^2))
  expect_equal(u$estimates$se, slope_se * kb_u * 9000^2, tolerance = 0.05)
  expect_lt(u$estimates$lower, 9000)
  expect_gt(u$estimates$upper, 9000)
  expect_equal(u$failed, 0)
  # exact points: no scatter
  expect_equal(u$chi_squared, 0)
  expect_output(print(u), "Boltzmann plot")

  # the same uncertainty from the intensities
  lines$intensity_se <- 0.1 * lines$intensity
  set.seed(1)
  ui <- plasma_uncertainty(boltzmann(lines), n = 5000)
  expect_equal(ui$inputs$intensity_rsd, rep(0.1, 4))
  expect_equal(ui$estimates$se, u$estimates$se, tolerance = 0.05)
})

test_that("plasma_uncertainty reads the accuracy grades and computes the chi-squared", {
  set.seed(2)
  lines <- u_lines()[1:4, ]
  lines$intensity <- lines$intensity * exp(rnorm(4, sd = 0.1))
  lines$accuracy <- c("B", "C", "C+", "B+")
  fit <- boltzmann(lines)
  u <- plasma_uncertainty(fit, n = 100)
  expect_equal(u$inputs$aki_rsd, c(0.10, 0.25, 0.18, 0.07))
  sigma <- sqrt(log1p(u$inputs$aki_rsd^2))
  expect_equal(u$chi_squared, sum((stats::residuals(fit$fit) / sigma)^2) / 2)
  expect_equal(u$temperature_se, fit$temperature_se)
  # aki_rsd overrides the grades
  expect_equal(plasma_uncertainty(fit, n = 10, aki_rsd = 0.05)$inputs$aki_rsd, rep(0.05, 4))
  lines$accuracy[2] <- "E"
  expect_error(plasma_uncertainty(boltzmann(lines), n = 10), "420 nm.*grade E")
  expect_no_error(plasma_uncertainty(boltzmann(lines), n = 10, aki_rsd = 0.5))
})

test_that("plasma_uncertainty gives exact draws for exact inputs", {
  fit <- boltzmann(u_lines()[1:4, ])
  u <- plasma_uncertainty(fit, n = 20)
  expect_equal(u$draws$temperature, rep(9000, 20))
  expect_equal(u$estimates$se, 0)
  expect_true(is.na(u$chi_squared))
  expect_output(print(u), "not included")
})

test_that("plasma_uncertainty counts the plots without a finite temperature", {
  lines <- u_lines()[1:3, ]
  lines$Ek <- c(4.0, 4.1, 4.2)
  lines$intensity <- with(lines, gk * Aki / wavelength * exp(-Ek / (kb_u * 9000)))
  set.seed(3)
  u <- plasma_uncertainty(boltzmann(lines), n = 400, aki_rsd = 0.5)
  expect_gt(u$failed, 0)
  expect_equal(u$failed, sum(is.infinite(u$draws$temperature)))
  expect_equal(u$estimates$upper, Inf)
  expect_true(is.finite(u$estimates$se))
  expect_output(print(u), "infinite temperatures")
})

test_that("plasma_uncertainty of a Saha-Boltzmann plot repeats the fit", {
  lines <- u_lines(12000, ne = 1e17)
  fit <- saha_boltzmann(lines, ionization_energy = 6.11, electron_density = 1e17)
  expect_equal(fit$settings$ionization_energy, 6.11)
  # exact inputs: every draw gives the temperature of the fit
  u0 <- plasma_uncertainty(fit, n = 10)
  expect_equal(u0$draws$temperature, rep(fit$temperature, 10), tolerance = 1e-6)
  # the draws are Saha-Boltzmann fits of the perturbed lines
  n <- 6
  set.seed(4)
  u <- plasma_uncertainty(fit, n = n, aki_rsd = 0.1, electron_density_se = 2e16)
  set.seed(4)
  e_int <- matrix(stats::rnorm(7 * n), 7) * 0
  e_aki <- matrix(stats::rnorm(7 * n), 7) * sqrt(log1p(0.1^2))
  ne <- 1e17 * exp(stats::rnorm(n) * sqrt(log1p(0.2^2)))
  expected <- vapply(seq_len(n), function(d) {
    l <- lines
    l$Aki <- l$Aki * exp(e_aki[, d])
    saha_boltzmann(l, ionization_energy = 6.11, electron_density = ne[d])$temperature
  }, numeric(1))
  expect_equal(u$draws$temperature, expected, tolerance = 1e-6)
  expect_equal(u$electron_density_rsd, 0.2)
  expect_output(print(u), "Electron density: +20%")
})

test_that("plasma_uncertainty of a CF-LIBS fit repeats the fit", {
  ne <- 1e17
  lines <- u_cf_lines()
  fit <- cf_libs(lines, partition = u_partition)
  expect_equal(fit$lines$intensity, lines$intensity)
  u0 <- plasma_uncertainty(fit, n = 5)
  expect_equal(u0$draws$temperature, rep(9000, 10))
  expect_equal(u0$draws$atomic_fraction, rep(c(0.7, 0.3), 5))
  expect_equal(u0$estimates$quantity,
               c("temperature", "atomic_fraction", "atomic_fraction", "mass_fraction",
                 "mass_fraction"))
  expect_equal(u0$estimates$element, c(NA, "Fe", "Ca", "Fe", "Ca"))

  n <- 4
  set.seed(5)
  u <- plasma_uncertainty(fit, n = n, aki_rsd = 0.2)
  set.seed(5)
  e_int <- matrix(stats::rnorm(8 * n), 8) * 0
  e_aki <- matrix(stats::rnorm(8 * n), 8) * sqrt(log1p(0.2^2))
  expected <- lapply(seq_len(n), function(d) {
    l <- lines
    l$Aki <- l$Aki * exp(e_aki[, d])
    cf_libs(l, partition = u_partition)
  })
  expect_equal(u$draws$temperature, rep(vapply(expected, `[[`, numeric(1), "temperature"), each = 2))
  expect_equal(u$draws$mass_fraction,
               unlist(lapply(expected, function(f) f$composition$mass_fraction)))
  sums <- tapply(u$draws$atomic_fraction, u$draws$draw, sum)
  expect_equal(as.vector(sums), rep(1, n))
  expect_true(all(u$estimates$se > 0))
  expect_output(print(u), "CF-LIBS fit")
  expect_null(u$electron_density_rsd)
  expect_error(plasma_uncertainty(fit, electron_density_se = 1e16), "electron density")
  expect_error(plasma_uncertainty(fit, temperature_se = 100), "given the temperature")
})

test_that("plasma_uncertainty propagates a given temperature to the composition", {
  fit <- cf_libs(u_cf_lines(), partition = u_partition, temperature = 9000)
  set.seed(6)
  u <- plasma_uncertainty(fit, n = 400, temperature_se = 450)
  t <- u$draws$temperature[u$draws$element == "Fe"]
  expect_equal(stats::sd(t), 450, tolerance = 0.15)
  expect_equal(u$temperature_rsd, 0.05)
  expect_true(is.na(u$chi_squared))
  expect_gt(u$estimates$se[u$estimates$quantity == "mass_fraction"][1], 0)
  expect_output(print(u), "Temperature: +5%")
})

test_that("plasma_uncertainty leaves out the CF-LIBS draws that fail", {
  fit <- cf_libs(u_cf_lines(), partition = u_partition)
  set.seed(7)
  expect_warning(u <- plasma_uncertainty(fit, n = 200, aki_rsd = 3), "draws failed")
  expect_gt(u$failed, 0)
  expect_equal(u$failed, sum(is.na(u$draws$temperature)) / 2)
  expect_true(all(is.finite(u$estimates$estimate)))
})

test_that("plasma_uncertainty validates its inputs", {
  fit <- boltzmann(u_lines()[1:4, ])
  expect_error(plasma_uncertainty(list()), "must be returned")
  old <- fit
  old$lines <- NULL
  expect_error(plasma_uncertainty(old), "fit it again")
  expect_error(plasma_uncertainty(fit, n = 1), "'n'")
  expect_error(plasma_uncertainty(fit, level = 1), "'level'")
  expect_error(plasma_uncertainty(fit, aki_rsd = c(0.1, 0.2)), "aki_rsd")
  expect_error(plasma_uncertainty(fit, electron_density_se = 1e16), "electron density")
  bad <- u_lines()[1:4, ]
  bad$intensity_se <- c(1, NA, 1, 1)
  expect_error(plasma_uncertainty(boltzmann(bad)), "intensity_se")
})

test_that("line_intensities gives the standard error of the intensities", {
  set.seed(8)
  wl <- seq(390, 400, by = 0.02)
  line <- 2000 * exp(-(wl - 395)^2 / (2 * 0.03^2))
  x <- matrix(100 + line, 1000, length(wl), byrow = TRUE) +
    matrix(stats::rnorm(1000 * length(wl), sd = 5), 1000)
  colnames(x) <- wl
  for (method in c("area", "height")) {
    for (baseline in c(TRUE, FALSE)) {
      out <- line_intensities(x, c(L = 395), method = method, baseline = baseline)
      expect_equal(mean(out$intensity_se), stats::sd(out$intensity), tolerance = 0.1,
                   label = paste(method, baseline))
    }
  }
  # the noise gain of the trapezoidal rule: 0.02 * sqrt(m - 1.5) without baseline
  expect_equal(noise_gain(seq(0, 0.3, by = 0.02), "area", FALSE), 0.02 * sqrt(16 - 1.5))
  expect_equal(noise_gain(seq(0, 0.3, by = 0.02), "height", FALSE, top = 8), 1)
  expect_equal(noise_gain(c(0, 1, 2), "height", TRUE, top = 2), sqrt(1.5))
  # the Voigt method gives the standard error of the fitted area, of the
  # same order as that of the integrated area
  v <- line_intensities(x[1:2, ], c(L = 395), method = "voigt", fit_width = 0.3)
  a <- line_intensities(x[1:2, ], c(L = 395), baseline = TRUE)
  expect_true(all(v$intensity_se > 0))
  expect_true(all(v$intensity_se / a$intensity_se > 0.3 & v$intensity_se / a$intensity_se < 3))
})

test_that("correct_self_absorption gives the standard error of the corrected intensities", {
  lines <- data.frame(species = "X I", wavelength = c(400, 420, 450, 480, 500),
                      Aki = c(2e8, 5e7, 2e7, 8e7, 3e7), gk = c(3, 5, 7, 5, 9),
                      Ek = c(3.1, 3.9, 4.6, 5.3, 6.0), Ei = c(0, 1.0, 1.9, 2.7, 3.5))
  lines$intensity <- with(lines, gk * Aki / wavelength * exp(-Ek / (kb_u * 9000)))
  lines$intensity[1] <- 0.5 * lines$intensity[1]
  lines$intensity_se <- lines$intensity * c(0.01, 0.02, 0.03, 0.04, 0.05)
  out <- correct_self_absorption(lines, temperature = 9000)
  expect_equal(which(out$reference), 5)
  expect_equal(out$intensity_se / out$intensity, rep(0.05, 5))
})
