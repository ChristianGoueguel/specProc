kB <- 8.617333262e-5

# Line intensities of a plasma in LTE (energy units unless photons = TRUE)
simulate_lines <- function(temperature, ne = NULL, ionization_energy = 6.11, photons = FALSE) {
  lines <- data.frame(
    stage = c(1, 1, 1, 1, 2, 2, 2),
    wavelength = c(400, 420, 450, 500, 390, 395, 850),
    Aki = c(1e8, 5e7, 2e7, 3e7, 1.5e8, 1.4e8, 1e7),
    gk = c(3, 5, 7, 9, 4, 2, 6),
    Ek = c(3.1, 3.9, 4.6, 6.0, 3.1, 3.2, 3.2)
  )
  saha <- if (is.null(ne)) 1 else
    2 * (2 * pi * 9.1093837015e-31 * 1.380649e-23 * temperature / 6.62607015e-34^2)^1.5 * 1e-6 / ne
  energy <- lines$Ek + (lines$stage == 2) * ionization_energy
  lines$intensity <- with(lines, gk * Aki * exp(-energy / (kB * temperature)) *
                            ifelse(stage == 2, saha, 1) / if (photons) 1 else wavelength)
  lines
}

test_that("boltzmann_plot recovers the temperature", {
  lines <- simulate_lines(9000)[1:4, ]
  fit <- boltzmann_plot(lines)
  expect_s3_class(fit, "specproc_boltzmann")
  expect_equal(fit$temperature, 9000)
  expect_equal(fit$r_squared, 1)
  expect_lt(fit$temperature_se, 1e-6)

  photons <- simulate_lines(9000, photons = TRUE)[1:4, ]
  expect_equal(boltzmann_plot(photons, units = "photons")$temperature, 9000)

  expect_output(print(fit), "Temperature:  9000")
  expect_s3_class(plot_boltzmann(fit), "ggplot")
})

test_that("boltzmann_plot gives a standard error with noisy intensities", {
  set.seed(1)
  lines <- simulate_lines(9000)[1:4, ]
  lines$intensity <- lines$intensity * exp(rnorm(4, sd = 0.05))
  fit <- boltzmann_plot(lines)
  expect_gt(fit$temperature_se, 0)
  expect_equal(fit$temperature, 9000, tolerance = 0.1)
})

test_that("saha_boltzmann_plot recovers the temperature with ionic lines", {
  lines <- simulate_lines(12000, ne = 1e17)
  fit <- saha_boltzmann_plot(lines, ionization_energy = 6.11, electron_density = 1e17)
  expect_equal(fit$temperature, 12000, tolerance = 1e-6)
  expect_equal(fit$method, "Saha-Boltzmann")
  expect_lt(fit$iterations, 100)
  # the Saha-corrected points are all on the line
  expect_equal(fit$r_squared, 1)
  expect_s3_class(plot_boltzmann(fit), "ggplot")
  # a wrong electron density biases the temperature
  wrong <- saha_boltzmann_plot(lines, ionization_energy = 6.11, electron_density = 1e18)
  expect_false(isTRUE(all.equal(wrong$temperature, 12000, tolerance = 0.01)))
})

test_that("plasma functions validate their inputs", {
  lines <- simulate_lines(9000)
  expect_error(boltzmann_plot(lines[1:2, ]), "at least 3")
  expect_error(boltzmann_plot(lines[c("wavelength", "Aki")]), "needs the column")
  bad <- lines
  bad$Aki[1] <- -1
  expect_error(boltzmann_plot(bad), "positive")
  expect_error(saha_boltzmann_plot(lines[lines$stage == 1, ], 6.11, 1e17), "both stages")
  # intensities increasing with energy give a non-negative slope
  inverted <- lines[1:4, ]
  inverted$intensity <- rev(inverted$intensity)
  expect_error(boltzmann_plot(inverted), "slope")
})

test_that("mcwhirter_criterion computes the minimum density", {
  res <- mcwhirter_criterion(10000, 3, electron_density = c(1e16, 1e17))
  expect_equal(res$minimum_density, rep(1.6e12 * 100 * 27, 2))
  expect_equal(res$satisfied, c(TRUE, TRUE))
  expect_false(mcwhirter_criterion(10000, 10, electron_density = 1e15)$satisfied)
  expect_error(mcwhirter_criterion(-1, 3), "positive")
})

test_that("self_absorption follows the width relation", {
  res <- self_absorption(c(0.02, 0.04), thin_width = 0.02)
  expect_equal(res$SA, c(1, 2^(1 / -0.54)))
  expect_equal(res$intensity_correction, 1 / res$SA)
  expect_warning(self_absorption(0.01, 0.02), "narrower")
})

test_that("saturation_summary finds saturated channels", {
  x <- matrix(1000, 3, 5, dimnames = list(NULL, c("400", "401", "402", "403", "404")))
  x[1, 2] <- 65535
  x[2, 2:3] <- 65535
  sat <- saturation_summary(x)
  expect_equal(sat$spectra$n_saturated, c(1, 2, 0))
  expect_equal(sat$channels$wavelength, c(401, 402))
  expect_equal(sat$channels$n_spectra, c(2, 1))
  expect_equal(nrow(saturation_summary(x, limit = 70000)$channels), 0)
  expect_equal(saturation_summary(x, limit = 65540, tolerance = 10)$spectra$n_saturated, c(1, 2, 0))
})

test_that("voigt_fwhm has the Gaussian and Lorentzian limits", {
  expect_equal(voigt_fwhm(0.3, 0), 0.3)
  expect_equal(voigt_fwhm(0, 0.3), 0.3, tolerance = 1e-3)
  # numerical FWHM of the exact Voigt profile
  x <- seq(-3, 3, length.out = 200001)
  v <- voigt_profile(x, y0 = 0, xc = 0, wG = 0.4, wL = 0.3, A = 1)
  half <- x[v >= max(v) / 2]
  expect_equal(voigt_fwhm(0.4, 0.3), max(half) - min(half), tolerance = 1e-3)
  expect_error(voigt_fwhm(-1, 0.1), "non-negative")
})
