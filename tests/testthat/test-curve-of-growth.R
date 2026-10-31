cog_lines <- function() {
  lines <- data.frame(
    species = "Ca I", wavelength = c(400, 405, 520, 560, 610, 650),
    Aki = c(1.5e8, 1.2e8, 3e7, 5e7, 4e7, 2e7), gk = c(4, 2, 6, 4, 3, 5),
    Ei = c(0, 0, 2.0, 2.1, 2.2, 2.4), stark_width = 0.02
  )
  lines$Ek <- lines$Ei + 1239.842 / lines$wavelength
  lines
}

test_that("the curve of growth integrates the line profile", {
  # W(tau) of the tables against direct integration
  for (wl in c(0, 0.02, 0.2)) {
    wg <- 0.005
    peak <- profile_voigt(0, 0, wg, wl)
    table <- cog_table(wg, wl, peak)
    for (tau in c(1e-3, 1, 30, 1e4)) {
      f <- function(x) -expm1(-tau * profile_voigt(x, 0, wg, wl) / peak)
      ref <- 2 * (stats::integrate(f, 0, 1, subdivisions = 2000, rel.tol = 1e-10)$value +
                    stats::integrate(f, 1, Inf, rel.tol = 1e-10)$value)
      expect_equal(cog_width(table, tau), ref, tolerance = 1e-4)
    }
    # optically thin: W = tau / V0
    expect_equal(cog_width(table, 1e-6), 1e-6 / peak, tolerance = 1e-3)
  }
})

test_that("curve_of_growth_model follows the optically thin limit", {
  lines <- cog_lines()
  thin <- curve_of_growth_model(lines, 9000, scale = 1, optical_depth = 1e-4)
  expect_equal(thin$SA, rep(1, 6), tolerance = 1e-3)
  # the thin intensities lie on a Boltzmann plot of the temperature
  lines$intensity <- thin$thin_intensity
  expect_equal(boltzmann(lines)$temperature, 9000, tolerance = 1e-6)
  thick <- curve_of_growth_model(lines, 9000, scale = 1, optical_depth = 5)
  expect_equal(max(thick$optical_depth), 5)
  expect_lt(thick$SA[1], 0.5)
  expect_equal(thick$intensity, thick$SA * thick$thin_intensity)
  # photon units: intensities divided by the photon energy
  photons <- curve_of_growth_model(lines, 9000, scale = 1, optical_depth = 5, units = "photons")
  expect_equal(photons$intensity / thick$intensity,
               lines$wavelength / mean(lines$wavelength) * (photons$intensity[1] /
                 thick$intensity[1]) * mean(lines$wavelength) / lines$wavelength[1])
})

test_that("curve_of_growth recovers the optical depths and the temperature", {
  lines <- cog_lines()
  truth <- curve_of_growth_model(lines, 9000, scale = 1e20, optical_depth = 5)
  lines$intensity <- truth$intensity
  cog <- curve_of_growth(lines, temperature = 9000)
  expect_s3_class(cog, "specproc_curve_of_growth")
  expect_equal(cog$optical_depth, truth$optical_depth, tolerance = 1e-3)
  expect_equal(cog$SA, truth$SA, tolerance = 1e-3)
  expect_equal(cog$intensity, truth$thin_intensity, tolerance = 1e-3)
  expect_equal(cog$measured_intensity, lines$intensity)
  expect_equal(cog$doppler_width, doppler_width(lines$wavelength, 9000, "Ca"))
  expect_equal(attr(cog, "column_density")$max_optical_depth, 5, tolerance = 1e-3)
  # the corrected intensities give the temperature back; the measured ones do not
  expect_equal(boltzmann(cog)$temperature, 9000, tolerance = 1e-3)
  expect_gt(boltzmann(lines)$temperature, 10000)
  expect_output(print(cog), "Curve of growth")
  expect_s3_class(plot_curve_of_growth(cog), "ggplot")
  styled <- plot_curve_of_growth(cog, xlab = "tau", point_size = 4, base_size = 9)
  expect_equal(styled$labels$x, "tau")
  expect_equal(styled$layers[[3]]$aes_params$size, 4)
  # optically thin lines keep their intensities
  lines$intensity <- curve_of_growth_model(lines, 9000, 1e20, 1e-3)$intensity
  expect_equal(curve_of_growth(lines, 9000)$SA, rep(1, 6), tolerance = 1e-3)
})

test_that("curve_of_growth fits one column density per species", {
  ca <- cog_lines()
  k <- data.frame(species = "K I", wavelength = c(404.41, 404.72, 691.11, 693.88, 766.49),
                  Aki = c(1.15e6, 1.07e6, 2.5e6, 4.96e6, 3.78e7), gk = c(4, 2, 2, 2, 4),
                  Ei = c(0, 0, 1.61, 1.617, 0), stark_width = 0.03)
  k$Ek <- k$Ei + 1239.842 / k$wavelength
  lines <- rbind(ca, k)
  truth <- curve_of_growth_model(lines, 9000, scale = 1e20,
                                 optical_depth = c(`Ca I` = 3, `K I` = 20))
  lines$intensity <- truth$intensity
  cog <- curve_of_growth(lines, 9000)
  cd <- attr(cog, "column_density")
  expect_equal(cd$species, c("Ca I", "K I"))
  expect_equal(cd$max_optical_depth, c(3, 20), tolerance = 1e-3)
  expect_equal(cog$SA, truth$SA, tolerance = 1e-3)
})

test_that("curve_of_growth validates its inputs", {
  lines <- cog_lines()
  lines$intensity <- 1
  expect_error(curve_of_growth(lines[setdiff(names(lines), "Ei")], 9000), "`Ei`")
  expect_error(curve_of_growth(lines[setdiff(names(lines), "stark_width")], 9000), "stark_width")
  expect_error(curve_of_growth(lines[1:2, ], 9000), "more lines")
  expect_error(curve_of_growth(lines, -1), "temperature")
  bad <- lines
  bad$stark_width[1] <- -1
  expect_error(curve_of_growth(bad, 9000), "negative")
  expect_error(curve_of_growth_model(lines, 9000, 1, optical_depth = c(`K I` = 1)), "per species")
  expect_error(plot_curve_of_growth(lines), "curve_of_growth")
})
