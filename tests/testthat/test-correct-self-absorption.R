kb_sa <- 8.617333262e-5

sa_lines <- function() {
  lines <- data.frame(species = "X I", wavelength = c(400, 420, 450, 480, 500),
                      Aki = c(2e8, 5e7, 2e7, 8e7, 3e7), gk = c(3, 5, 7, 5, 9),
                      Ek = c(3.1, 3.9, 4.6, 5.3, 6.0), Ei = c(0, 1.0, 1.9, 2.7, 3.5))
  lines$intensity <- with(lines, gk * Aki / wavelength * exp(-Ek / (kb_sa * 9000)))
  lines
}

test_that("correct_self_absorption recovers the absorbed intensities", {
  lines <- sa_lines()
  thin <- lines$intensity
  lines$intensity[1:2] <- lines$intensity[1:2] * c(0.4, 0.8)
  out <- correct_self_absorption(lines, temperature = 9000)
  expect_equal(out$SA, c(0.4, 0.8, 1, 1, 1))
  expect_equal(out$intensity, thin)
  expect_equal(out$measured_intensity, lines$intensity)
  expect_equal(which(out$reference), 5)   # smallest optical depth
  expect_equal(attr(out, "temperature"), 9000)
  # photon units
  photons <- lines
  photons$intensity <- photons$intensity * photons$wavelength
  expect_equal(correct_self_absorption(photons, temperature = 9000, units = "photons")$SA,
               c(0.4, 0.8, 1, 1, 1))
  # a given reference line
  ref <- correct_self_absorption(lines, temperature = 9000, reference = 3)
  expect_equal(ref$SA, c(0.4, 0.8, 1, 1, 1))
  expect_equal(which(ref$reference), 3)
})

test_that("correct_self_absorption estimates the temperature with the Saha equation", {
  temperature <- 10000
  ne <- 1e17
  e_ion <- 6.11
  lines <- data.frame(species = c("Y I", "Y I", "Y I", "Y II", "Y II", "Y II"),
                      wavelength = c(420, 445, 560, 390, 395, 850),
                      Aki = c(2e8, 8e7, 5e7, 1.5e8, 1.4e8, 1e7), gk = c(3, 5, 7, 4, 2, 6),
                      Ek = c(2.9, 4.7, 5.0, 3.1, 3.2, 3.2), Ei = c(0, 1.9, 2.5, 0, 0, 1.7))
  ion <- lines$species == "Y II"
  lines$intensity <- with(lines, gk * Aki / wavelength *
                            exp(-(Ek + ion * e_ion) / (kb_sa * temperature)) *
                            ifelse(ion, saha_factor(temperature, ne), 1))
  lines$intensity[c(1, 4)] <- lines$intensity[c(1, 4)] * c(0.5, 0.3)
  out <- correct_self_absorption(lines, electron_density = ne, ionization_energy = c(Y = e_ion))
  expect_equal(attr(out, "temperature"), temperature, tolerance = 1e-5)
  expect_equal(out$SA, c(0.5, 1, 1, 0.3, 1, 1), tolerance = 1e-5)
  expect_equal(sum(out$reference), 2)
  expect_error(correct_self_absorption(lines[!ion, ], electron_density = ne,
                                       ionization_energy = c(Y = e_ion)), "neutral atom and")
})

test_that("correct_self_absorption checks its inputs", {
  lines <- sa_lines()
  expect_error(correct_self_absorption(lines), "temperature")
  expect_error(correct_self_absorption(lines[names(lines) != "Ei"], temperature = 9000), "Ei")
  expect_error(correct_self_absorption(lines, temperature = 9000, reference = 9), "row numbers")
  two <- rbind(lines, transform(lines, species = "Z I"))
  expect_error(correct_self_absorption(two, temperature = 9000, reference = 1), "one line per species")
  bad <- lines
  bad$intensity[1] <- 2 * bad$intensity[1]
  expect_warning(correct_self_absorption(bad, temperature = 9000), "SA > 1")
})
