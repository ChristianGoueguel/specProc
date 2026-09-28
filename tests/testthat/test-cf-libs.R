kb <- 8.617333262e-5

# Simulated lines of two elements at a known temperature and composition
cf_partitions <- c("Fe I" = 30, "Fe II" = 40, "Ca I" = 3, "Ca II" = 2)
cf_u <- function(species, temperature) cf_partitions[species]
cf_lines <- function(temperature = 9000, n = c("Fe I" = 0.14, "Fe II" = 0.56, "Ca I" = 0.015,
                                               "Ca II" = 0.285)) {
  lines <- data.frame(
    species = c("Fe I", "Fe I", "Fe I", "Fe II", "Fe II", "Ca I", "Ca I", "Ca II"),
    wavelength = c(371, 404, 438, 259, 275, 422, 445, 393),
    Aki = c(1.6e7, 8.6e7, 5.0e7, 2.2e8, 2.1e8, 2.2e8, 8.7e7, 1.5e8),
    gk = c(11, 9, 11, 10, 8, 3, 7, 4),
    Ek = c(3.33, 4.55, 4.31, 4.77, 5.55, 2.93, 4.68, 3.15)
  )
  lines$intensity <- with(lines, n[species] / cf_partitions[species] * gk * Aki / wavelength *
                            exp(-Ek / (kb * temperature)))
  lines
}

test_that("cf_libs recovers the temperature and composition", {
  fit <- cf_libs(cf_lines(), partition = cf_u)
  expect_s3_class(fit, "specproc_cflibs")
  expect_equal(fit$temperature, 9000)
  expect_equal(fit$composition$element, c("Fe", "Ca"))
  expect_equal(fit$composition$atomic_fraction, c(0.7, 0.3))
  mass <- c(55.845, 40.078) * c(0.7, 0.3)
  expect_equal(fit$composition$mass_fraction, mass / sum(mass))
  expect_equal(fit$species$density / sum(fit$species$density), c(0.14, 0.56, 0.015, 0.285))
  expect_true(all(fit$species$observed))
  expect_output(print(fit), "CF-LIBS|Calibration-free")
  expect_s3_class(plot_boltzmann(fit), "ggplot")
})

test_that("cf_libs uses a given temperature and photon units", {
  lines <- cf_lines()
  fit <- cf_libs(lines, partition = cf_u, temperature = 9000)
  expect_null(fit$fit)
  expect_equal(fit$composition$atomic_fraction, c(0.7, 0.3))
  lines$intensity <- lines$intensity * lines$wavelength
  expect_equal(cf_libs(lines, partition = cf_u, units = "photons")$composition$atomic_fraction,
               c(0.7, 0.3))
})

test_that("cf_libs completes a missing stage with the Saha equation", {
  temperature <- 9000
  ne <- 1e17
  # ionization energy consistent with Ca II / Ca I = 0.285 / 0.015
  e_ion <- -kb * temperature *
    log(0.285 / 0.015 / (saha_factor(temperature, ne) * cf_partitions[["Ca II"]] / cf_partitions[["Ca I"]]))
  lines <- cf_lines()
  no_ion <- lines[lines$species != "Ca II", ]
  fit <- cf_libs(no_ion, electron_density = ne, method = "boltzmann", partition = cf_u,
                 ionization_energy = c("Ca I" = e_ion))
  expect_equal(fit$composition$atomic_fraction, c(0.7, 0.3))
  expect_equal(fit$composition$stages[2], "I, II (Saha)")
  expect_false(fit$species$observed[fit$species$species == "Ca II"])
  # the ion alone, the neutral from the Saha equation
  no_neutral <- lines[lines$species != "Ca I", ]
  fit2 <- cf_libs(no_neutral, electron_density = ne, method = "boltzmann", partition = cf_u,
                  ionization_energy = c(Ca = e_ion))
  expect_equal(fit2$composition$atomic_fraction, c(0.7, 0.3))
  # without the electron density, the missing stage is neglected
  expect_warning(fit3 <- cf_libs(no_ion, partition = cf_u), "neglected")
  expect_equal(fit3$composition$atomic_fraction[2], 0.015 / (0.7 + 0.015))
  expect_error(cf_libs(no_ion, electron_density = ne, method = "boltzmann", partition = cf_u,
                       ionization_energy = c(Fe = 7.9)),
               "no value for Ca")
})

test_that("cf_libs scales to an internal reference", {
  fit <- cf_libs(cf_lines(), partition = cf_u, reference = c(Fe = 0.5))
  expect_equal(fit$composition$mass_fraction[1], 0.5)
  expect_equal(fit$composition$mass_fraction[2] / 0.5, 0.3 * 40.078 / (0.7 * 55.845))
  expect_error(cf_libs(cf_lines(), partition = cf_u, reference = c(Mg = 0.1)), "no line")
  expect_error(cf_libs(cf_lines(), partition = cf_u, reference = 0.1), "named")
})

test_that("cf_libs accepts partition functions as levels", {
  levels <- data.frame(species = rep(c("Fe I", "Fe II", "Ca I", "Ca II"), each = 2),
                       g = c(9, 7, 10, 8, 1, 3, 2, 4), energy = c(0, 0.05, 0, 0.05, 0, 1.9, 0, 1.7))
  lines <- cf_lines()
  e_ion <- c(Fe = 7.9024, Ca = 6.1132)
  fit <- cf_libs(lines, partition = levels, ionization_energy = e_ion)
  expect_equal(fit$species$partition,
               partition_function(levels, fit$temperature)$partition[c(1, 2, 3, 4)])
  expect_error(cf_libs(lines, partition = levels[levels$species != "Ca II", ], ionization_energy = e_ion),
               "no levels for Ca II")
  # neutral levels above the lowered ionization energy are left out
  high <- rbind(levels, data.frame(species = "Ca I", g = 50, energy = 6.05))
  fit_high <- cf_libs(lines, electron_density = 1e17, method = "boltzmann", partition = high,
                      ionization_energy = e_ion)
  expect_equal(fit_high$species$partition[3],
               partition_function(levels[levels$species == "Ca I", ], fit_high$temperature)$partition)
  expect_equal(cf_libs(lines, partition = high, ionization_energy = e_ion)$species$partition[3],
               partition_function(high[high$species == "Ca I", ], fit$temperature)$partition)
  expect_error(cf_libs(lines, partition = function(s, t) rep(-1, length(s))), "positive")
})

test_that("cf_libs checks its inputs", {
  lines <- cf_lines()
  expect_error(cf_libs(lines[, -1]), "species")
  bad <- lines
  bad$species[1] <- "Fe III"
  expect_error(cf_libs(bad, partition = cf_u), "stages I and II")
  one_each <- lines[!duplicated(lines$species), ]
  expect_error(cf_libs(one_each, partition = cf_u), "two lines")
  expect_equal(cf_libs(one_each, partition = cf_u, temperature = 9000)$composition$atomic_fraction,
               c(0.7, 0.3))
})

test_that("cf_libs fits Saha-Boltzmann plots when the electron density is known", {
  temperature <- 9500
  ne <- 1e17
  e_ion <- c(Fe = 7.9024, Ca = 6.1132)
  kt <- kb * temperature
  # ionization balance from the Saha equation, 70% Fe and 30% Ca atoms
  ion_fraction <- vapply(names(e_ion), function(el) {
    r <- saha_factor(temperature, ne) * cf_partitions[[paste(el, "II")]] /
      cf_partitions[[paste(el, "I")]] * exp(-e_ion[[el]] / kt)
    r / (1 + r)
  }, numeric(1))
  n <- c("Fe I" = 0.7 * (1 - ion_fraction[["Fe"]]), "Fe II" = 0.7 * ion_fraction[["Fe"]],
         "Ca I" = 0.3 * (1 - ion_fraction[["Ca"]]), "Ca II" = 0.3 * ion_fraction[["Ca"]])
  lines <- cf_lines(temperature, n)
  fit <- cf_libs(lines, electron_density = ne, partition = cf_u, ionization_energy = e_ion)
  expect_equal(fit$method, "saha-boltzmann")
  expect_equal(fit$temperature, temperature, tolerance = 1e-6)
  expect_equal(fit$composition$atomic_fraction, c(0.7, 0.3), tolerance = 1e-6)
  expect_equal(levels(fit$points$group), c("Fe", "Ca"))
  expect_equal(fit$points$x[fit$points$species == "Ca II"], 3.15 + e_ion[["Ca"]])
  expect_gt(fit$iterations, 1)
  expect_output(print(fit), "Saha-Boltzmann")
  expect_s3_class(plot_boltzmann(fit), "ggplot")
  # an element seen as an ion only: its neutral follows from the Saha equation
  no_ca_neutral <- lines[lines$species != "Ca I", ]
  fit2 <- cf_libs(no_ca_neutral, electron_density = ne, partition = cf_u, ionization_energy = e_ion)
  expect_equal(fit2$composition$atomic_fraction, c(0.7, 0.3), tolerance = 1e-6)
  expect_equal(fit2$composition$stages[2], "I (Saha), II")
  # a given temperature
  fit3 <- cf_libs(lines, electron_density = ne, temperature = temperature, partition = cf_u,
                  ionization_energy = e_ion)
  expect_equal(fit3$composition$atomic_fraction, c(0.7, 0.3), tolerance = 1e-6)
  expect_error(cf_libs(lines, method = "saha-boltzmann", partition = cf_u), "electron_density")
  expect_error(cf_libs(lines, electron_density = ne, partition = cf_u, ionization_energy = c(Fe = 7.9)),
               "no value for Ca")
})

test_that("the lowering of the ionization energy follows Debye-Hueckel", {
  # Debye length at 1e4 K and 1e17 cm-3 is 21.8 nm; e^2 / (4 pi eps0 lambda_D) = 0.066 eV
  expect_equal(ionization_lowering(1e4, 1e17), 0.0660, tolerance = 1e-3)
  expect_equal(ionization_lowering(1e4, NULL), 0)
})
