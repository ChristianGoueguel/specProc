# Stark widths from VALD3 extracts and Kurucz line lists, in the formats
# documented by VALD and Kurucz (made-up values).

vald_file <- function(lines) {
  file <- tempfile(fileext = ".txt")
  writeLines(lines, file)
  file
}

# FWHM in nm at 1e17 cm-3 and temperature, from log10 of gamma per electron
# at 1e4 K
expected_width <- function(wavelength, log_gamma, temperature = 1e4) {
  (wavelength * 1e-9)^2 / (2 * pi * 299792458) * 10^log_gamma * 1e17 * 1e9 *
    (temperature / 1e4)^(1 / 6)
}

short_all <- c(
  "                                            Damping parameters    Lande",
  "Elm Ion       WL_air(A) Excit(eV) log(gf)   Rad.  Stark    Waals  factor References",
  "'Cu 2',       4403.7511,  14.1980, -2.009, 9.300,-5.840,  -7.750, 1.460,'   1 K11       1 K11     Cu+    '",
  "'Cr 1',       4403.7742,   1.0300, -9.583, 0.930,-6.150,  -7.790, 1.770,'   2 K10       2 K10     Cr     '",
  "'Cr 1',       4403.7742,   1.0300,-11.444, 0.930,-6.150,  -7.790, 1.770,'   2 K10       2 K10     Cr     '",
  "'Ir 1',       4403.7890,   1.5150, -1.950, 0.000, 0.000,   0.000, 1.240,'   5 GHcor     5 GHcor   Ir     '",
  "'Cr 2',       4405.6660,   8.0910, -6.189, 8.910,-5.820,  -7.720, 2.450,'   4 K10       7 RU      Cr+    '",
  " References:",
  "  1. Kurucz obs. energy level: Cu 2"
)

test_that("read_vald reads the short format of Extract All", {
  x <- read_vald(vald_file(short_all), temperature = c(5000, 10000, 20000))
  expect_named(x, names(empty_stark_table()))
  # the line without a Stark constant is left out, the isotopic component kept once
  expect_equal(unique(x$species), c("Cu II", "Cr I", "Cr II"))
  expect_equal(nrow(x), 3 * 3)
  cu <- x[x$species == "Cu II", ]
  expect_equal(cu$wavelength, rep(440.37511, 3))
  expect_equal(cu$width, expected_width(440.37511, -5.84, c(5000, 10000, 20000)))
  expect_equal(unique(cu$density), 1e17)
  expect_equal(unique(cu$perturber), "electron")
  expect_equal(unique(cu$shift), 0)
  expect_equal(unique(cu$upper), "")
  expect_match(cu$source[1], "^VALD3 \\(K11\\), log gamma_Stark -5.84 at 10000 K$")
  expect_match(x$source[x$species == "Cr II"][1], "(K10, RU)", fixed = TRUE)
  # filters
  expect_equal(unique(read_vald(vald_file(short_all), species = "Cr I")$species), "Cr I")
  expect_equal(unique(read_vald(vald_file(short_all), wavelength = c(440.5, 441))$species),
               "Cr II")
  expect_error(read_vald(vald_file(short_all), species = "Cr"), "Roman")
})

test_that("read_vald converts the wavelength units of the header", {
  nm <- sub("WL_air(A)", "WL_air(nm)", short_all, fixed = TRUE)
  nm[3] <- sub("4403.7511", "440.37511", nm[3], fixed = TRUE)
  expect_equal(read_vald(vald_file(nm), species = "Cu II")$wavelength[1], 440.37511)
  wn <- sub("WL_air(A)", "WL_vac(cm^-1)", short_all, fixed = TRUE)
  wn[3] <- sub("4403.7511", "22708.0", wn[3], fixed = TRUE)
  expect_equal(read_vald(vald_file(wn), species = "Cu II")$wavelength[1], 1e7 / 22708)
})

test_that("read_vald reads the long format with the level designations", {
  long <- c(
    "                                                                   Lande factors      Damping parameters",
    "Elm Ion      WL_air(A)   log(gf) E_low(eV) J lo  E_up(eV) J up  lower  upper   mean   Rad.  Stark  Waals",
    "'Cu 2',       4403.7511,  -2.009, 14.1980,  1.0, 17.0120,  1.0, 1.890, 1.040, 1.460, 9.300,-5.840,  -7.750",
    "'  LS                                                                        3d9.(2D).4d 3S'",
    "'  LS                                                              3d8.(3P).4s.4p.(3P*) 1P*'",
    "'K11               Kurucz CuII 2011   1 K11       1 K11       1 K11     Cu+           '",
    "'Fe 2',       4405.7002,  -8.648, 11.2080,  3.5, 14.0210,  2.5, 1.410, 1.370, 1.460, 8.200,-4.340,  -7.420",
    "'  LS                                                                       3d6.(5D).5p 4D*'",
    "'  JK                                                                   3d6.(5D<4>).5g 2[2]'",
    "'K10               Kurucz FeII 2010   3 K10       3 K10       3 K10     Fe+           '",
    " References:"
  )
  x <- read_vald(vald_file(long), temperature = 1e4)
  expect_equal(x$species, c("Cu II", "Fe II"))
  expect_equal(x$lower, c("3d9.(2D).4d 3S", "3d6.(5D).5p 4D*"))
  expect_equal(x$upper, c("3d8.(3P).4s.4p.(3P*) 1P*", "3d6.(5D<4>).5g 2[2]"))
  expect_equal(x$width, expected_width(c(440.37511, 440.57002), c(-5.84, -4.34)))
  expect_match(x$source[2], "VALD3 (K10)", fixed = TRUE)
})

test_that("read_vald reads Extract Stellar, short and long", {
  stellar <- c(
    "                                                    Damping parameters   Lande  Central",
    "Spec Ion       WL_air(A)  Excit(eV) Vmic log gf*  Rad.  Stark  Waals  factor  depth  Reference",
    "'Fe 1',       4403.9663,   4.1540, 1.0, -1.810, 8.340,-4.340,  -7.340, 1.370, 0.430, '  1 wl:K14   1 gf:K14   1 K14  Fe   '"
  )
  x <- read_vald(vald_file(stellar), temperature = 1e4)
  expect_equal(x$width, expected_width(440.39663, -4.34))
  expect_match(x$source, "VALD3 (K14)", fixed = TRUE)
  stellar_long <- c(
    "                                                                   Lande factors      Damping parameters  Central",
    "Spec Ion     WL_air(A)   log gf* E_low(eV) J lo  E_up(eV) J up  lower  upper   mean   Rad.  Stark  Waals  depth",
    "'Fe 1',       4403.9663,  -1.810,  4.1540,  2.0,  6.9690,  3.0, 1.500, 1.250, 1.370, 8.340,-4.340,  -7.340, 0.430,",
    "'  LS                                                            3d7.(4F).4p y5D*'",
    "'  LS                                                            3d7.(4F).5s e5F'",
    "'K14               Kurucz FeI 2014   1 K14       1 K14     Fe            '"
  )
  y <- read_vald(vald_file(stellar_long), temperature = 1e4)
  expect_equal(y$width, x$width)
  expect_equal(y$lower, "3d7.(4F).4p y5D*")
})

test_that("read_vald checks its input", {
  expect_error(read_vald(tempfile()), "existing line list")
  expect_error(read_vald(vald_file(c("a", "b"))), "not a VALD3 extract")
  expect_error(read_vald(vald_file(short_all), temperature = -1), "temperature")
  odd <- c(short_all[1:2], "'Cu 2', 4403.7511, 14.1980, -2.009, 9.300,'  1 K11 '")
  expect_error(read_vald(vald_file(odd)), "unknown VALD3 format")
  expect_warning(none <- read_vald(vald_file(short_all), species = "Zn I"), "No line")
  expect_equal(nrow(none), 0)
})

# A record of the 160-column format of the Kurucz line lists.
kurucz_line <- function(wl, code, e1, j1, l1, e2, j2, l2, stark, ref = "K10") {
  sprintf("%11.4f%7.3f%6.2f%12.3f%5.1f %-10s%12.3f%5.1f %-10s%6.2f%6.2f%6.2f%-4s", wl, 0, code,
          e1, j1, l1, e2, j2, l2, 8.17, stark, -7.76, ref)
}

test_that("read_kurucz reads the 160-column format", {
  # the format of the Ca II K line in the Kurucz list of Ca II
  expect_equal(kurucz_line(393.3663, 20.01, 0, 0.5, "4s 2S", 25414.4, 1.5, "4p 2P", -6.27),
               sub("  0.111", "  0.000", paste0(
                 "   393.3663  0.111 20.01       0.000  0.5 4s 2S        25414.400  1.5 4p 2P",
                 "       8.17 -6.27 -7.76K10 "), fixed = TRUE))
  file <- vald_file(c(
    kurucz_line(393.3663, 20.01, 0, 0.5, "4s 2S", 25414.4, 1.5, "4p 2P", -6.27),
    # the upper level first, and a predicted level (negative energy)
    kurucz_line(854.2091, 20.01, 25414.4, 1.5, "4p 2P", -13710.88, 2.5, "3d 2D", -6.52),
    kurucz_line(285.2127, 12.00, 0, 0, "3s2 1S", 35051.264, 1, "3s3p 1P", -5.75),
    kurucz_line(500.0000, 26.00, 0, 4, "a5D", 20000, 4, "z5D", 0)
  ))
  x <- read_kurucz(file, temperature = c(5000, 20000))
  expect_equal(unique(x$species), c("Mg I", "Ca II"))
  ca <- x[x$species == "Ca II" & x$temperature == 20000, ]
  expect_equal(ca$wavelength, c(393.3663, 854.2091))
  expect_equal(ca$lower, c("4s 2S", "3d 2D"))
  expect_equal(ca$upper, c("4p 2P", "4p 2P"))
  expect_equal(ca$width, expected_width(c(393.3663, 854.2091), c(-6.27, -6.52), 20000))
  expect_match(ca$source[1], "^Kurucz \\(K10\\), log gamma_Stark -6.27")
  expect_equal(nrow(read_kurucz(file, species = "Mg I", temperature = 1e4)), 1)
  expect_equal(read_kurucz(file, wavelength = c(800, 900), temperature = 1e4)$wavelength, 854.2091)
  expect_error(read_kurucz(vald_file(short_all)), "not a Kurucz line list")
})

test_that("stark_width needs the species of a table of several species", {
  file <- vald_file(short_all)
  x <- read_vald(file)
  expect_error(stark_width(x, 440.38, 9000), "several species")
  cr <- stark_width(x, 440.38, 9000, species = "Cr I")
  expect_equal(cr$tabulated_wavelength, 440.37742)
  expect_equal(cr$width, expected_width(440.37742, -6.15, 9000) * (440.38 / 440.37742)^2)
  expect_error(stark_width(x, 440.38, 9000, species = "Zn I"), "No data for Zn I")
  ne <- electron_density(2 * cr$width, stark = x, wavelength = 440.38, temperature = 9000,
                         species = "Cr I")
  expect_equal(ne, 2e17)
  expect_error(electron_density(0.01, stark = x, wavelength = 440.38, temperature = 9000),
               "several species")
  # a table of one species needs no `species`
  expect_equal(stark_width(read_vald(file, species = "Cr I"), 440.38, 9000)$width, cr$width)
})
