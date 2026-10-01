# Text in the format of the NIST ASD CSV output (made-up lines)
nist_csv <- c(
  'obs_wl_air(nm),ritz_wl_air(nm),wn(cm-1),intens,Aki(s^-1),fik,Acc,Ei(eV),Ek(eV),conf_i,term_i,J_i,conf_k,term_k,J_k,g_i,g_k,Type,tp_ref,line_ref,',
  '"=""400.000""","=""400.0001""","=""25000.0""","=""50""","=""1.00e+08""","=""5.0e-01""",B+,"=""0.0000000""","=""3.0990000""","=""4s""","=""2S""","=""1/2""","=""4p""","=""2P*""","=""3/2""",2,4,,"=""T1""","=""L1""",',
  '"=""""","=""410.5000""","=""24361.7""","=""""","=""2.5e+07""","=""1.0e-01""",C,"=""3.0990000""","=""[6.1182]""","=""4p""","=""2P*""","=""3/2""","=""5s""","=""2S""","=""1/2""",4,2,,"=""T2""","=""""",',
  '"=""420.000""","=""""","=""23809.5""","=""10""","=""""","=""""",,"=""""","=""""","=""""","=""""","=""""","=""""","=""""","=""""",1,1,,"=""""","=""L2""",'
)

test_that("parse_nist_lines reads the ASD CSV format", {
  lines <- parse_nist_lines(nist_csv, "X II")
  expect_equal(nrow(lines), 3)
  expect_equal(lines$species, rep("X II", 3))
  # observed wavelength first, Ritz wavelength when there is no observed one
  expect_equal(lines$wavelength, c(400, 410.5, 420))
  expect_equal(lines$Aki, c(1e8, 2.5e7, NA))
  expect_equal(lines$Ek, c(3.099, 6.1182, NA))   # brackets removed
  expect_equal(lines$gk, c(4, 2, 1))
  expect_equal(lines$accuracy, c("B+", "C", ""))
  expect_equal(lines$upper[1], "4p 2P* 3/2")
  expect_equal(lines$lower[1], "4s 2S 1/2")
})

test_that("parse_nist_ie reads ionization energies", {
  ie_csv <- c(
    'At. num,Sp. Name,Ion Charge,Prefix,Ionization Energy (eV),Suffix,Uncertainty (eV),References,',
    '"=""99""","=""Xx I""","=""0""","=""""","=""6.0000""","=""""","=""0.0001""","=""L1""",',
    '"=""99""","=""Xx II""","=""+1""","=""[""","=""12.5""","=""]""","=""0.1""","=""L2""",'
  )
  ie <- parse_nist_ie(ie_csv)
  expect_equal(ie$species, c("Xx I", "Xx II"))
  expect_equal(ie$energy, c(6, 12.5))
})

test_that("nist_lines output feeds boltzmann", {
  lines <- parse_nist_lines(nist_csv, "X II")[1:2, ]
  lines <- rbind(lines, transform(lines[1, ], wavelength = 430, Ek = 5))
  lines$intensity <- with(lines, gk * Aki / wavelength * exp(-Ek / (8.617333262e-5 * 9000)))
  expect_equal(boltzmann(lines)$temperature, 9000)
})

test_that("nist functions validate their inputs", {
  expect_error(nist_lines("Ca II"), "wavelength")
  expect_error(nist_lines("Ca II", c(300, NA)), "wavelength")
  expect_error(nist_lines("Ca+", c(300, 400)), "Roman")
  expect_error(nist_ionization_energy(1), "character")
})

test_that("nist_lines and nist_ionization_energy query the NIST database", {
  skip_on_cran()
  skip_if_offline("physics.nist.gov")
  ca <- tryCatch(nist_lines("Ca II", c(390, 400)),
                 error = function(e) skip(paste("NIST unavailable:", conditionMessage(e))))
  expect_true(any(abs(ca$wavelength - 393.366) < 0.01))
  expect_true(all(!is.na(ca$Aki)))
  ie <- tryCatch(nist_ionization_energy("Ca I"),
                 error = function(e) skip(paste("NIST unavailable:", conditionMessage(e))))
  expect_equal(unname(ie), 6.11, tolerance = 0.01)
})

test_that("parse_nist_levels reads levels and drops the limit rows", {
  levels_csv <- c(
    'Configuration,Term,J,g,Prefix,Level (eV),Suffix,Uncertainty (eV),Splitting,Reference',
    '"=""4s2""","=""1S""","=""0""",1,"=""""","=""0.0000000""","=""""","=""0""","=""""","=""L1"""',
    '"=""4s.4p""","=""3P*""","=""1""",3,"=""""","=""1.8858075""","=""""","=""""","=""""","=""L1"""',
    '"=""4s.5s""","=""3S""","=""1""",3,"=""[""","=""3.9103""","=""]""","=""""","=""""","=""L1"""',
    '"=""Xx II (4s 2S<1/2>)""","=""Limit""","=""---""",,"=""""","=""6.1131549""","=""""","=""""","=""""","=""L2"""',
    '"=""3d.4d""","=""1S""","=""0""",1,"=""""","=""6.5000""","=""""","=""""","=""""","=""L1"""',
    '"=""4p.4d""","=""""","=""""",,"=""""","=""6.9000""","=""""","=""""","=""""","=""L1"""',
    '',
    'Partition function for Te = 0.8617 eV: Z = 1.23'
  )
  lev <- parse_nist_levels(levels_csv, "Xx I")
  expect_equal(lev$energy, c(0, 1.8858075, 3.9103, 6.5))   # brackets removed, no g dropped
  expect_equal(lev$g, c(1, 3, 3, 1))
  expect_equal(lev$term[2], "3P*")
  pf <- partition_function(lev, c(5000, 10000))
  kt <- 8.617333262e-5 * c(5000, 10000)
  expect_equal(pf$partition, vapply(kt, function(k) sum(lev$g * exp(-lev$energy / k)), numeric(1)))
  expect_equal(pf$temperature, c(5000, 10000))
  cut <- partition_function(lev, 10000, max_energy = c("Xx I" = 6))
  expect_equal(cut$partition, sum((lev$g * exp(-lev$energy / kt[2]))[lev$energy <= 6]))
  expect_equal(partition_function(lev, 10000, max_energy = c("Yy I" = 0))$partition, pf$partition[2])
  expect_error(partition_function(lev, 1000, max_energy = "a"), "numeric")
  expect_error(partition_function(lev, -1), "positive")
  expect_error(partition_function(data.frame(g = 1), 1000), "columns")
})
