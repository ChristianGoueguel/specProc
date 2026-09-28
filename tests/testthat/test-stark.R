# A minimal XSAMS document in the format of the STARK-B VAMDC node, with
# made-up values (one Ca II multiplet, electrons at two temperatures and two
# densities, protons at one).
xsams <- '<?xml version="1.0" encoding="UTF-8"?>
<XSAMSData xmlns="http://vamdc.org/xml/xsams/1.0">
<Sources><Source sourceID="B1"><Title>Test tables</Title><Year>1999</Year></Source></Sources>
<Environments>
<Environment envID="E1"><Temperature><Value units="K">5000</Value></Temperature><TotalNumberDensity><Value units="1/cm3">1e16</Value></TotalNumberDensity><Composition><Species name="electron" speciesRef="XP1"></Species></Composition></Environment>
<Environment envID="E2"><Temperature><Value units="K">20000</Value></Temperature><TotalNumberDensity><Value units="1/cm3">1e16</Value></TotalNumberDensity><Composition><Species name="electron" speciesRef="XP1"></Species></Composition></Environment>
<Environment envID="E3"><Temperature><Value units="K">5000</Value></Temperature><TotalNumberDensity><Value units="1/cm3">1e17</Value></TotalNumberDensity><Composition><Species name="electron" speciesRef="XP1"></Species></Composition></Environment>
<Environment envID="E4"><Temperature><Value units="K">20000</Value></Temperature><TotalNumberDensity><Value units="1/cm3">1e17</Value></TotalNumberDensity><Composition><Species name="electron" speciesRef="XP1"></Species></Composition></Environment>
<Environment envID="E5"><Temperature><Value units="K">5000</Value></Temperature><TotalNumberDensity><Value units="1/cm3">1e17</Value></TotalNumberDensity><Composition><Species name="Hydrogen" speciesRef="X1"></Species></Composition></Environment>
</Environments>
<Species><Atoms>
<Atom><ChemicalElement><NuclearCharge>1</NuclearCharge><ElementSymbol>H</ElementSymbol></ChemicalElement><Isotope><Ion speciesID="X1"><IonCharge>1</IonCharge></Ion></Isotope></Atom>
<Atom><ChemicalElement><NuclearCharge>20</NuclearCharge><ElementSymbol>Ca</ElementSymbol></ChemicalElement><Isotope><Ion speciesID="X13"><IonCharge>1</IonCharge>
<AtomicState stateID="S1"><AtomicComposition><Component><Configuration><ConfigurationLabel>3p6.4s</ConfigurationLabel></Configuration><Term><TermLabel>2S</TermLabel></Term></Component></AtomicComposition></AtomicState>
<AtomicState stateID="S2"><AtomicComposition><Component><Configuration><ConfigurationLabel>3p6.4p</ConfigurationLabel></Configuration><Term><TermLabel>2Po</TermLabel></Term></Component></AtomicComposition></AtomicState>
</Ion></Isotope></Atom>
</Atoms></Species>
<Processes><Radiative>
<RadiativeTransition id="P1"><SourceRef>B1</SourceRef><EnergyWavelength><Wavelength><Value units="A">3950.0</Value></Wavelength></EnergyWavelength><UpperStateRef>S2</UpperStateRef><LowerStateRef>S1</LowerStateRef><SpeciesRef>X13</SpeciesRef>
<Broadening name="pressure" envRef="E1"><Lineshape name="Lorentzian"><LineshapeParameter name="gammaL"><Value units="A">0.030</Value></LineshapeParameter></Lineshape></Broadening>
<Broadening name="pressure" envRef="E2"><Lineshape name="Lorentzian"><LineshapeParameter name="gammaL"><Value units="A">0.015</Value></LineshapeParameter></Lineshape></Broadening>
<Broadening name="pressure" envRef="E3"><Lineshape name="Lorentzian"><LineshapeParameter name="gammaL"><Value units="A">0.30</Value></LineshapeParameter></Lineshape></Broadening>
<Broadening name="pressure" envRef="E4"><Lineshape name="Lorentzian"><LineshapeParameter name="gammaL"><Value units="A">0.15</Value></LineshapeParameter></Lineshape></Broadening>
<Broadening name="pressure" envRef="E5"><Lineshape name="Lorentzian"><LineshapeParameter name="gammaL"><Value units="A">0.01</Value></LineshapeParameter></Lineshape></Broadening>
<Shifting envRef="E3"><ShiftingParameter name="delta"><Value units="A">-0.05</Value></ShiftingParameter></Shifting>
<Shifting envRef="E4"><ShiftingParameter name="delta"><Value units="A">-0.03</Value></ShiftingParameter></Shifting>
</RadiativeTransition>
</Radiative></Processes>
</XSAMSData>'

test_that("parse_xsams reads widths, shifts and plasma conditions", {
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".xml")
  writeLines(xsams, file)
  stark <- parse_xsams(file)
  expect_equal(nrow(stark), 5)
  expect_equal(unique(stark$species), "Ca II")
  expect_equal(unique(stark$wavelength), 395)
  expect_equal(unique(stark$upper), "3p6.4p 2Po")
  expect_equal(unique(stark$lower), "3p6.4s 2S")
  expect_setequal(stark$perturber, c("electron", "H II"))
  e17 <- stark[stark$perturber == "electron" & stark$density == 1e17, ]
  expect_equal(e17$temperature, c(5000, 20000))
  expect_equal(e17$width, c(0.030, 0.015))    # nm
  expect_equal(e17$shift, c(-0.005, -0.003))
  expect_equal(unique(stark$source), "Test tables (1999)")
  expect_true(is.na(stark$shift[stark$perturber == "H II"]))
})

test_that("stark_width interpolates in log T and scales with density", {
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".xml")
  writeLines(xsams, file)
  stark <- parse_xsams(file)
  w <- stark_width(stark, 395, temperature = c(5000, 10000, 20000), density = 1e17)
  # log-log interpolation halfway (in log T) between 0.030 and 0.015
  expect_equal(w$width, c(0.030, sqrt(0.030 * 0.015), 0.015))
  # the closest tabulated density (1e17) is scaled linearly
  expect_equal(stark_width(stark, 395, 5000, density = 2e17)$width, 0.060)
  expect_equal(stark_width(stark, 395, 20000, density = 1e17)$shift, -0.003)
  expect_error(stark_width(stark, 400, 5000), "No tabulated line")
  expect_error(stark_width(stark, 395, 40000), "tabulated range")
  expect_error(stark_width(stark, 395, 5000, perturber = "He II"), "No data")
})

test_that("electron_density inverts the Stark width", {
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".xml")
  writeLines(xsams, file)
  stark <- parse_xsams(file)
  expect_equal(electron_density(0.015, stark = stark, wavelength = 395, temperature = 5000), 5e16)
  expect_equal(electron_density(c(0.012, 0.024), reference_width = 0.012), c(1e17, 2e17))
  expect_error(electron_density(0.01), "reference_width")
})

test_that("the H-alpha relation of Gigosos et al. is inverted exactly", {
  ne <- c(1e16, 1e17, 5e17)
  width <- 1.098 * (ne / 1e17)^0.67823
  expect_equal(electron_density(width, method = "halpha"), ne)
  expect_error(electron_density(-1, method = "halpha"), "positive")
})

test_that("parse_species checks the notation", {
  expect_equal(parse_species("Ca II")[c("element", "charge")], list(element = "Ca", charge = 1L))
  expect_equal(parse_species("Na I")$charge, 0L)
  expect_error(parse_species("Ca+"), "Roman")
  expect_error(parse_species(c("Ca II", "Na I")), "single")
})

test_that("starkb_lines downloads data from the STARK-B service", {
  skip_on_cran()
  skip_if_offline("stark-b.obspm.fr")
  skip_if_not_installed("xml2")
  ca <- tryCatch(starkb_lines("Ca II", wavelength = c(390, 400), perturber = "electron"),
                 error = function(e) skip(paste("STARK-B unavailable:", conditionMessage(e))))
  expect_true(nrow(ca) > 0)
  expect_true(all(ca$wavelength >= 390 & ca$wavelength <= 400))
  expect_true(all(ca$perturber == "electron"))
  expect_true(all(ca$width > 0))
})

test_that("stark_width scales multiplet data to a line with lambda^2", {
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".xml")
  writeLines(xsams, file)
  stark <- parse_xsams(file)
  w <- stark_width(stark, 393.37, 5000, density = 1e17, tolerance = 2)
  expect_equal(w$tabulated_wavelength, 395)
  expect_equal(w$wavelength, 393.37)
  expect_equal(w$width, 0.030 * (393.37 / 395)^2)
  expect_equal(w$shift, -0.005 * (393.37 / 395)^2)
})

test_that("read_starkb reads a saved XSAMS file and filters it", {
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".xml")
  writeLines(xsams, file)
  expect_equal(read_starkb(file), parse_xsams(file))
  expect_equal(nrow(read_starkb(file, perturber = "H II")), 1)
  expect_equal(nrow(read_starkb(file, wavelength = c(400, 410))), 0)
  expect_error(read_starkb(tempfile()), "existing")
  # a file without level descriptions: labels are empty, matching still works
  no_states <- gsub("<AtomicState.*?</AtomicState>", "", xsams)
  no_states <- gsub("<(Upper|Lower)StateRef>S[12]</(Upper|Lower)StateRef>", "", no_states)
  file2 <- tempfile(fileext = ".xml")
  writeLines(no_states, file2)
  bare <- read_starkb(file2)
  expect_equal(unique(bare$upper), "")
  expect_equal(stark_width(bare, 395, 5000)$width, 0.030)
  bad <- tempfile(fileext = ".xml")
  writeLines("not xml", bad)
  expect_error(read_starkb(bad), "Could not read")
})

test_that("stark_table builds a table from tabulated widths", {
  tab <- stark_table(wavelength = 3950, temperature = c(5000, 20000), width = c(0.30, 0.15),
                     units = "A", species = "Ca II", upper = "4p 2Po", lower = "4s 2S")
  expect_named(tab, c("species", "wavelength", "upper", "lower", "perturber", "temperature",
                      "density", "width", "shift", "source"))
  expect_equal(tab$wavelength, c(395, 395))
  expect_equal(tab$width, c(0.030, 0.015))
  expect_equal(tab$perturber, c("electron", "electron"))
  # same results as the equivalent STARK-B table
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".xml")
  writeLines(xsams, file)
  stark <- parse_xsams(file)
  expect_equal(stark_width(tab, 395, 10000)$width, stark_width(stark, 395, 10000)$width)
  expect_equal(electron_density(0.015, stark = tab, wavelength = 395, temperature = 5000), 5e16)
})

test_that("stark_table evaluates fitted temperature laws", {
  a <- c(1.2, -0.7, 0.02)
  b <- c(-0.1, 0.01, 0)
  temperature <- c(5000, 10000, 20000)
  tab <- stark_table(wavelength = 3950, temperature = temperature, coefficients = a,
                     shift_coefficients = b, units = "A")
  lt <- log10(temperature)
  width_a <- 10^(a[1] + a[2] * lt + a[3] * lt^2)
  expect_equal(tab$width, width_a / 10)
  expect_equal(tab$shift, (b[1] + b[2] * lt) * width_a / 10)
  natural <- stark_table(wavelength = 395, temperature = temperature, coefficients = a,
                         units = "nm", log_base = exp(1))
  expect_equal(natural$width, exp(a[1] + a[2] * log(temperature) + a[3] * log(temperature)^2))
})

test_that("stark_table validates its inputs", {
  expect_error(stark_table(395, 10000, width = 0.02), "'units' is required")
  expect_error(stark_table(395, 10000, units = "nm"), "either 'width' or 'coefficients'")
  expect_error(stark_table(395, 10000, width = 0.02, coefficients = c(1, 0, 0), units = "nm"),
               "not both")
  expect_error(stark_table(395, c(5000, 10000, 20000), width = c(0.1, 0.2), units = "nm"),
               "length 1 or 3")
  expect_error(stark_table(395, 10000, width = -1, units = "nm"), "positive")
  expect_error(stark_table(395, 10000, coefficients = c(1, 2), units = "nm"), "length 3")
})
