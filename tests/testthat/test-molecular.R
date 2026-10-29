test_that("air and vacuum wavelengths follow the dispersion of the NIST ASD", {
  expect_equal(vacuum_to_air(500), 500 / (1 + 2.7896e-4), tolerance = 1e-7)
  expect_equal(vacuum_to_air(c(150, 2500)), c(150, 2500))
  expect_equal(air_to_vacuum(vacuum_to_air(c(250, 388, 800))), c(250, 388, 800), tolerance = 1e-10)
})

test_that("molecular_bands reproduces measured band heads", {
  heads <- molecular_bands(c("CN", "C2", "N2", "N2+", "AlO"), wavelength = c(300, 600),
                           min_relative = 0)
  # measured heads (nm, air), Pearse and Gaydon (1976)
  measured <- data.frame(
    molecule = c("CN", "CN", "CN", "CN", "CN", "C2", "C2", "C2", "C2", "N2", "N2", "N2", "N2",
                 "N2+", "N2+", "AlO", "AlO", "AlO"),
    system = c(rep("B-X", 5), rep("d-a", 4), rep("C-B", 4), "B-X", "B-X", rep("B-X", 3)),
    v_upper = c(0, 1, 2, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0),
    v_lower = c(0, 1, 2, 1, 0, 0, 1, 0, 1, 0, 1, 0, 2, 0, 1, 0, 0, 1),
    head = c(388.34, 387.14, 386.19, 421.60, 359.04, 516.52, 512.93, 473.71, 563.55, 337.13,
             357.69, 315.93, 380.49, 391.44, 427.81, 484.22, 464.82, 507.93)
  )
  found <- merge(measured, heads, by = c("molecule", "system", "v_upper", "v_lower"))
  expect_equal(nrow(found), nrow(measured))
  expect_true(all(abs(found$wavelength - found$head.x) < 0.06))
  expect_true(all(found$head.y))
  cn <- heads[heads$molecule == "CN" & heads$v_upper == 0 & heads$v_lower == 0, ]
  expect_equal(cn$degraded, "violet")
  expect_gt(cn$wavelength, cn$origin)          # head on the red side of a violet-degraded band
  alo <- heads[heads$molecule == "AlO" & heads$v_upper == 0 & heads$v_lower == 0, ]
  expect_equal(alo$degraded, "red")
  expect_lt(alo$wavelength, alo$origin)
  # Franck-Condon factors (0-0 of CN violet: 0.918; C2 Swan: 0.72)
  expect_equal(cn$fcf, 0.918, tolerance = 0.01)
  expect_equal(heads$fcf[heads$molecule == "C2" & heads$system == "d-a" & heads$v_upper == 0 &
                           heads$v_lower == 0], 0.72, tolerance = 0.03)
})

test_that("molecular_bands normalizes, filters and handles incomplete constants", {
  bands <- molecular_bands()
  expect_true(all(c("CN", "OH", "TiO", "CaCl") %in% bands$molecule))
  peak <- tapply(bands$relative_intensity, paste(bands$molecule, bands$system), max)
  expect_true(all(abs(peak - 1) < 1e-12 | peak < 1))   # filtering may drop the strongest band
  expect_false(is.unsorted(bands$wavelength))
  narrow <- molecular_bands("CN", wavelength = c(385, 390))
  expect_true(all(narrow$wavelength >= 385 & narrow$wavelength <= 390))
  expect_true(all(narrow$relative_intensity >= 0.01))
  # hydride bands without a head: the band is at its origin
  nh <- molecular_bands("NH", wavelength = c(300, 400))
  expect_false(any(nh$head))
  expect_equal(nh$wavelength, nh$origin)
  # no rotational constants: only v' = v'', without Franck-Condon factor
  cacl <- molecular_bands("CaCl", min_relative = 0)
  expect_true(all(is.na(cacl$fcf)))
  expect_equal(cacl$v_upper, cacl$v_lower)
  expect_equal(sort(unique(cacl$upper)), c("A 2Pi1/2", "A 2Pi3/2", "B 2Sigma+"))
  # hot bands grow with the temperature
  hot <- function(t) {
    b <- molecular_bands("CN", wavelength = c(385, 390), temperature = t, min_relative = 0)
    b$relative_intensity[b$v_upper == 2 & b$v_lower == 2]
  }
  expect_gt(hot(9000), hot(3000))
  expect_error(molecular_bands("XY"), "Unknown molecule")
  expect_error(molecular_bands("CN", wavelength = 400), "wavelength")
  expect_error(molecular_bands("CN", temperature = -1), "temperature")
})

test_that("the vibrational wavefunctions are orthonormal and complete", {
  r <- seq(0.4, 3, length.out = 6000)
  morse <- vibrational_wavefunctions(r, list(we = 2068.59, wexe = 13.087, re = 1.17182), 6.46, 6)
  harmonic <- vibrational_wavefunctions(r, list(we = 1794.9, wexe = NA, re = 1.1975), 0.93, 6)
  expect_equal(crossprod(morse), diag(7), tolerance = 1e-6)
  expect_equal(crossprod(harmonic), diag(7), tolerance = 1e-6)
  row <- molecular_constants()[1, ]
  q <- franck_condon(row, molecular_state(row, "u"), molecular_state(row, "l"))
  expect_equal(rowSums(q)[1:3], rep(1, 3), tolerance = 1e-3)
})

test_that("plot_lines marks molecular bands", {
  data(forageLIBS, package = "specProc", envir = environment())
  spectrum <- colMeans(forageLIBS[-(1:14)])
  wl <- as.numeric(names(spectrum))
  spectrum <- spectrum[wl > 383 & wl < 390]
  bands <- molecular_bands("CN")
  p <- plot_lines(spectrum, bands = bands, interactive = FALSE)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[3]]), sum(bands$wavelength > 383 & bands$wavelength < 390))
  skip_if_not_installed("plotly")
  pi <- plotly::plotly_build(plot_lines(spectrum, bands = bands))
  names <- vapply(pi$x$data, function(t) t$name %||% "", character(1))
  expect_true("CN bands" %in% names)
  expect_error(plot_lines(spectrum), "lines")
  expect_error(plot_lines(spectrum, bands = data.frame(a = 1)), "molecular_bands")
})

test_that("the line finder overlays the selected molecular bands", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("bslib")
  wl <- seq(380, 400, by = 0.05)
  x <- matrix(1, 2, length(wl), dimnames = list(NULL, wl))
  app <- line_finder_app(x, fetch = function(species, wavelength) empty_line_list())
  shiny::testServer(app, {
    session$setInputs(spectrum = "mean", stages = "1", temperature = 10000, top = 5,
                      min_relative = 0.01, wl_range = c(380, 400), shift = 0, scale = TRUE,
                      molecules = list("CN"), band_temperature = 5000)
    expect_true(all(bands()$molecule == "CN"))
    expect_match(output$n_lines, "bands")
    session$setInputs(molecules = list())
    expect_null(bands())
  })
})

# A fake ExoMol line list: three levels, two lines near 400 nm.
fake_exomol <- function(dir) {
  con <- bzfile(file.path(dir, "1X-1Y__test.states.bz2"), "w")
  writeLines(c("           1     0.000000      2     0.5  X(2SIGMA+)",
               "           2 25000.000000      4     1.5  B(2SIGMA+)",
               "           3 25100.000000      4     1.5  B(2SIGMA+)"), con)
  close(con)
  con <- bzfile(file.path(dir, "1X-1Y__test.trans.bz2"), "w")
  writeLines(c("           2           1 1.0000E+07", "           3           1 2.0000E+07"), con)
  close(con)
}

test_that("molecular_spectrum computes LTE line intensities from a line list", {
  dir <- tempfile("exomol")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fake_exomol(dir)
  sticks <- molecular_spectrum("XY", temperature = 5000, wavelength = c(395, 402),
                               isotopologue = "1X-1Y", dataset = "test", cache = dir, lines = TRUE)
  expect_equal(sticks$wavelength, vacuum_to_air(1e7 / c(25100, 25000)))
  ratio <- 2 * 25100 / 25000 * exp(-1.438777 * 100 / 5000)
  expect_equal(sticks$intensity, c(1, 1 / ratio))
  expect_equal(sticks$J_upper, c(1.5, 1.5))
  photons <- molecular_spectrum("XY", temperature = 5000, wavelength = c(395, 402),
                                isotopologue = "1X-1Y", dataset = "test", cache = dir, lines = TRUE,
                                units = "photons")
  expect_equal(photons$intensity[2], 1 / (2 * exp(-1.438777 * 100 / 5000)))
  spec <- molecular_spectrum("XY", temperature = 5000, wavelength = c(395, 402), resolution = 0.1,
                             isotopologue = "1X-1Y", dataset = "test", cache = dir)
  expect_named(spec, c("wavelength", "intensity"))
  expect_equal(max(spec$intensity), 1)
  expect_equal(spec$wavelength[which.max(spec$intensity)], sticks$wavelength[1], tolerance = 1e-4)
  # the profile has the requested width
  above <- spec$wavelength[spec$intensity >= 0.5 & spec$wavelength < 399]
  expect_equal(diff(range(above)), 0.1, tolerance = 0.1)
  expect_equal(attr(spec, "dataset"), "test")
  expect_error(molecular_spectrum("XY", wavelength = c(500, 510), isotopologue = "1X-1Y",
                                  dataset = "test", cache = dir), "no line")
  expect_error(molecular_spectrum("XY", wavelength = 400), "wavelength")
  expect_error(molecular_spectrum(c("CN", "C2"), wavelength = c(380, 390)), "one molecule")
})

test_that("the ExoMol API listing is parsed into line lists", {
  api <- list(`(12C)(14N)` = list(molecule = "CN", linelist = list(
    `data type` = "line list",
    `MoLLIST-CN` = list(recommended = FALSE, files = list(
      list(url = "exomol.com/db/CN/12C-14N/MoLLIST-CN/12C-14N__MoLLIST-CN.states.bz2", size = 61460),
      list(url = "exomol.com/db/CN/12C-14N/MoLLIST-CN/12C-14N__MoLLIST-CN.trans.bz2", size = 1101752),
      list(url = "exomol.com/db/CN/12C-14N/MoLLIST-CN/CN_README__MoLLIST-CN.txt"))),
    Trihybrid = list(recommended = TRUE, files = list(
      list(url = "exomol.com/db/CN/12C-14N/Trihybrid/12C-14N__Trihybrid.states.bz2"),
      list(url = "exomol.com/db/CN/12C-14N/Trihybrid/12C-14N__Trihybrid.trans.bz2", size = 27018411))))))
  sets <- exomol_parse_api(api, "CN")
  expect_equal(sets$dataset, c("MoLLIST-CN", "Trihybrid"))
  expect_equal(sets$isotopologue, c("12C-14N", "12C-14N"))
  expect_equal(sets$trans_mb, c(1.101752, 27.018411))
  expect_true(is.na(sets$states_mb[2]))
  expect_true(all(startsWith(sets$urls[[1]], "https://exomol.com/db/CN/")))
  expect_error(exomol_parse_api(list(), "XY"), "no line list")
})

test_that("molecular_spectrum reproduces the CN violet head from ExoMol", {
  skip_on_cran()
  skip_if_offline("exomol.com")
  cn <- tryCatch(molecular_spectrum("CN", temperature = 6000, wavelength = c(386, 389)),
                 error = function(e) skip(paste("ExoMol unavailable:", conditionMessage(e))))
  near <- cn$wavelength > 387.8 & cn$wavelength < 388.6
  expect_equal(cn$wavelength[near][which.max(cn$intensity[near])], 388.33, tolerance = 2e-4)
  expect_equal(attr(cn, "dataset"), "MoLLIST-CN")
})
