# Two Gaussian lines on a sloped background, sampled every 0.02 nm
li_spectra <- function() {
  wl <- seq(390, 400, by = 0.02)
  line <- function(center, height) height * exp(-(wl - center)^2 / (2 * 0.03^2))
  base <- 100 + 2 * (wl - 390)
  x <- rbind(base + line(393.40, 1000) + line(396.85, 500),
             base + line(393.42, 2000) + line(396.87, 1000))
  colnames(x) <- wl
  data.frame(Sample = c("a", "b"), x, check.names = FALSE)
}

test_that("line_intensities finds peaks and measures areas and heights", {
  d <- li_spectra()
  lines <- c(`Ca II 393` = 393.37, `Ca II 397` = 396.85)
  out <- line_intensities(d, lines, baseline = TRUE, half_width = 0.15)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 4)
  expect_equal(out$Sample, c("a", "a", "b", "b"))
  expect_equal(out$line, rep(names(lines), 2))
  expect_equal(out$peak_wavelength, c(393.40, 396.84, 393.42, 396.88), tolerance = 0.011)
  expect_equal(out$shift, out$peak_wavelength - out$wavelength)
  # Gaussian area = height * sigma * sqrt(2 pi), the window holds 5 sigma
  expect_equal(out$intensity[1], 1000 * 0.03 * sqrt(2 * pi), tolerance = 0.02)
  expect_equal(out$intensity[3] / out$intensity[1], 2, tolerance = 0.01)
  expect_true(all(out$detected))
  expect_true(all(is.na(out$saturated)))
  h <- line_intensities(d, lines, method = "height", baseline = TRUE)
  expect_equal(h$intensity[c(1, 3)], c(1000, 2000), tolerance = 0.01)
  # without baseline, the background adds to the area
  raw <- line_intensities(d, lines)
  expect_gt(raw$intensity[1], out$intensity[1] + 100 * 0.25)
  # saturation
  sat <- line_intensities(d, lines, limit = 1500)
  expect_equal(sat$saturated, c(FALSE, FALSE, TRUE, FALSE))
})

test_that("line_intensities takes a single spectrum, a matrix and a table of lines", {
  d <- li_spectra()
  one <- unlist(d[1, -1])
  table <- data.frame(wavelength = c(393.37, 396.85), Aki = c(1.47e8, 1.4e8), gk = c(4, 2))
  out <- line_intensities(one, table, baseline = TRUE)
  expect_equal(names(out)[1:3], c("wavelength", "Aki", "gk"))
  # a measured column replaces a column of the lines
  nist_like <- transform(table, intensity = c("1000", "500"))
  expect_type(line_intensities(one, nist_like)$intensity, "double")
  expect_equal(nrow(out), 2)
  expect_false("spectrum" %in% names(out))
  m <- line_intensities(as.matrix(d[-1]), 393.37)
  expect_equal(m$spectrum, 1:2)
  expect_equal(m$line, rep("393.37", 2))
  # a line outside the range gives NA
  out2 <- line_intensities(one, c(393.37, 500))
  expect_true(is.na(out2$intensity[2]))
  expect_false(out2$detected[2])
})

test_that("line_intensities fits Voigt profiles", {
  d <- li_spectra()
  v <- line_intensities(d, c(393.37, 396.85), method = "voigt", fit_width = 0.4)
  expect_equal(v$intensity[1], 1000 * 0.03 * sqrt(2 * pi), tolerance = 0.03)
  expect_warning(line_intensities(d, 393.37, method = "voigt", fit_width = 0.04), "Fewer than 6")
})

test_that("line_intensities checks its inputs", {
  d <- li_spectra()
  expect_error(line_intensities(d, "a"), "numeric vector")
  expect_error(line_intensities(d, data.frame(x = 1)), "wavelength")
  expect_error(line_intensities(c(1, 2, 3), 393), "named")
  expect_error(line_intensities(data.frame(a = 1:3), 393), "wavelengths")
  expect_error(line_intensities(d, 393, method = "foo"))
  expect_error(line_intensities(d, 393, half_width = 0), "half_width")
})

test_that("step_line_intensities replaces spectra by line intensities", {
  skip_if_not_installed("recipes")
  d <- li_spectra()
  lines <- c(Ca1 = 393.37, Ca2 = 396.85)
  rec <- recipes::recipe(~ ., data = d) |>
    step_line_intensities(recipes::all_numeric(), lines = lines, baseline = TRUE)
  prepped <- recipes::prep(rec)
  out <- recipes::bake(prepped, new_data = NULL)
  expect_named(out, c("Sample", "line_Ca1", "line_Ca2"))
  expected <- line_intensities(d, lines, baseline = TRUE)$intensity
  expect_equal(c(out$line_Ca1, out$line_Ca2), expected[c(1, 3, 2, 4)])
  kept <- recipes::recipe(~ ., data = d) |>
    step_line_intensities(recipes::all_numeric(), lines = 393.37, prefix = "I_",
                          keep_original_cols = TRUE) |>
    recipes::prep() |> recipes::bake(new_data = NULL)
  expect_true(all(c("I_393.37", "393.4") %in% names(kept)))
  expect_equal(recipes::tidy(prepped, 1)$line, c("line_Ca1", "line_Ca2"))
  expect_match(paste(utils::capture.output(print(rec), type = "message"), collapse = " "),
               "2 emission lines")
  expect_equal(nrow(generics::tunable(rec$steps[[1]])), 0)
  expect_error(recipes::recipe(~ ., data = d) |> step_line_intensities(recipes::all_numeric()),
               "lines")
  expect_error(recipes::recipe(~ ., data = d) |>
                 step_line_intensities(recipes::all_numeric(), lines = 393, method = "voigt"))
})
