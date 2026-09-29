# Two detectors, the second overlapping the first; lines shifted by a known
# linear error on the first detector and a constant one on the second
wc_spectra <- function() {
  wl1 <- seq(300, 500, by = 0.08)
  wl2 <- seq(499, 560, by = 0.08)
  wl <- c(wl1, wl2)
  true <- c(320, 350, 380, 410, 440, 470, 520, 540)
  shift1 <- function(w) -0.05 - 0.0002 * (w - 400)          # measured - true, detector 1
  spec <- function(w, seg) {
    v <- rep(100, length(w))
    for (t in true) {
      m <- if (seg == 1) t + shift1(t) else t + 0.15
      v <- v + 5000 * exp(-(w - m)^2 / (2 * 0.04^2))
    }
    v
  }
  set.seed(1)
  x <- rbind(c(spec(wl1, 1), spec(wl2, 2)), c(spec(wl1, 1), spec(wl2, 2))) +
    matrix(stats::rnorm(2 * length(wl), sd = 2), 2)
  colnames(x) <- wl
  list(x = x, true = true, shift1 = shift1, wl = wl, n1 = length(wl1))
}

test_that("wavelength_calibration recovers the error of each detector", {
  d <- wc_spectra()
  cal <- wavelength_calibration(d$x, d$true, degree = 1)
  expect_s3_class(cal, "specproc_wavelength_calibration")
  expect_equal(cal$segments$segment, 1:2)
  expect_true(all(cal$lines$used))
  expect_equal(cal$lines$segment, c(1, 1, 1, 1, 1, 1, 2, 2))
  expect_equal(cal$lines$offset[1:6], -d$shift1(d$true[1:6]), tolerance = 0.02)
  expect_equal(cal$lines$offset[7:8], c(-0.15, -0.15), tolerance = 0.02)
  expect_lt(max(abs(cal$lines$residual)), 0.005)
  # corrected wavelengths put the lines at their reference positions
  expect_equal(predict(cal, 470 + d$shift1(470)), 470, tolerance = 1e-3)
  expect_equal(predict(cal, 540.15), 540, tolerance = 1e-3)
  fixed <- apply_calibration(d$x, cal)
  recal <- wavelength_calibration(fixed, d$true)
  expect_lt(max(abs(recal$lines$offset)), 0.005)
  expect_output(print(cal), "8 of 8 reference lines")
  expect_s3_class(plot_wavelength_calibration(cal), "ggplot")
})

test_that("wavelength_calibration flags unusable lines", {
  d <- wc_spectra()
  x <- d$x
  # a flat-topped (saturated) line and a missing one
  top <- which(abs(d$wl[seq_len(d$n1)] - (410 + d$shift1(410))) < 0.13)
  x[, top] <- max(x[, top])
  lines <- c(a = 320, b = 350, c = 380, sat = 410, d = 440, e = 470, missing = 455, f = 520, g = 540)
  cal <- wavelength_calibration(x, lines)
  reasons <- stats::setNames(cal$lines$reason, cal$lines$line)
  expect_equal(unname(reasons["sat"]), "flat")
  expect_true(reasons["missing"] %in% c("weak", "edge"))
  expect_true(all(is.na(reasons[c("a", "b", "f", "g")])))
  # a wrongly identified line is rejected as an outlier
  wrong <- c(d$true[1:6] + c(0, 0, 0, 0.12, 0, 0), d$true[7:8])
  expect_equal(sum(wavelength_calibration(d$x, wrong)$lines$reason == "outlier", na.rm = TRUE), 1)
  expect_equal(sum(wavelength_calibration(d$x, wrong, reject = FALSE)$lines$reason == "outlier",
                   na.rm = TRUE), 0)
})

test_that("segments without enough lines get a lower degree or no correction", {
  d <- wc_spectra()
  expect_warning(cal <- wavelength_calibration(d$x, d$true[1:7], degree = 1), "lower degree")
  expect_equal(cal$segments$degree, c(1, 0))
  expect_warning(cal0 <- wavelength_calibration(d$x, d$true[1:6]), "not corrected")
  expect_equal(cal0$segments$degree[2], -1)
  expect_equal(predict(cal0, 530), 530)
  # a single fit across detectors
  full <- wavelength_calibration(d$x, d$true)
  one <- wavelength_calibration(d$x, d$true, segments = FALSE, reject = FALSE)
  expect_equal(nrow(one$segments), 1)
  expect_equal(full$lines$measured, one$lines$measured, tolerance = 1e-6)
})

test_that("apply_calibration keeps the other columns and checks the axis", {
  d <- wc_spectra()
  cal <- wavelength_calibration(d$x, d$true)
  df <- data.frame(id = c("s1", "s2"), d$x, check.names = FALSE)
  out <- apply_calibration(df, cal)
  expect_equal(names(out)[1], "id")
  expect_equal(unname(unlist(out[1, -1])), unname(d$x[1, ]))   # intensities unchanged
  expect_equal(as.numeric(names(out)[-1]), predict(cal, d$wl), tolerance = 1e-4)
  v <- apply_calibration(d$x[1, ], cal)
  expect_equal(names(v), names(out)[-1])
  expect_error(apply_calibration(d$x[, -1], cal), "wavelength axis")
  expect_error(apply_calibration(d$x, list()), "wavelength_calibration")
  expect_error(wavelength_calibration(d$x, d$true, degree = 3), "0, 1 or 2")
})
