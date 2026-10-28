# A calibration lamp (blackbody at 3100 K, certified every 10 nm) seen
# through an instrument of known response.
planck_r <- function(wl, t = 3100) 1 / (wl^5 * (exp(1.4388e7 / (wl * t)) - 1))
true_response <- function(wl) exp(-((wl - 500) / 250)^2)
lamp_reference <- function(rsd = 0.03, from = 250, to = 850) {
  d <- data.frame(wavelength = seq(from, to, by = 10))
  d$value <- planck_r(d$wavelength)
  d$rsd <- rsd
  d
}
lamp_spectra <- function(wl, n = 5, sd = 2, seed = 1, response = true_response(wl)) {
  set.seed(seed)
  m <- t(replicate(n, 2e4 * response * planck_r(wl) / planck_r(500) +
                     stats::rnorm(length(wl), sd = sd)))
  colnames(m) <- wl
  m
}

test_that("response_calibration recovers the response of the instrument", {
  wl <- seq(260, 840, by = 0.1)
  r <- response_calibration(lamp_spectra(wl), lamp_reference())
  expect_s3_class(r, "specproc_response")
  expect_equal(r$response$wavelength, wl)
  expect_equal(r$response$response, true_response(wl), tolerance = 0.005)
  expect_equal(max(r$response$response), 1)
  expect_equal(r$response$reference_rsd, rep(0.03, length(wl)))
  # the window is one-sided at the ends of the axis, where the lamp is weakest
  expect_true(all(r$response$measurement_rsd < 0.02))
  expect_lt(stats::median(r$response$measurement_rsd), 0.001)
  expect_equal(r$response$rsd, sqrt(0.03^2 + r$response$measurement_rsd^2))
  expect_equal(r$n_spectra, 5)
  expect_output(print(r), "5 lamp spectra")
  p <- plot_response(r)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "Spectral response")
  # one spectrum, as a named vector, without smoothing
  one <- response_calibration(lamp_spectra(wl, n = 1)[1, ], lamp_reference(), smooth = 0)
  expect_equal(one$response$response, true_response(wl), tolerance = 0.01)
  expect_output(print(one), "1 lamp spectrum")
})

test_that("the measurement uncertainty matches the spread of repeated calibrations", {
  wl <- seq(300, 800, by = 0.2)
  at <- c(which.min(abs(wl - 400)), which.min(abs(wl - 700)))
  for (n in c(1, 4)) {
    reps <- vapply(1:40, function(seed) {
      r <- response_calibration(lamp_spectra(wl, n = n, sd = 50, seed = seed), lamp_reference(0))
      resp <- r$response
      c(log(resp$response[at[1]] / resp$response[at[2]]), sqrt(sum(resp$measurement_rsd[at]^2)))
    }, numeric(2))
    # the response ratio of two channels is free of the arbitrary scaling
    expect_equal(stats::sd(reps[1, ]), mean(reps[2, ]), tolerance = 0.3, label = paste("n =", n))
  }
})

test_that("response_calibration subtracts the dark, excludes ranges and splits detectors", {
  wl <- seq(300, 800, by = 0.1)
  lamp <- lamp_spectra(wl)
  plain <- response_calibration(lamp, lamp_reference())
  dark <- matrix(500, 2, length(wl), dimnames = list(NULL, wl))
  expect_equal(response_calibration(lamp + 500, lamp_reference(), dark = dark)$response$response,
               plain$response$response)
  # a lamp emission line (the Balmer beta line of deuterium) left out
  balmer <- lamp + rep(1e5 * exp(-(wl - 486)^2 / 0.02), each = nrow(lamp))
  near <- abs(wl - 486) < 0.5
  # the response relative to that at 600 nm, against the true one (the
  # line may become the maximum, to which the response is scaled)
  shape <- function(r) {
    at <- which.min(abs(wl - 600))
    (r$response$response[near] / r$response$response[at]) /
      (true_response(wl[near]) / true_response(wl[at]))
  }
  expect_gt(max(shape(response_calibration(balmer, lamp_reference()))), 1.05)
  clean <- response_calibration(balmer, lamp_reference(), exclude = list(c(485, 487)))
  expect_equal(shape(clean), rep(1, sum(near)), tolerance = 0.005)
  expect_equal(clean$response$response[near], true_response(wl[near]), tolerance = 0.005)
  expect_output(print(clean), "Excluded: +485 to 487 nm")
  # two detectors, with different responses, that overlap
  wl1 <- seq(300, 560, by = 0.1)
  wl2 <- seq(550.05, 800, by = 0.1)
  both <- cbind(lamp_spectra(wl1), lamp_spectra(wl2, response = 0.5 * true_response(wl2)))
  r <- response_calibration(both, lamp_reference())
  expect_equal(r$response$segment, rep(1:2, c(length(wl1), length(wl2))))
  expect_equal(r$response$response, c(true_response(wl1), 0.5 * true_response(wl2)),
               tolerance = 0.005)
})

test_that("response_calibration has no response outside the reference", {
  wl <- seq(240, 860, by = 0.5)
  expect_warning(r <- response_calibration(lamp_spectra(wl), lamp_reference()), "outside the range")
  out <- wl < 250 | wl > 850
  expect_true(all(is.na(r$response$response[out])))
  expect_true(all(is.na(r$response$rsd[out])))
  expect_false(anyNA(r$response$response[!out]))
  expect_output(print(r), "No response")
  expect_s3_class(plot_response(r), "ggplot")
})

test_that("response_calibration validates its inputs", {
  wl <- seq(300, 800, by = 1)
  lamp <- lamp_spectra(wl, n = 1)
  expect_error(response_calibration(lamp, data.frame(wavelength = 1:3)), "`value`")
  expect_error(response_calibration(lamp, data.frame(wavelength = 400, value = 1)), "2 distinct")
  bad <- lamp_reference()
  bad$rsd[2] <- -1
  expect_error(response_calibration(lamp, bad), "`rsd`")
  expect_error(response_calibration(lamp, lamp_reference(), exclude = c(500, 400)), "exclude")
  expect_error(response_calibration(lamp, lamp_reference(), dark = lamp[, -1, drop = FALSE]),
               "wavelengths of 'lamp'")
  expect_error(response_calibration(lamp, lamp_reference(), smooth = -1), "smooth")
  expect_error(plot_response(list()), "response_calibration")
})

test_that("correct_response corrects spectra on the axis of the response", {
  wl <- seq(300, 800, by = 0.5)
  r <- response_calibration(lamp_spectra(wl), lamp_reference())
  m <- lamp_spectra(wl, n = 2)
  corrected <- correct_response(m, r)
  expect_equal(corrected, sweep(m, 2, r$response$response, "/"))
  v <- correct_response(m[1, ], r)
  expect_equal(unname(v), unname(corrected[1, ]))
  d <- data.frame(Sample = c("a", "b"), m, check.names = FALSE)
  cd <- correct_response(d, r)
  expect_equal(cd$Sample, c("a", "b"))
  expect_equal(as.matrix(cd[-1]), corrected)
  expect_error(correct_response(m[, -1], r), "wavelength axis")
  expect_error(correct_response(m, list()), "response_calibration")
})

test_that("correct_response corrects line intensities and their standard errors", {
  wl <- seq(300, 800, by = 0.1)
  r <- response_calibration(lamp_spectra(wl), lamp_reference())
  emission <- 1000 * exp(-(wl - 350)^2 / 0.01) + 1000 * exp(-(wl - 700)^2 / 0.01)
  set.seed(2)
  spectrum <- stats::setNames(true_response(wl) * emission + stats::rnorm(length(wl)), wl)
  lines <- line_intensities(spectrum, c(a = 350, b = 700))
  out <- correct_response(lines, r)
  peak <- match(lines$peak_wavelength, wl)
  expect_equal(out$response, r$response$response[peak])
  expect_equal(out$intensity, lines$intensity / out$response)
  expect_equal(out$height, lines$height / out$response)
  expect_equal(out$intensity_se / out$intensity,
               sqrt((lines$intensity_se / lines$intensity)^2 + r$response$rsd[peak]^2))
  # equal emission, equal corrected intensities
  expect_equal(out$intensity[1], out$intensity[2], tolerance = 0.02)
  # without intensity_se, the response alone
  plain <- data.frame(wavelength = c(350, 700), intensity = c(10, 20))
  pc <- correct_response(plain, r)
  idx <- vapply(c(350, 700), function(w) which.min(abs(wl - w)), integer(1))
  expect_equal(pc$intensity_se, pc$intensity * r$response$rsd[idx])
  # a line beyond the axis
  expect_warning(far <- correct_response(data.frame(wavelength = 900, intensity = 1), r),
                 "no response")
  expect_true(is.na(far$intensity))
})
