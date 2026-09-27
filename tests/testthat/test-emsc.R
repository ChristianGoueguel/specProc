wl <- seq(390, 400, length.out = 120)
pure <- exp(-(wl - 393.4)^2 / 0.05) + 0.6 * exp(-(wl - 396.8)^2 / 0.05)
w <- (wl - 395) / 5
make_emsc_data <- function(n, seed, interferent = NULL) {
  set.seed(seed)
  x <- t(sapply(seq_len(n), function(i) {
    s <- runif(1, 0.5, 2) * pure + runif(1, 0, 1) + runif(1, -1, 1) * w + runif(1, -1, 1) * w^2
    if (!is.null(interferent)) s <- s + runif(1, 0, 2) * interferent
    s
  }))
  colnames(x) <- as.character(wl)
  x
}

test_that("emsc removes multiplicative effects and a polynomial baseline", {
  x <- make_emsc_data(12, 1)
  # with the pure spectrum as reference, every corrected spectrum equals it
  fit <- emsc(x, xref = pure, degree = 2)
  expect_lt(max(abs(sweep(as.matrix(fit$correction), 2, pure))), 1e-10)
  # with the median reference, the corrected spectra are close to each other
  fit <- emsc(x, degree = 2)
  raw_spread <- max(apply(x, 2, sd))
  expect_lt(max(apply(as.matrix(fit$correction), 2, sd)), 0.05 * raw_spread)
  expect_named(fit$coefficients, c("slope", "offset", "poly1", "poly2"))
  expect_s3_class(fit, c("specproc_emsc", "specproc_filter"))
  expect_output(print(fit), "Polynomial degree:    2")
})

test_that("emsc with degree 0 equals msc", {
  x <- make_emsc_data(8, 2)
  expect_equal(emsc(x, degree = 0)$correction, msc(x)$correction)
  expect_equal(emsc(x, degree = 0, robust = FALSE)$correction, msc(x, robust = FALSE)$correction)
})

test_that("emsc subtracts known interferents", {
  interferent <- exp(-(wl - 395)^2 / 0.5)
  x <- make_emsc_data(12, 3, interferent = interferent)
  without <- as.matrix(emsc(x, xref = pure, degree = 2)$correction)
  with <- emsc(x, xref = pure, degree = 2, interferents = interferent)
  expect_named(with$coefficients, c("slope", "offset", "poly1", "poly2", "interferent1"))
  expect_lt(max(apply(as.matrix(with$correction), 2, sd)), 1e-8)
  expect_gt(max(apply(without, 2, sd)), 0.01)
})

test_that("predict() applies the calibration reference to new spectra", {
  x <- make_emsc_data(15, 4)
  fit <- emsc(x[1:10, ], degree = 2)
  expect_equal(predict(fit, x[1:10, ]), fit$correction)
  exact <- emsc(x[1:10, ], xref = pure, degree = 2)
  new <- as.matrix(predict(exact, x[11:15, ]))
  expect_lt(max(abs(sweep(new, 2, pure))), 1e-10)
  # with an explicit reference, emsc() on the new data gives the same result
  expect_equal(predict(fit, x[11:15, ]), emsc(x[11:15, ], xref = fit$reference)$correction)
})

test_that("emsc uses numeric column names or explicit wavelengths", {
  x <- make_emsc_data(8, 5)
  unnamed <- unname(x)
  expect_equal(emsc(x)$correction, emsc(unnamed, wavelength = wl)$correction, ignore_attr = TRUE)
})

test_that("emsc validates its inputs", {
  x <- make_emsc_data(8, 6)
  expect_error(emsc(x, degree = -1), "degree")
  expect_error(emsc(x, xref = 1:3), "xref")
  expect_error(emsc(x, interferents = rep(1, ncol(x))), "rank deficient")
  x[1, 1] <- NA
  expect_error(emsc(x), "missing")
})
