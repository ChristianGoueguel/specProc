spec <- make_spectra(n = 4, p = 150)

test_that("baseline_als (ALS) matches a dense reference implementation", {
  res <- baseline_als(spec$x, lambda = 1e4, p = 0.01, max.iter = 20)
  expect_named(res, c("correction", "background"))
  expect_s3_class(res$correction, "tbl_df")
  expect_equal(dim(res$background), dim(spec$x))
  expect_equal(names(res$background), colnames(spec$x))
  for (i in 1:2) {
    ref <- dense_als(spec$x[i, ], 1e4, 0.01, 20)
    expect_equal(unlist(res$background[i, ], use.names = FALSE), ref, tolerance = 1e-8)
  }
  expect_equal(as.matrix(res$correction) + as.matrix(res$background), spec$x,
               ignore_attr = TRUE)
})

test_that("baseline_arpls matches a dense reference implementation", {
  res <- baseline_arpls(spec$x, lambda = 1e4, ratio = 0.05, max.iter = 20)
  for (i in 1:2) {
    ref <- dense_arpls(spec$x[i, ], 1e4, 0.05, 20)
    expect_equal(unlist(res$background[i, ], use.names = FALSE), ref, tolerance = 1e-7)
  }
})

test_that("penalized baselines recover a smooth continuum under emission lines", {
  # noise sd = 0.5; ALS sits slightly below the noise, arPLS in its middle
  res <- baseline_als(spec$x, lambda = 1e3, p = 0.01, max.iter = 30)
  err <- abs(as.matrix(res$background) - spec$background)
  expect_lt(stats::median(err), 1)
  res2 <- baseline_arpls(spec$x, lambda = 1e3, max.iter = 30)
  err2 <- abs(as.matrix(res2$background) - spec$background)
  expect_lt(stats::median(err2), 0.2)
})

test_that("negative corrected values are not clipped", {
  res <- baseline_als(spec$x, lambda = 1e4, p = 0.01)
  expect_true(any(as.matrix(res$correction) < 0))
})

test_that("baseline functions accept data frames and vectors-as-rows", {
  df <- as.data.frame(spec$x)
  expect_equal(baseline_als(df)$background, baseline_als(spec$x)$background)
  expect_equal(baseline_lsp(df)$background, baseline_lsp(spec$x)$background)
  one <- baseline_als(spec$x[1, , drop = FALSE])
  expect_equal(nrow(one$correction), 1)
})

test_that("baseline functions validate their inputs", {
  expect_error(baseline_als(), "Missing 'x' argument.")
  expect_error(baseline_als(spec$x, p = 1.5), "'p' must be between 0 and 1")
  expect_error(baseline_als(spec$x, lambda = "a"), "'lambda' must be a single numeric value.")
  expect_error(baseline_als(spec$x, lambda = -1), "'lambda' must be positive.")
  expect_error(baseline_arpls(), "Missing 'x' argument.")
  expect_error(baseline_arpls(spec$x, ratio = c(1, 2)), "'ratio' must be a single numeric value.")
  bad <- spec$x
  bad[1, 1] <- NA
  expect_error(baseline_als(bad), "missing values")
  expect_error(baseline_als(data.frame(a = letters[1:3])), "numeric")
})

test_that("banded solver scales to many channels", {
  set.seed(1)
  x <- matrix(stats::rnorm(5 * 20000), 5)
  expect_no_error(res <- baseline_arpls(x, lambda = 1e6))
  expect_equal(dim(res$background), c(5L, 20000L))
})
