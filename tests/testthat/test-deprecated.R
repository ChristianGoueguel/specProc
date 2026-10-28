test_that("old function names are defunct and name the new function", {
  x <- seq(-2, 2, length.out = 20)
  expect_error(lorentzian(x, 0, 0, 1, 1), "'lorentzian\\(\\)' is defunct; use 'lorentzian_profile\\(\\)'")
  expect_error(pseudo_voigt(x, 0, 0, 1, 1, 1), "defunct")
  expect_error(summaryStats(data.frame(a = 1:5)), "summary_stats")
  expect_error(pareto(matrix(1:6, 3)), "pareto_scale")
  expect_error(plotSpec(), "plot_spectra")
  expect_error(boltzmann_plot(), "use 'boltzmann\\(\\)'")
})

test_that("renamed arguments still work, with a deprecation warning", {
  rlang::local_options(lifecycle_verbosity = "warning")
  x <- c(1, 2, NA, 4, 10)
  expect_warning(z <- zscore(x, drop.na = TRUE), "drop_na")
  expect_equal(z, zscore(x, drop_na = TRUE))
  m <- matrix(stats::rnorm(40, 10), 4)
  expect_warning(s <- snv(m, drop.na = FALSE), "drop_na")
  expect_equal(s, snv(m, drop_na = FALSE))
  expect_warning(b <- baseline_als(m, max.iter = 5), "max_iter")
  expect_equal(b, baseline_als(m, max_iter = 5))
  expect_no_warning(baseline_als(m, max_iter = 5))
})

test_that("gaussian no longer masks stats::gaussian for glm users", {
  expect_false("gaussian" %in% getNamespaceExports("specProc"))
  expect_s3_class(stats::gaussian(), "family")
})
