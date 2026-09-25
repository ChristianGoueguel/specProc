spec <- make_spectra(n = 3, p = 120)

test_that("baseline_lsp removes a polynomial background below positive peaks", {
  x <- seq(0, 1, length.out = 300)
  bg <- 10 + 5 * x - 3 * x^2
  y <- bg + 40 * exp(-(x - 0.4)^2 / 0.0005) + 30 * exp(-(x - 0.7)^2 / 0.0005)
  res <- baseline_lsp(matrix(y, nrow = 1), degree = 2, tol = 1e-8, max.iter = 500)
  expect_lt(max(abs(unlist(res$background) - bg)) / diff(range(bg)), 0.5)
  expect_lt(stats::median(abs(unlist(res$background) - bg)), 0.3)
})

test_that("baseline_lsp returns the documented structure", {
  res <- baseline_lsp(spec$x)
  expect_named(res, c("correction", "background"))
  expect_equal(dim(res$correction), dim(spec$x))
  expect_equal(names(res$correction), colnames(spec$x))
  # the fitted baseline never exceeds the spectrum by construction of the iterations
  expect_equal(as.matrix(res$correction) + as.matrix(res$background), spec$x, ignore_attr = TRUE)
})

test_that("baseline_lsp validates its inputs", {
  expect_error(baseline_lsp(), "Missing 'x' argument.")
  expect_error(baseline_lsp(spec$x, degree = "8"), "'degree' must be a single numeric value.")
  expect_error(baseline_lsp(spec$x, tol = NULL), "'tol must be a single numeric value.")
  expect_error(baseline_lsp(spec$x, max.iter = NULL), "'max.iter' must be a single numeric value.")
  expect_error(baseline_lsp(spec$x, degree = 500), "smaller than the number")
})
