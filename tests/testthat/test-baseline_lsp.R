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
  # the correction and the baseline add up to the spectrum
  expect_equal(as.matrix(res$correction) + as.matrix(res$background), spec$x, ignore_attr = TRUE)
})

test_that("baseline_lsp validates its inputs", {
  expect_error(baseline_lsp(), "Missing 'x' argument.")
  expect_error(baseline_lsp(spec$x, degree = "8"), "'degree' must be a single numeric value.")
  expect_error(baseline_lsp(spec$x, tol = NULL), "'tol must be a single numeric value.")
  expect_error(baseline_lsp(spec$x, max.iter = NULL), "'max.iter' must be a single numeric value.")
  expect_error(baseline_lsp(spec$x, degree = 500), "smaller than the number")
})

test_that("the imodpoly baseline follows the background up to the ends of the spectrum", {
  # a strong line near one end pulls the first fits up: the modpoly baseline
  # then falls far below the background at the other end
  x <- seq(0, 1, length.out = 400)
  bg <- 1000 + 200 * x
  set.seed(1)
  y <- bg + 60000 * exp(-(x - 0.7)^2 / 0.00002) + 20000 * exp(-(x - 0.1)^2 / 0.00002) +
    stats::rnorm(400, sd = 20)
  y <- matrix(y, nrow = 1)
  imod <- unlist(baseline_lsp(y)$background)
  mod <- unlist(baseline_lsp(y, method = "modpoly")$background)
  expect_lt(max(abs(imod - bg)), 60)
  expect_gt(max(abs(mod - bg)), 5 * max(abs(imod - bg)))
  expect_error(baseline_lsp(y, method = "spline"), "should be one of")
})
