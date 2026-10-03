# Spectra with an informative line (channels 41-50, intensity proportional
# to y), a strong interfering line (channels 81-90) unrelated to y, a
# sloping baseline and noise.
make_selection_data <- function(n = 80, p = 120, seed = 1) {
  set.seed(seed)
  y <- stats::runif(n)
  ch <- seq_len(p)
  profile <- function(center) exp(-(ch - center)^2 / 4)
  x <- outer(stats::runif(n, 5, 10), ch / p) +
    outer(50 * y, profile(45.5)) +
    outer(stats::runif(n, 0, 200), profile(85.5)) +
    matrix(stats::rnorm(n * p, sd = 1), n, p)
  colnames(x) <- format(seq(400, 410, length.out = p), nsmall = 3, trim = TRUE)
  list(x = x, y = y + stats::rnorm(n, sd = 0.02), line = 41:50)
}

test_that("VIP reproduces the NIPALS definition", {
  d <- make_selection_data()
  sel <- select_wavelengths(d$x, d$y, "vip", ncomp = 3, num_terms = 10)
  ref <- pls::plsr(d$y ~ d$x, ncomp = 3, method = "oscorespls")
  w <- unclass(ref$loading.weights)
  ssy <- drop(ref$Yloadings)^2 * colSums(unclass(ref$scores)^2)
  vip <- sqrt(ncol(d$x) * drop(w^2 %*% ssy) / sum(ssy))
  expect_equal(sel$importance, vip, tolerance = 1e-8, ignore_attr = TRUE)
  # the squared VIP values average 1
  expect_equal(mean(sel$importance^2), 1)
})

test_that("VIP agrees with mixOmics", {
  skip_on_cran()
  skip_if_not_installed("mixOmics")
  d <- make_selection_data(seed = 2)
  sel <- select_wavelengths(d$x, d$y, "vip", ncomp = 4, num_terms = 10)
  ref <- mixOmics::vip(mixOmics::pls(d$x, d$y, ncomp = 4, scale = FALSE))[, 4]
  expect_equal(unname(sel$importance), unname(ref), tolerance = 1e-8)
})

test_that("the selectivity ratio follows its definition", {
  d <- make_selection_data(seed = 3)
  sel <- select_wavelengths(d$x, d$y, "sr", ncomp = 3, num_terms = 10)
  fit <- pls::plsr(d$y ~ d$x, ncomp = 3, method = "simpls")
  b <- drop(coef(fit, ncomp = 3))
  xc <- sweep(d$x, 2, colMeans(d$x))
  t <- drop(xc %*% b) / sqrt(sum(b^2))
  explained <- t %o% drop(crossprod(xc, t) / sum(t^2))
  sr <- colSums(explained^2) / colSums((xc - explained)^2)
  expect_equal(sel$importance, sr, tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("VIP and SR select the informative line", {
  d <- make_selection_data(seed = 4)
  sr <- select_wavelengths(d$x, d$y, "sr", ncomp = 3, num_terms = 6)
  expect_length(sr$selected, 6L)
  expect_true(all(match(sr$selected, colnames(d$x)) %in% d$line))
  expect_s3_class(plot_wavelength_selection(sr), "ggplot")
  # the strong interfering line gets a large VIP, but not a large SR
  vip <- select_wavelengths(d$x, d$y, "vip", ncomp = 3, num_terms = 6)
  expect_true(any(match(vip$selected, colnames(d$x)) %in% 81:90))
  expect_true(all(sr$importance[81:90] < 1))
  expect_s3_class(plot_wavelength_selection(vip), "ggplot")
  # VIP > 1 by default; SR needs num_terms or threshold
  vip <- select_wavelengths(d$x, d$y, "vip", ncomp = 3)
  expect_equal(vip$selected, names(which(vip$importance > 1)))
  expect_error(select_wavelengths(d$x, d$y, "sr"), "no general threshold")
  sr <- select_wavelengths(d$x, d$y, "sr", threshold = 2)
  expect_equal(sr$selected, names(which(sr$importance > 2)))
  expect_output(print(sr), "SR > 2")
})

test_that("backward elimination removes variables in rounds", {
  d <- make_selection_data(seed = 5)
  sel <- select_wavelengths(d$x, d$y, "sr", ncomp = 3, num_terms = 8, recursive = TRUE, prop_drop = 0.5)
  expect_length(sel$selected, 8L)
  expect_equal(sel$path$variables, c(120L, 60L, 30L, 15L, 8L))
  expect_true(all(match(sel$selected, colnames(d$x)) %in% d$line))
  expect_output(print(sel), "backward elimination in 4 rounds")
  expect_error(select_wavelengths(d$x, d$y, "vip", recursive = TRUE), "needs `num_terms`")
})

test_that("iPLS selects the interval of the informative line first", {
  d <- make_selection_data(seed = 6)
  sel <- select_wavelengths(d$x, d$y, "ipls", ncomp = 3, intervals = 12)
  expect_equal(sel$path$interval[1], 5L)
  expect_equal(sum(sel$intervals$size), 120L)
  expect_true(all(diff(sel$path$RMSECV) < 0))
  expect_equal(sel$selected, colnames(d$x)[sel$intervals$step[ceiling(seq_len(120) / 10)] %in% sel$path$step])
  expect_equal(unname(sel$importance[41]), sel$intervals$RMSECV[5])
  fixed <- select_wavelengths(d$x, d$y, "ipls", ncomp = 3, intervals = 12, num_intervals = 3)
  expect_equal(nrow(fixed$path), 3L)
  expect_length(fixed$selected, 30L)
  expect_s3_class(plot_wavelength_selection(fixed), "ggplot")
  expect_output(print(fixed), "3 of 12")
})

test_that("iPLS gives the same result in parallel", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  d <- make_selection_data(seed = 7)
  sequential <- select_wavelengths(d$x, d$y, "ipls", ncomp = 3, intervals = 12, num_intervals = 2)
  in_parallel <- function() {
    old <- future::plan("multisession", workers = 2)
    on.exit(future::plan(old), add = TRUE)
    select_wavelengths(d$x, d$y, "ipls", ncomp = 3, intervals = 12, num_intervals = 2)
  }
  expect_identical(in_parallel()$path, sequential$path)
})

test_that("robust selection sets the regression outliers aside", {
  d <- make_selection_data(seed = 8)
  x <- d$x
  y <- d$y
  # wrong reference values and spectra with a spurious line
  y[1:6] <- y[1:6] + 3
  x[7:10, 100:105] <- x[7:10, 100:105] + 300
  y[7:10] <- y[7:10] - 3
  set.seed(9)
  sr <- select_wavelengths(x, y, "sr", ncomp = 3, num_terms = 6, robust = TRUE)
  expect_true(sr$observations <= 70)
  expect_true(all(match(sr$selected, colnames(x)) %in% d$line))
  set.seed(9)
  vip <- select_wavelengths(x, y, "vip", ncomp = 3, num_terms = 6, robust = TRUE)
  expect_equal(vip$observations, sr$observations)
  set.seed(10)
  ipls <- select_wavelengths(x, y, "ipls", ncomp = 3, intervals = 12, num_intervals = 1, robust = TRUE)
  expect_equal(ipls$path$interval, 5L)
  expect_output(print(ipls), "robust")
  expect_error(select_wavelengths(x, y, "vip", robust = TRUE, beta = 1), "`alpha`, `ndir` or `nsamp`")
})

test_that("select_wavelengths checks its arguments", {
  d <- make_selection_data(seed = 11)
  expect_error(select_wavelengths(d$x), "Both 'x' and 'y'")
  expect_error(select_wavelengths(d$x, cbind(d$y, d$y)), "single response")
  expect_error(select_wavelengths(d$x, d$y, "vip", threshold = 50), "No variable")
  # num_terms larger than the number of variables keeps them all
  expect_length(select_wavelengths(d$x, d$y, "sr", num_terms = 500)$selected, 120L)
})

test_that("step_select_wavelengths keeps the selected predictors", {
  skip_if_not_installed("recipes", "1.1.0")
  d <- make_selection_data(seed = 12)
  dat <- data.frame(y = d$y, d$x, check.names = FALSE)
  rec <- recipes::recipe(y ~ ., data = dat[1:60, ]) |>
    step_select_wavelengths(recipes::all_predictors(), method = "sr", num_terms = 6, num_comp = 3)
  prepped <- recipes::prep(rec)
  ref <- select_wavelengths(d$x[1:60, ], d$y[1:60], "sr", ncomp = 3, num_terms = 6)
  baked <- recipes::bake(prepped, new_data = dat[61:80, -1])
  expect_equal(names(baked), ref$selected)
  expect_equal(names(recipes::bake(prepped, new_data = NULL)), c(ref$selected, "y"))

  td <- recipes::tidy(prepped, number = 1)
  expect_named(td, c("terms", "selected", "importance", "id"))
  expect_equal(sum(td$selected), 6L)
  expect_equal(td$importance, unname(ref$importance))
  expect_true(all(is.na(recipes::tidy(rec, number = 1)$selected)))

  expect_equal(generics::tunable(rec)$name, c("num_terms", "num_intervals", "num_comp"))
  expect_true(any(grepl("Wavelength selection", cli::cli_fmt(print(prepped)))))
  expect_equal(generics::required_pkgs(rec), c("recipes", "specProc"))
})

test_that("step_select_wavelengths is re-estimated when tuned", {
  skip_on_cran()
  skip_if_not_installed("tune")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dials")
  d <- make_selection_data(seed = 13)
  dat <- data.frame(y = d$y, d$x, check.names = FALSE)
  rec <- recipes::recipe(y ~ ., data = dat) |>
    step_select_wavelengths(recipes::all_predictors(), method = "vip", num_terms = tune::tune(),
                            num_comp = 3)
  wf <- workflows::workflow(rec, parsnip::linear_reg())
  set.seed(14)
  res <- tune::tune_grid(wf, resamples = rsample::vfold_cv(dat, v = 3),
                         grid = data.frame(num_terms = c(3L, 8L)))
  metrics <- tune::collect_metrics(res)
  expect_equal(sort(unique(metrics$num_terms)), c(3L, 8L))
  expect_true(all(is.finite(metrics$mean)))
})

test_that("num_intervals is an integer dials parameter", {
  skip_if_not_installed("dials")
  p <- num_intervals()
  expect_s3_class(p, "quant_param")
  expect_equal(dials::value_seq(p, 2), c(1L, 10L))
})
