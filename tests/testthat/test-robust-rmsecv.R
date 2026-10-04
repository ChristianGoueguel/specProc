test_that("robust_rmsecv leaves the outliers out of the cross-validated error", {
  d <- make_regression(n = 60, p = 30, seed = 21)
  y <- d$train$y
  y[1:4] <- y[1:4] + 20  # wrong reference values
  set.seed(22)
  res <- robust_rmsecv(d$train$x, y, kmax = 4, folds = 4)
  expect_named(res, c("ncomp", "R2", "RMSE", "RMSECV", "RCS"))
  expect_equal(res$ncomp, 1:4)
  expect_equal(res$RCS, sqrt(0.5 * res$RMSECV^2 + 0.5 * res$RMSE^2))
  expect_true(all(attr(res, "weights")[1:4] == 0))
  # three latent variables and noise sd 0.3: the error is small at 3
  # components, despite the wrong reference values
  expect_lt(res$RMSECV[3], 0.6)
  expect_lt(res$RMSECV[3], res$RMSECV[1])
  # the fit to all the data is that of rsimpls()
  set.seed(22)
  fit <- rsimpls(d$train$x, y, ncomp = 2, kmax = 4)
  expect_equal(res$R2, fit$components$R2)
  expect_equal(res$RMSE, fit$components$RMSE)
})

test_that("robust_rmsecv handles rpcr, gamma and its arguments", {
  d <- make_regression(n = 50, p = 20, seed = 23)
  set.seed(24)
  res <- robust_rmsecv(d$train$x, d$train$y, method = "rpcr", kmax = 3, folds = 3, gamma = 1)
  expect_equal(res$RCS, res$RMSECV)
  expect_identical(attr(res, "method"), "rpcr")
  expect_true(all(is.finite(res$RMSECV)))
  expect_error(robust_rmsecv(d$train$x, d$train$y, folds = 1), "'folds'")
  expect_error(robust_rmsecv(d$train$x, d$train$y, folds = 51), "cannot exceed")
  expect_error(robust_rmsecv(d$train$x, d$train$y, gamma = 2), "'gamma'")
})
