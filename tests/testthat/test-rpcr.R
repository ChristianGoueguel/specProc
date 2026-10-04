test_that("rpcr resists vertical outliers and bad leverage points", {
  d <- make_regression()
  x <- d$train$x
  y <- d$train$y
  y[1:10] <- y[1:10] + 15
  set.seed(3)
  x[11:15, ] <- x[11:15, ] + matrix(rnorm(5 * ncol(x), mean = 1, sd = 0.5), 5)
  y[11:15] <- y[11:15] - 20
  set.seed(4)
  fit <- rpcr(x, y, ncomp = 3)
  expect_s3_class(fit, "specproc_rpcr")
  expect_identical(fit$regression, "LTS")
  rmsep <- function(pred) sqrt(mean((pred - d$test$y)^2))
  classical <- pls::pcr(y ~ x, ncomp = 3)
  clean <- pls::pcr(d$train$y[-(1:15)] ~ d$train$x[-(1:15), ], ncomp = 3)
  robust_error <- rmsep(predict(fit, d$test$x))
  expect_lt(robust_error, 1.2 * rmsep(drop(predict(clean, d$test$x, ncomp = 3))))
  expect_lt(robust_error, 0.5 * rmsep(drop(predict(classical, d$test$x, ncomp = 3))))
  expect_true(all(fit$outlier_type[1:15] %in% c("vertical outlier", "bad leverage")))
  expect_lt(mean(fit$outlier_type[-(1:15)] %in% c("vertical outlier", "bad leverage")), 0.1)
  expect_true(all(fit$weights[1:15] == 0))
  expect_output(print(fit), "RPCR")
})

test_that("rpcr is close to PCR on clean data, and uses the robust PCA", {
  d <- make_regression(seed = 5)
  set.seed(6)
  fit <- rpcr(d$train$x, d$train$y, ncomp = 3, kmax = 3)
  ref <- pls::pcr(d$train$y ~ d$train$x, ncomp = 3)
  expect_gt(cor(drop(fit$coefficients), drop(coef(ref, ncomp = 3))), 0.95)
  expect_gt(fit$R2, 0.95)
  # with kmax = ncomp, the distances are those of robpca() with ncomp components
  set.seed(6)
  rob <- robpca(d$train$x, k = 3, kmax = 3)
  expect_equal(fit$sd, rob$sd, ignore_attr = TRUE)
  expect_equal(fit$od, rob$od, ignore_attr = TRUE)
  expect_equal(fit$cutoff_od, rob$cutoff_od)
  expect_equal(fit$components$ncomp, 1:3)
})

test_that("predict() of rpcr reproduces the fit, scores and distances", {
  d <- make_regression(seed = 9)
  set.seed(10)
  fit <- rpcr(d$train$x, d$train$y, ncomp = 3)
  expect_equal(predict(fit, d$train$x), drop(fit$fitted), ignore_attr = TRUE)
  scores <- predict(fit, d$train$x, type = "scores")
  expect_named(scores, c("Comp1", "Comp2", "Comp3", "sd", "od"))
  expect_equal(as.matrix(scores[1:3]), fit$x_scores, ignore_attr = TRUE)
  expect_equal(scores$sd, fit$sd)
  expect_equal(scores$od, fit$od)
  expect_length(fit$models, fit$kmax)
  expect_equal(predict(fit, d$test$x, ncomp = 3), predict(fit, d$test$x))
  expect_error(predict(fit, d$test$x, ncomp = fit$kmax + 1), "cannot exceed")
})

test_that("rpcr handles several responses with MCD regression", {
  d <- make_regression(seed = 11)
  y <- cbind(a = d$train$y, b = d$train$x[, 1:5] %*% rep(1, 5) + rnorm(100, sd = 0.1))
  y[1:5, "a"] <- y[1:5, "a"] + 20
  set.seed(12)
  fit <- rpcr(d$train$x, y, ncomp = 3)
  expect_identical(fit$regression, "MCD")
  expect_equal(dim(fit$coefficients), c(200L, 2L))
  expect_equal(dim(fit$sigma), c(2L, 2L))
  expect_equal(fit$cutoff_rd, sqrt(stats::qchisq(0.975, 2)))
  expect_true(all(fit$rd[1:5] > fit$cutoff_rd))
  expect_equal(dim(predict(fit, d$test$x)), c(200L, 2L))
  expect_match(plot_outlier_map(fit)$labels$title, "RPCR outlier map")
  expect_match(plot_outlier_map(fit, map = "score", newdata = d$test$x)$labels$title,
               "RPCR score outlier map")
})

test_that("rpcr checks its arguments", {
  d <- make_regression(n = 40, p = 20, seed = 13)
  expect_error(rpcr(d$train$x, d$train$y), "'ncomp'")
  expect_error(rpcr(d$train$x, d$train$y[-1], ncomp = 2), "same number of rows")
  expect_error(rpcr(d$train$x, c(NA, d$train$y[-1]), ncomp = 2), "missing")
  set.seed(14)
  expect_error(rpcr(d$train$x[, 1:4], d$train$y, ncomp = 5), "cannot exceed 4")
})
