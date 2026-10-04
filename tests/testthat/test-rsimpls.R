# Spectra-like data with three latent variables, and a response linear in
# them. The test set comes from the same model.
make_regression <- function(n = 100, p = 200, seed = 1) {
  set.seed(seed)
  loadings <- qr.Q(qr(matrix(rnorm(p * 3), p, 3)))
  latent <- function(m) matrix(rnorm(m * 3), m, 3) %*% diag(c(10, 6, 3))
  draw <- function(m) {
    l <- latent(m)
    list(x = l %*% t(loadings) + matrix(rnorm(m * p, sd = 0.3), m, p),
         y = drop(l %*% c(0.3, -0.5, 0.8)) + rnorm(m, sd = 0.3))
  }
  list(train = draw(n), test = draw(200))
}

test_that("svd_reduce_cpp spans the data and keeps their inner products", {
  set.seed(1)
  for (dims in list(c(30, 80), c(80, 30))) {
    x <- matrix(rnorm(dims[1] * 5), dims[1], 5) %*% matrix(rnorm(5 * dims[2]), 5, dims[2]) + 100
    red <- svd_reduce(x)
    xc <- sweep(x, 2, colMeans(x))
    expect_equal(ncol(red$z), 5L)
    expect_equal(crossprod(red$v), diag(5), tolerance = 1e-10)
    expect_equal(red$z %*% t(red$v), xc, tolerance = 1e-10, ignore_attr = TRUE)
  }
  expect_error(svd_reduce(matrix(1, 10, 4)), "no variation")
})

test_that("the robust SIMPLS core reproduces SIMPLS with the empirical covariance", {
  set.seed(2)
  x <- matrix(rnorm(80 * 30), 80) %*% matrix(rnorm(30 * 30, sd = 0.3), 30)
  y <- cbind(x[, 1:3] %*% c(1, -2, 0.5), x[, 4] - x[, 5]) + rnorm(160, sd = 0.3)
  for (q in 1:2) {
    yq <- y[, seq_len(q), drop = FALSE]
    e <- eigen(stats::cov(cbind(x, yq)), symmetric = TRUE)
    fit <- simpls_robust(e$vectors[1:30, ], e$vectors[30 + seq_len(q), , drop = FALSE], e$values, 6)
    t <- sweep(x, 2, colMeans(x)) %*% fit$weights
    b <- fit$weights %*% solve(crossprod(t), crossprod(t, sweep(yq, 2, colMeans(yq))))
    ref <- pls::plsr(yq ~ x, ncomp = 6, method = "simpls")
    expect_equal(b, coef(ref, ncomp = 6)[, , 1], tolerance = 1e-8, ignore_attr = TRUE)
  }
})

test_that("rsimpls resists vertical outliers and bad leverage points", {
  d <- make_regression()
  x <- d$train$x
  y <- d$train$y
  vertical <- 1:10
  leverage <- 11:15
  y[vertical] <- y[vertical] + 15
  set.seed(3)
  x[leverage, ] <- x[leverage, ] + matrix(rnorm(5 * ncol(x), mean = 1, sd = 0.5), 5)
  y[leverage] <- y[leverage] - 20
  set.seed(4)
  fit <- rsimpls(x, y, ncomp = 3)
  expect_s3_class(fit, "specproc_rsimpls")
  rmsep <- function(pred) sqrt(mean((pred - d$test$y)^2))
  classical <- pls::plsr(y ~ x, ncomp = 3, method = "simpls")
  clean <- pls::plsr(d$train$y[-(1:15)] ~ d$train$x[-(1:15), ], ncomp = 3, method = "simpls")
  robust_error <- rmsep(predict(fit, d$test$x))
  expect_lt(robust_error, 1.2 * rmsep(drop(predict(clean, d$test$x, ncomp = 3))))
  expect_lt(robust_error, 0.5 * rmsep(drop(predict(classical, d$test$x, ncomp = 3))))
  # all the outliers are flagged, few regular observations
  expect_true(all(fit$outlier_type[1:15] %in% c("vertical outlier", "bad leverage")))
  expect_lt(mean(fit$outlier_type[-(1:15)] %in% c("vertical outlier", "bad leverage")), 0.1)
  expect_true(all(fit$weights[1:15] == 0))
  expect_output(print(fit), "RSIMPLS")
})

test_that("rsimpls is close to SIMPLS on clean data", {
  d <- make_regression(seed = 5)
  set.seed(6)
  fit <- rsimpls(d$train$x, d$train$y, ncomp = 3)
  ref <- pls::plsr(d$train$y ~ d$train$x, ncomp = 3, method = "simpls")
  expect_gt(cor(drop(fit$coefficients), drop(coef(ref, ncomp = 3))), 0.95)
  expect_equal(fit$components$ncomp, seq_len(fit$kmax))
  expect_gt(fit$components$R2[3], 0.95)
})

test_that("the results do not depend on the units of the response", {
  d <- make_regression(seed = 7)
  set.seed(8)
  a <- rsimpls(d$train$x, d$train$y, ncomp = 2)
  set.seed(8)
  b <- rsimpls(d$train$x, d$train$y * 1000, ncomp = 2)
  expect_equal(b$coefficients, a$coefficients * 1000)
  expect_equal(b$intercept, a$intercept * 1000)
  expect_identical(b$outlier_type, a$outlier_type)
})

test_that("predict() reproduces the fitted values, scores and score distances", {
  d <- make_regression(seed = 9)
  set.seed(10)
  fit <- rsimpls(d$train$x, d$train$y, ncomp = 3)
  expect_equal(predict(fit, d$train$x), drop(fit$fitted), ignore_attr = TRUE)
  expect_equal(d$train$y - predict(fit, d$train$x), drop(fit$residuals), ignore_attr = TRUE)
  scores <- predict(fit, d$train$x, type = "scores")
  expect_named(scores, c("Comp1", "Comp2", "Comp3", "sd", "od"))
  expect_equal(as.matrix(scores[1:3]), fit$x_scores, ignore_attr = TRUE)
  expect_equal(scores$sd, fit$sd)
  expect_equal(scores$od, fit$od)
  expect_error(predict(fit, d$train$x[, 1:10]), "200 columns")
})

test_that("rsimpls flags orthogonal outliers and gives the robust R2 of the model", {
  d <- make_regression(seed = 17)
  x <- d$train$x
  # spectra with a pattern outside the latent space, and correct responses
  set.seed(18)
  x[1:5, ] <- x[1:5, ] + matrix(rnorm(5 * ncol(x), sd = 2), 5)
  set.seed(19)
  fit <- rsimpls(x, d$train$y, ncomp = 3)
  expect_length(fit$od, nrow(x))
  expect_gt(fit$cutoff_od, 0)
  expect_true(all(fit$od[1:5] > fit$cutoff_od))
  expect_lt(mean(fit$od[-(1:5)] > fit$cutoff_od), 0.1)
  # X residuals of the components
  xc <- sweep(x, 2, fit$center)
  expect_equal(fit$od, sqrt(rowSums((xc - fit$x_scores %*% t(fit$x_loadings))^2)),
               ignore_attr = TRUE)
  regular <- fit$od <= fit$cutoff_od & fit$rd <= fit$cutoff_rd
  yr <- d$train$y[regular]
  expect_equal(fit$R2, 1 - sum(fit$residuals[regular]^2) / sum((yr - mean(yr))^2))
  expect_gt(fit$R2, 0.95)
  expect_output(print(fit), "Orthogonal outliers: ")
})

test_that("rsimpls handles several responses", {
  d <- make_regression(seed = 11)
  y <- cbind(a = d$train$y, b = d$train$x[, 1:5] %*% rep(1, 5) + rnorm(100, sd = 0.1))
  set.seed(12)
  fit <- rsimpls(d$train$x, y, ncomp = 3)
  expect_equal(dim(fit$coefficients), c(200L, 2L))
  expect_equal(dim(fit$sigma), c(2L, 2L))
  expect_equal(fit$cutoff_rd, sqrt(stats::qchisq(0.975, 2)))
  expect_true(fit$R2 > 0 && fit$R2 < 1)
  expect_equal(dim(predict(fit, d$test$x)), c(200L, 2L))
  expect_s3_class(plot_outlier_map(fit), "ggplot")
})

test_that("rsimpls checks its arguments", {
  d <- make_regression(n = 40, p = 20, seed = 13)
  expect_error(rsimpls(d$train$x, d$train$y), "'ncomp'")
  expect_error(rsimpls(d$train$x, d$train$y[-1], ncomp = 2), "same number of rows")
  expect_error(rsimpls(d$train$x, rep(1, 40), ncomp = 2), "no variation")
  # kmax is raised to ncomp, and limited by the variables
  set.seed(14)
  expect_equal(rsimpls(d$train$x, d$train$y, ncomp = 12)$kmax, 12L)
  expect_error(rsimpls(d$train$x[, 1:5], d$train$y, ncomp = 5), "cannot exceed 4")
})

test_that("plot_outlier_map() draws the regression outlier map", {
  d <- make_regression(seed = 15)
  set.seed(16)
  fit <- rsimpls(d$train$x, d$train$y, ncomp = 3)
  p <- plot_outlier_map(fit, shade = TRUE)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "RSIMPLS")
  expect_equal(p$labels$y, "Absolute standardized residual")
  expect_error(plot_outlier_map(fit, newdata = d$test$x), "not used")
})

test_that("plot_outlier_map() draws the score outlier map of an rsimpls fit", {
  d <- make_regression(seed = 15)
  set.seed(16)
  fit <- rsimpls(d$train$x, d$train$y, ncomp = 3)
  p <- plot_outlier_map(fit, map = "score", newdata = d$test$x[1:20, ])
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "score outlier map")
  expect_equal(p$labels$y, "Orthogonal distance")
  expect_equal(nrow(p$data), 120L)
  expect_equal(p$data$y[1:100], fit$od)
  expect_equal(as.character(p$data$type[1:100]),
               as.character(outlier_type(fit$sd, fit$od, fit$cutoff_sd, fit$cutoff_od)))
  # map is not used for robust PCA fits
  set.seed(17)
  pca <- robpca(d$train$x, k = 3)
  expect_equal(plot_outlier_map(pca, map = "score")$labels$y, "Orthogonal distance")
})
