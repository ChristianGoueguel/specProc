d <- make_xy()
xc <- scale(d$x, scale = FALSE)
yc <- d$y - mean(d$y)

test_that("direct_orthogonal removes the orthogonal subspace", {
  res <- direct_orthogonal(d$x, d$y, ncomp = 2)
  expect_named(res, c("correction", "loading", "score", "center", "scale"))
  P <- as.matrix(res$loading)
  expect_equal(crossprod(P), diag(2), tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(as.matrix(res$correction) %*% P, matrix(0, 40, 2), tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(names(res$correction), colnames(d$x))
  # loadings are the dominant right singular vectors of X orthogonalized to y
  z <- xc - yc %*% crossprod(yc, xc) / sum(yc^2)
  expect_equal(abs(P), abs(svd(z)$v[, 1:2]), tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(unname(drop(crossprod(z %*% P, yc))), c(0, 0), tolerance = 1e-8)
  expect_error(direct_orthogonal(d$x), "must be provided")
  expect_error(direct_orthogonal(d$x, d$y[-1]), "same number of rows")
})

test_that("direct_osc scores are orthogonal to y and reproduce the correction", {
  res <- direct_osc(d$x, d$y, ncomp = 2, tol = 1e-12)
  T <- as.matrix(res$score)
  expect_equal(unname(drop(crossprod(T, yc))), c(0, 0), tolerance = 1e-6)
  xw <- xc %*% as.matrix(res$weight)
  expect_equal(xw, T, tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(as.matrix(res$correction), xc - T %*% t(as.matrix(res$loading)),
               tolerance = 1e-10, ignore_attr = TRUE)
  expect_warning(direct_osc(d$x[1:5, 1:3], d$y[1:5], ncomp = 10), "reduced")
})

test_that("nas projects out the interferent space", {
  res <- nas(d$x, d$y, ncomp = 2)
  expect_s3_class(res, "tbl_df")
  P <- attr(res, "loadings")
  expect_equal(as.matrix(res) %*% P, matrix(0, 40, 2), tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(nas(as.data.frame(d$x), data.frame(y = d$y), ncomp = 2), res)
})

test_that("osc methods remove variation orthogonal to y", {
  fe <- osc(d$x, d$y, method = "fearn", ncomp = 2)
  expect_equal(fe$angle, 90, tolerance = 1e-8)
  expect_equal(unname(drop(crossprod(as.matrix(fe$scores), yc))), c(0, 0), tolerance = 1e-8)
  wo <- osc(d$x, d$y, method = "wold", ncomp = 2)
  sj <- osc(d$x, d$y, method = "sjoblom", ncomp = 2)
  for (res in list(fe, wo, sj)) {
    expect_named(res, c("correction", "weights", "scores", "loadings", "angle", "R2", "center", "scale"))
    expect_equal(dim(res$correction), dim(d$x))
    expect_true(res$R2 > 0 && res$R2 < 100)
    expect_gt(res$angle, 80)
  }
  # Removing y-orthogonal variation must not hurt the linear relation with y.
  fit_r2 <- function(m) summary(stats::lm(yc ~ as.matrix(m)[, 1:5]))$r.squared
  expect_gte(fit_r2(fe$correction), fit_r2(xc) - 1e-6)
  expect_error(osc(d$x, d$y, method = "foo"), "should be one of")
  expect_error(osc(d$x, cbind(d$y, d$y)), "single response")
})

test_that("o2pls matches OPLS and handles Y-orthogonal variation", {
  fit <- o2pls(d$x, d$y, ncomp = 1, nx = 2)
  expect_s3_class(fit, "o2pls")
  expect_output(print(fit), "o2pls")
  To <- fit$scores$x_ortho
  expect_equal(dim(To), c(40L, 2L))
  # orthogonal scores are orthogonal to the joint (predictive) scores
  expect_equal(unname(drop(crossprod(To, fit$scores$x))), c(0, 0), tolerance = 1e-8)

  set.seed(2)
  Y <- cbind(d$y, d$y + stats::rnorm(40), stats::rnorm(40))
  fit2 <- o2pls(d$x, Y, ncomp = 1, nx = 1, ny = 1)
  expect_equal(dim(fit2$scores$y_ortho), c(40L, 1L))
  expect_error(o2pls(d$x, d$y, ncomp = 1, ny = 1), "ncomp \\+ ny")
})

test_that("projected_osc equals O2PLS/OPLS filtering (Kemsley & Tapp)", {
  posc <- projected_osc(d$x, d$y, ncomp = 3)
  o2 <- o2pls(d$x, d$y, ncomp = 1, nx = 2)
  expect_equal(as.matrix(posc$correction), as.matrix(o2$correction), tolerance = 1e-8)
})

test_that("projected_osc and o2pls agree with ropls", {
  skip_if_not_installed("ropls")
  m <- suppressWarnings(suppressMessages(ropls::opls(
    d$x, d$y, predI = 1, orthoI = 2, scaleC = "center", crossvalI = 5, permI = 0,
    fig.pdfC = "none", info.txtC = "none"
  )))
  ref <- xc - m@orthoScoreMN %*% t(m@orthoLoadingMN)
  posc <- projected_osc(d$x, d$y, ncomp = 3)
  expect_equal(as.matrix(posc$correction), ref, tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("projected_osc corrects new data with the training model", {
  res <- projected_osc(d$x[1:30, ], d$y[1:30], ncomp = 3, newdata = d$x[1:30, ])
  expect_equal(as.matrix(res$newdata$correction), as.matrix(res$correction), tolerance = 1e-10)
  res2 <- projected_osc(d$x[1:30, ], d$y[1:30], ncomp = 3, newdata = d$x[31:40, ])
  expect_equal(dim(res2$newdata$correction), c(10L, 25L))
  expect_error(projected_osc(d$x, d$y, ncomp = 1), ">= 2")
  expect_error(projected_osc(d$x, d$y, newdata = d$x[, 1:3]), "same number of columns")
})

test_that("epo removes the dominant clutter directions", {
  set.seed(3)
  x <- matrix(stats::rnorm(20 * 30), 20, 30)
  direction <- sin(seq(0, pi, length.out = 30))
  direction <- direction / sqrt(sum(direction^2))
  clutter <- outer(stats::rnorm(20, sd = 5), direction) + matrix(stats::rnorm(600, sd = 0.01), 20)
  res <- epo(x + clutter, ncomp = 1, clutter = clutter)
  v <- as.matrix(res$loadings)[, 1]
  expect_equal(abs(sum(v * direction)), 1, tolerance = 1e-3)
  expect_equal(drop(as.matrix(res$correction) %*% v), rep(0, 20), tolerance = 1e-10)
  expect_equal(as.matrix(res$correction) + as.matrix(res$clutter), x + clutter, ignore_attr = TRUE)
  # default: clutter estimated from x itself (the largest singular directions)
  res2 <- epo(x, ncomp = 2)
  expect_equal(res2$singular_values, svd(x)$d[1:2], tolerance = 1e-10)
  expect_error(epo(x, ncomp = 0), "positive integer")
  expect_error(epo(x, clutter = x[, 1:5]), "same number of columns")
})
