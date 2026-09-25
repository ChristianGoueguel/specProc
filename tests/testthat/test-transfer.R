glsw_reference <- function(xd, alpha) {
  e <- eigen(crossprod(xd), symmetric = TRUE)
  lambda <- pmax(e$values, 0)
  e$vectors %*% diag(1 / sqrt(lambda / alpha + 1)) %*% t(e$vectors)
}

test_that("glsw matches the eigen-decomposition definition", {
  set.seed(1)
  x1 <- matrix(stats::rnorm(15 * 8), 15, 8, dimnames = list(NULL, paste0("w", 1:8)))
  x2 <- x1 + outer(stats::rnorm(15), stats::rnorm(8)) + matrix(stats::rnorm(120, sd = 0.1), 15)
  G <- glsw(x1, x2, alpha = 0.05)
  expect_s3_class(G, "tbl_df")
  expect_named(G, paste0("w", 1:8))
  xd <- scale(x2, scale = FALSE) - scale(x1, scale = FALSE)
  expect_equal(as.matrix(G), glsw_reference(xd, 0.05), tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(as.matrix(G), t(as.matrix(G)), tolerance = 1e-12, ignore_attr = TRUE)
  # a very large alpha leaves the data untouched
  expect_equal(as.matrix(glsw(x1, x2, alpha = 1e12)), diag(8), tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("glsw works when there are more variables than samples", {
  set.seed(2)
  x1 <- matrix(stats::rnorm(5 * 30), 5, 30)
  x2 <- x1 + matrix(stats::rnorm(150, sd = 0.3), 5)
  G <- as.matrix(glsw(x1, x2, alpha = 0.01))
  xd <- scale(x2, scale = FALSE) - scale(x1, scale = FALSE)
  expect_equal(G, glsw_reference(xd, 0.01), tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("glsw validates its inputs", {
  m <- matrix(1:6, 3)
  expect_error(glsw(m), "Missing 'x2'")
  expect_error(glsw(m, m[1:2, ]), "same number of rows")
  expect_error(glsw(m, cbind(m, 1)), "same number of columns")
  expect_error(glsw(m, m, alpha = 0), "positive")
})

test_that("y_gradient_glsw matches a reference implementation", {
  set.seed(3)
  x <- matrix(stats::rnorm(30 * 6), 30, 6)
  y <- x[, 1] + stats::rnorm(30, sd = 0.1)
  G <- y_gradient_glsw(x, y, alpha = 0.02)
  expect_equal(dim(G), c(6L, 6L))
  o <- order(y)
  xd <- t(prospectr::savitzkyGolay(t(x[o, ]), m = 1, p = 2, w = 5))
  yd <- drop(prospectr::savitzkyGolay(matrix(y[o], 1), m = 1, p = 2, w = 5))
  w <- 2^(-abs(yd) / stats::sd(yd))
  expect_equal(as.matrix(G), glsw_reference(w * xd, 0.02), tolerance = 1e-8, ignore_attr = TRUE)
  expect_no_error(y_gradient_glsw(as.data.frame(x), data.frame(y = y)))
  expect_error(y_gradient_glsw(x, y[-1]), "same length")
  expect_error(y_gradient_glsw(x, y, window = 4), "odd")
  expect_error(y_gradient_glsw(x, y, alpha = -1), "positive")
})

test_that("pds recovers a linear instrument response", {
  set.seed(4)
  wl <- seq(0, 1, length.out = 40)
  x1 <- t(replicate(15, stats::runif(1) * stats::dnorm(wl, 0.5, 0.1) + stats::runif(1)))
  x2 <- 1.1 * x1 + 0.05
  for (alpha in c(0, 0.5, 1)) {
    model <- pds(x1, x2, win = 3, ncomp = 2, alpha = alpha)
    expect_named(model, c("transfer_matrix", "intercept"))
    expect_equal(dim(model$transfer_matrix), c(40L, 40L))
    x2_std <- x2 %*% model$transfer_matrix + matrix(model$intercept, 15, 40, byrow = TRUE)
    expect_lt(max(abs(x2_std - x1)) / max(abs(x1)), 1e-4)
  }
  # local model: the transfer matrix is banded
  F <- pds(x1, x2, win = 2, ncomp = 2)$transfer_matrix
  expect_true(all(F[abs(row(F) - col(F)) > 2] == 0))
})

test_that("pds validates its inputs", {
  m <- matrix(stats::rnorm(30), 5)
  expect_error(pds(m, m[, 1:3]), "same number of columns")
  expect_error(pds(m, m[1:4, ]), "same number of rows")
  expect_error(pds(m, m, alpha = 2), "'alpha'")
})
