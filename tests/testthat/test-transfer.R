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
  expect_no_error(y_gradient_glsw(as.data.frame(x), data.frame(y = y)))
  expect_error(y_gradient_glsw(x, y[-1]), "same length")
  expect_error(y_gradient_glsw(x, y, window = 4), "odd")
  expect_error(y_gradient_glsw(x, y, alpha = -1), "positive")
  skip_if_not_installed("prospectr")
  o <- order(y)
  xd <- t(prospectr::savitzkyGolay(t(x[o, ]), m = 1, p = 2, w = 5))
  yd <- drop(prospectr::savitzkyGolay(matrix(y[o], 1), m = 1, p = 2, w = 5))
  w <- 2^(-abs(yd) / stats::sd(yd))
  expect_equal(as.matrix(G), glsw_reference(w * xd, 0.02), tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("pds recovers a linear instrument response", {
  set.seed(4)
  wl <- seq(0, 1, length.out = 40)
  x1 <- t(replicate(15, stats::runif(1) * stats::dnorm(wl, 0.5, 0.1) + stats::runif(1)))
  x2 <- 1.1 * x1 + 0.05
  for (alpha in c(0, 0.5, 1)) {
    model <- pds(x1, x2, win = 3, ncomp = 2, alpha = alpha)
    expect_true(all(c("transfer_matrix", "intercept") %in% names(model)))
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

# Transfer spectra in a low-dimensional space (as spectra are), with a
# secondary instrument of another response and offset
transfer_data <- function(n = 40, p = 60, seed = 5) {
  set.seed(seed)
  scores <- matrix(stats::rnorm(n * 3), n)
  loadings <- matrix(stats::rnorm(3 * p), 3)
  x1 <- 100 + scores %*% loadings + matrix(stats::rnorm(n * p, sd = 0.01), n)
  response <- matrix(seq(0.8, 1.2, length.out = p), n, p, byrow = TRUE)
  x2 <- 5 + x1 * response
  colnames(x1) <- colnames(x2) <- paste0("w", seq_len(p))
  list(x1 = x1, x2 = x2)
}

test_that("ds transfers spectra of the secondary instrument", {
  d <- transfer_data()
  transfer <- ds(d$x1[1:20, ], d$x2[1:20, ])
  expect_s3_class(transfer, "specproc_transfer")
  # the transfer samples are reproduced exactly with all components
  expect_equal(predict(transfer, d$x2[1:20, ]), d$x1[1:20, ], tolerance = 1e-8)
  # new samples of the same space
  new <- predict(transfer, d$x2[21:40, ])
  expect_equal(colnames(new), colnames(d$x1))
  expect_lt(max(abs(new - d$x1[21:40, ])), 0.1)
  # the definition: m1 + (x - m2) X2c^+ X1c
  x1c <- sweep(d$x1[1:20, ], 2, colMeans(d$x1[1:20, ]))
  x2c <- sweep(d$x2[1:20, ], 2, colMeans(d$x2[1:20, ]))
  s <- svd(x2c)
  k <- sum(s$d > 1e-8 * s$d[1])
  f <- s$v[, 1:k] %*% diag(1 / s$d[1:k]) %*% t(s$u[, 1:k]) %*% x1c
  ref <- sweep(sweep(d$x2[21:40, ], 2, colMeans(d$x2[1:20, ])) %*% f, 2, colMeans(d$x1[1:20, ]), "+")
  expect_equal(unname(new), unname(ref), tolerance = 1e-8)
  # fewer components
  expect_equal(ds(d$x1[1:20, ], d$x2[1:20, ], ncomp = 3)$ncomp, 3)
  expect_output(print(transfer), "Direct standardization")
  # a data frame gives a tibble
  expect_s3_class(predict(transfer, as.data.frame(d$x2[21:22, ])), "tbl_df")
})

test_that("sst moves the secondary spectra by the difference of the instruments", {
  d <- transfer_data()
  transfer <- sst(d$x1[1:20, ], d$x2[1:20, ], ncomp = 3)
  expect_s3_class(transfer, "specproc_transfer")
  new <- predict(transfer, d$x2[21:40, ])
  untransferred <- mean(abs(d$x2[21:40, ] - d$x1[21:40, ]))
  expect_lt(mean(abs(new - d$x1[21:40, ])), untransferred / 50)
  # the definition: x + t (P1' - P2'), with t = x (P2')^+
  x1c <- sweep(d$x1[1:20, ], 2, colMeans(d$x1[1:20, ]))
  x2c <- sweep(d$x2[1:20, ], 2, colMeans(d$x2[1:20, ]))
  v <- svd(cbind(x1c, x2c))$v[, 1:3]
  p1 <- v[1:60, ]
  p2 <- v[61:120, ]
  xc <- sweep(d$x2[21:40, ], 2, colMeans(d$x2[1:20, ]))
  ref <- sweep(xc + xc %*% MASS::ginv(t(p2)) %*% (t(p1) - t(p2)), 2, colMeans(d$x1[1:20, ]), "+")
  expect_equal(unname(new), unname(ref), tolerance = 1e-8)
  # identical instruments: no change
  same <- sst(d$x1[1:20, ], d$x1[1:20, ], ncomp = 3)
  expect_equal(predict(same, d$x1[21:40, ]), d$x1[21:40, ], tolerance = 1e-8)
  # by explained variance
  expect_gte(sst(d$x1[1:20, ], d$x2[1:20, ])$explained, 0.99)
  expect_error(sst(d$x1[1:20, ], d$x2[1:20, ], ncomp = 30), "rank")
  expect_output(print(transfer), "Spectral space transformation")
})

test_that("pds results are transfers too", {
  d <- transfer_data()
  transfer <- pds(d$x1[1:20, ], d$x2[1:20, ], win = 2, ncomp = 2)
  expect_s3_class(transfer, "specproc_transfer")
  expect_equal(predict(transfer, d$x2[21:22, ]),
               sweep(d$x2[21:22, ] %*% transfer$transfer_matrix, 2, transfer$intercept, "+"))
  expect_output(print(transfer), "Piecewise")
  expect_error(predict(transfer, d$x2[, 1:5]), "wavelengths")
})

test_that("transfers validate their inputs", {
  d <- transfer_data()
  expect_error(ds(d$x1, d$x2[, -1]), "same samples")
  expect_error(sst(d$x1[1:2, ], d$x2[1:2, ]), "At least 3")
  bad <- d$x2
  bad[1, 1] <- NA
  expect_error(ds(d$x1, bad), "missing")
})

test_that("slope_bias corrects predictions", {
  set.seed(1)
  reference <- stats::runif(20, 0.5, 4)
  predicted <- 0.2 + 1.1 * reference + stats::rnorm(20, sd = 0.05)
  correction <- slope_bias(predicted, reference)
  expect_s3_class(correction, "specproc_slope_bias")
  fit <- stats::lm(reference ~ predicted)
  expect_equal(correction$coefficients$estimate, unname(stats::coef(fit)))
  expect_equal(predict(correction, c(1, 2)), unname(stats::coef(fit)[1] + stats::coef(fit)[2] * c(1, 2)))
  # the slope differs from 1 and the bias from 0
  expect_true(all(correction$coefficients$p_value < 0.01))
  expect_lt(correction$rmse[["after"]], correction$rmse[["before"]] / 5)
  # bias only
  bias <- slope_bias(reference + 0.3, reference, slope = FALSE)
  expect_equal(bias$coefficients$estimate, c(-0.3, 1))
  expect_equal(predict(bias, 2), 1.7)
  expect_output(print(correction), "Slope and bias")
  expect_error(slope_bias(1:3, 1:2), "same length")
  expect_error(slope_bias(1:2, 1:2), "at least 3")
})
