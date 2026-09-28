# Low-rank data with a known subspace, plus outlying observations.
make_lowrank <- function(n = 120, p = 40, k = 3, n_out = 8, seed = 1) {
  set.seed(seed)
  loadings <- qr.Q(qr(matrix(rnorm(p * k), p, k)))
  scores <- matrix(rnorm(n * k), n, k) %*% diag(seq(10, 4, length.out = k), k)
  x <- scores %*% t(loadings) + matrix(rnorm(n * p, sd = 0.2), n, p)
  out <- seq_len(n_out)
  x[out, ] <- x[out, ] + matrix(rnorm(n_out * p, mean = 3, sd = 2), n_out, p)
  colnames(x) <- paste0("v", seq_len(p))
  list(x = x, loadings = loadings, outliers = out)
}
max_angle <- function(a, b) {
  s <- svd(crossprod(qr.Q(qr(a)), qr.Q(qr(b))))$d
  max(acos(pmin(1, s))) * 180 / pi
}

test_that("univariate_mcd_cpp finds the h-subset with the smallest variance", {
  set.seed(1)
  x <- c(rnorm(40), rnorm(10, 20))
  h <- 30
  s <- sort(x)
  vars <- sapply(seq_len(length(x) - h + 1), function(i) var(s[i:(i + h - 1)]))
  best <- which.min(vars)
  res <- univariate_mcd_cpp(x, h)
  expect_equal(res[["location"]], mean(s[best:(best + h - 1)]))
  alpha <- h / length(x)
  factor <- alpha / pchisq(qchisq(alpha, 1), 3)
  expect_equal(res[["scale"]], sqrt(vars[best] * factor))
})

test_that("fast_mcd_cpp agrees with robustbase::covMcd on the center and shape", {
  set.seed(2)
  x <- MASS::mvrnorm(200, c(1, -1, 2), matrix(c(4, 1, 0, 1, 2, 0.5, 0, 0.5, 1), 3))
  x[1:30, ] <- x[1:30, ] + 10
  mine <- fast_mcd_cpp(x, 150L, 500L)
  ref <- robustbase::covMcd(x, alpha = 0.75)
  expect_false(mine$singular)
  expect_equal(mine$center, unname(ref$center), tolerance = 0.05)
  # same shape; scale factors differ by the small-sample corrections
  expect_equal(cov2cor(mine$cov), unname(cov2cor(ref$cov)), tolerance = 0.05)
  expect_false(any(mine$weights[1:30]))
})

test_that("robpca recovers the subspace and flags the outliers", {
  d <- make_lowrank()
  set.seed(3)
  fit <- robpca(d$x, k = 3)
  expect_s3_class(fit, "specproc_robpca")
  expect_lt(max_angle(fit$loadings, d$loadings), 2)
  expect_true(all(d$outliers %in% which(fit$outlier_type != "regular")))
  expect_lt(mean(fit$outlier_type[-d$outliers] != "regular"), 0.1)
  expect_equal(dim(fit$scores), c(nrow(d$x), 3L))
  expect_equal(unname(fit$sd), unname(sqrt(rowSums(sweep(fit$scores^2, 2, fit$eigenvalues, "/")))))
  expect_output(print(fit), "ROBPCA")
})

test_that("robpca works with more variables than observations and chooses k", {
  d <- make_lowrank(n = 50, p = 300, k = 2, n_out = 4, seed = 4)
  set.seed(5)
  fit <- robpca(d$x, k = 2)
  # as close to the true subspace as classical PCA of the clean observations
  clean <- prcomp(d$x[-d$outliers, ])$rotation[, 1:2]
  expect_lt(max_angle(fit$loadings, d$loadings), max_angle(clean, d$loadings) + 2)
  set.seed(5)
  auto <- robpca(d$x)
  expect_gte(auto$k, 1)
  expect_lte(auto$k, 10)
})

test_that("robpca is reproducible with set.seed", {
  d <- make_lowrank(seed = 6)
  set.seed(7)
  a <- robpca(d$x, k = 2)
  set.seed(7)
  b <- robpca(d$x, k = 2)
  expect_identical(a$loadings, b$loadings)
})

test_that("predict() reproduces the calibration scores and distances", {
  d <- make_lowrank(seed = 8)
  set.seed(9)
  fit <- robpca(d$x, k = 3)
  pred <- predict(fit, d$x)
  expect_named(pred, c("PC1", "PC2", "PC3", "sd", "od", "outlier_type"))
  expect_equal(as.matrix(pred[1:3]), fit$scores, ignore_attr = TRUE)
  expect_equal(pred$sd, unname(fit$sd), ignore_attr = TRUE)
  expect_equal(pred$od, unname(fit$od), ignore_attr = TRUE)
  expect_equal(pred$outlier_type, fit$outlier_type)
})

test_that("robpca agrees with rospca::robpca", {
  skip_if_not_installed("rospca")
  d <- make_lowrank(seed = 10)
  ref <- rospca::robpca(d$x, k = 3)
  set.seed(11)
  fit <- robpca(d$x, k = 3)
  expect_lt(max_angle(fit$loadings, ref$loadings), 2)
  expect_equal(fit$eigenvalues, ref$eigenvalues, tolerance = 0.1, ignore_attr = TRUE)
  expect_gt(mean((fit$outlier_type != "regular") == (ref$flag.all == 0)), 0.95)
})

test_that("rospca recovers a block-sparse structure", {
  set.seed(12)
  n <- 150
  f1 <- rnorm(n, sd = 3)
  f2 <- rnorm(n, sd = 2)
  x <- cbind(f1 + matrix(rnorm(n * 4, sd = 0.5), n), f2 + matrix(rnorm(n * 4, sd = 0.5), n),
             matrix(rnorm(n * 4, sd = 1), n))
  x[1:10, ] <- x[1:10, ] + 8
  fit <- rospca(x, k = 2, lambda = 1)
  expect_s3_class(fit, c("specproc_rospca", "specproc_robpca"))
  support <- fit$loadings != 0
  expect_true(all(support[1:4, 1]) && !any(support[5:12, 1]))
  expect_true(all(support[5:8, 2]) && !any(support[c(1:4, 9:12), 2]))
  expect_equal(unname(colSums(fit$loadings^2)), c(1, 1))
  expect_true(all(1:10 %in% which(fit$outlier_type != "regular")))
  expect_output(print(fit), "Non-zero loadings")
  # no sparsity without penalty
  dense <- rospca(x, k = 2, lambda = 0)
  expect_gt(sum(dense$loadings != 0), sum(support))
})

test_that("macropca wraps cellWise::MacroPCA and predicts with missing values", {
  set.seed(13)
  x <- matrix(rnorm(60 * 8), 60, 8) %*% diag(8:1)
  x[1:3, ] <- x[1:3, ] + 20
  x[10, 2] <- 40
  x[12, 5] <- NA
  fit <- macropca(x, k = 2)
  ref <- cellWise::MacroPCA(x, k = 2, MacroPCApars = list(alpha = 0.5, silent = TRUE))
  expect_equal(fit$sd, unname(ref$SD))
  expect_equal(fit$od, unname(ref$OD))
  expect_true(fit$flagged_cells[10, 2])
  expect_true(all(1:3 %in% which(fit$outlier_type != "regular")))
  new <- x[1:5, ]
  new[2, 3] <- NA
  pred <- predict(fit, new)
  expect_equal(nrow(pred), 5L)
  expect_false(anyNA(pred$PC1))
})

test_that("robust PCA functions validate their inputs", {
  d <- make_lowrank(seed = 14)
  expect_error(robpca(d$x, alpha = 0.3), "alpha")
  x_na <- d$x
  x_na[1, 1] <- NA
  expect_error(robpca(x_na), "macropca")
  expect_error(rospca(d$x, k = 2, lambda = -1), "lambda")
  expect_error(predict(robpca(d$x, k = 2), d$x[, -1]), "columns")
})

test_that("plot_outlier_map and plot_cell_map return ggplots", {
  d <- make_lowrank(seed = 15)
  set.seed(16)
  fit <- robpca(d$x[-(1:20), ], k = 3)
  p <- plot_outlier_map(fit, newdata = d$x[1:20, ])
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[3]]), nrow(d$x))
  expect_error(plot_outlier_map(list()), "robpca")

  set.seed(17)
  x <- matrix(rnorm(40 * 8), 40, 8) %*% diag(8:1)
  x[10, 2] <- 40
  mfit <- macropca(x, k = 2)
  expect_s3_class(plot_outlier_map(mfit), "ggplot")
  expect_s3_class(plot_cell_map(mfit), "ggplot")
  expect_s3_class(plot_cell_map(mfit, columns = 1:6, ncolumnsinblock = 2), "ggplot")
  expect_error(plot_cell_map(fit), "macropca")
})
