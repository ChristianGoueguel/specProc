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
  skip_on_cran()
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

test_that("robust PCA with k equal to the rank reduces to the reweighted MCD", {
  set.seed(14)
  x <- matrix(rnorm(500), 100, 5)
  x[1:5, ] <- x[1:5, ] + 6
  set.seed(15)
  fit <- robpca(x, k = 5)
  # the subspace is the whole space: no orthogonal distances or outliers
  expect_identical(fit$H1, fit$H0)
  expect_true(all(fit$od == 0))
  expect_identical(fit$cutoff_od, 0)
  expect_true(all(fit$outlier_type %in% c("regular", "good leverage")))
  expect_true(all(fit$outlier_type[1:5] == "good leverage"))
  mcd <- fast_mcd_cpp(x, fit$h, 500L)
  expect_equal(unname(fit$center), mcd$center, tolerance = 1e-6)
  expect_equal(fit$eigenvalues, eigen(mcd$cov, symmetric = TRUE)$values, tolerance = 1e-6)
  pred <- predict(fit, x)
  expect_true(all(pred$od == 0))
  expect_equal(pred$outlier_type, fit$outlier_type)
  # also when k is chosen
  set.seed(15)
  expect_equal(robpca(x, var_explained = 1)$k, 5L)

  sparse <- rospca(x, k = 5)
  expect_identical(sparse$H1, sparse$H0)
  expect_identical(sparse$H2, sparse$H1)
  expect_identical(sparse$cutoff_od, 0)
  expect_true(all(sparse$outlier_type[1:5] == "good leverage"))
  expect_false(any(macropca(x, k = 5)$outlier_type %in% c("orthogonal outlier", "bad leverage")))

  # more variables than observations: the rank is n - 1, and new observations
  # off the subspace are orthogonal outliers
  set.seed(16)
  wide <- matrix(rnorm(10 * 30), 10, 30)
  fit <- robpca(wide, k = 9)
  expect_true(all(fit$od == 0))
  expect_identical(fit$cutoff_od, 0)
  expect_true(all(predict(fit, matrix(rnorm(2 * 30), 2, 30))$od > 0))
})

test_that("orthogonal distances and their cut-off do not depend on the scale of the data", {
  d <- make_lowrank(seed = 17)
  xc <- sweep(d$x, 2, colMeans(d$x))
  od <- orthogonal_distance(xc, d$loadings)
  expect_equal(orthogonal_distance(xc * 1e-10, d$loadings) / 1e-10, od)
  expect_equal(od_cutoff(od * 1e-10, 90) / 1e-10, od_cutoff(od, 90))
  # rounding errors are zero distances, with a zero cut-off
  expect_true(all(orthogonal_distance(xc, svd(xc)$v) == 0))
  expect_identical(od_cutoff(rep(0, 10), 8), 0)
})

test_that("macropca detects outlying rows and cells and predicts with missing values", {
  set.seed(13)
  x <- matrix(rnorm(60 * 8), 60, 8) %*% diag(8:1)
  x[1:3, ] <- x[1:3, ] + 20
  x[10, 2] <- 40
  x[12, 5] <- NA
  fit <- macropca(x, k = 2)
  expect_s3_class(fit, c("specproc_macropca", "specproc_robpca"))
  expect_true(fit$flagged_cells[10, 2])
  expect_true(all(c(1:3, 10) %in% which(fit$outlier_type != "regular")))
  # the missing cell is imputed and has no residual
  expect_false(anyNA(fit$imputed))
  expect_true(is.na(fit$std_resid[12, 5]))
  expect_false(fit$flagged_cells[12, 5])
  # orthonormal loadings, eigenvalues in decreasing order
  expect_equal(crossprod(fit$loadings), diag(2), tolerance = 1e-8, ignore_attr = TRUE)
  expect_true(!is.unsorted(rev(fit$eigenvalues)))
  new <- x[1:5, ]
  new[2, 3] <- NA
  pred <- predict(fit, new)
  expect_equal(nrow(pred), 5L)
  expect_false(anyNA(pred$PC1))
  # new data identical to calibration rows get (nearly) the same distances
  expect_equal(predict(fit, x[20:30, ])$od, fit$od[20:30], tolerance = 1e-6)
})

test_that("macropca reproduces cellWise::MacroPCA", {
  skip_on_cran()
  skip_if_not_installed("cellWise")
  set.seed(13)
  x <- matrix(rnorm(60 * 8), 60, 8) %*% diag(8:1)
  x[1:3, ] <- x[1:3, ] + 20
  x[10, 2] <- 40
  x[12, 5] <- NA
  fit <- macropca(x, k = 2)
  ref <- cellWise::MacroPCA(x, k = 2, MacroPCApars = list(alpha = 0.5, silent = TRUE, scale = FALSE))
  expect_equal(abs(fit$loadings), abs(ref$loadings), tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(fit$eigenvalues, ref$eigenvalues, tolerance = 1e-8)
  expect_equal(fit$center, ref$center, tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(fit$od, ref$OD, tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(fit$sd, ref$SD, tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(fit$cutoff_od, ref$cutoffOD, tolerance = 1e-8)
  expect_equal(which(fit$flagged_cells), sort(ref$indcells))
  # new data, as MacroPCApredict
  new <- x[c(1, 10, 12, 20:25), ]
  new[4, 6] <- NA
  pred <- predict(fit, new)
  ref_pred <- cellWise::MacroPCApredict(new, ref)
  expect_equal(pred$od, ref_pred$OD, tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(pred$sd, ref_pred$SD, tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("macropca follows the steps of the paper", {
  set.seed(13)
  x <- matrix(rnorm(60 * 8), 60, 8) %*% diag(8:1)
  x[1:3, ] <- x[1:3, ] + 20
  fit <- macropca(x, k = 2)
  # rows flagged by DDC, and the 99% cut-offs of the paper
  expect_type(fit$flagged_rows, "logical")
  expect_length(fit$flagged_rows, 60L)
  expect_equal(fit$cutoff_sd, sqrt(stats::qchisq(0.99, 2)))
  expect_equal(h_alpha_n(0.5, 60, 2), 31)
  # the scores are those of the data (no missing cells to impute)
  expect_equal(fit$scores, sweep(x, 2, fit$center) %*% fit$loadings, ignore_attr = TRUE)
  expect_equal(max_angle(diag(8)[, 1:2], diag(8)[, 1:2]), 0)
})

test_that("macropca chooses k from the explained variance", {
  set.seed(13)
  x <- matrix(rnorm(60 * 8), 60, 8) %*% diag(8:1)
  x[1:3, ] <- x[1:3, ] + 20
  devices <- grDevices::dev.list()
  expect_silent(fit <- macropca(x))
  expect_identical(grDevices::dev.list(), devices)
  # variances 64, 49, 36, 25, ... of a total of 204: 4 components for 80%
  expect_equal(fit$k, 4L)
  # kmax components when they explain less than var_explained
  expect_equal(macropca(x, kmax = 2, var_explained = 0.99)$k, 2L)
  expect_error(macropca(x, var_explained = 0), "var_explained")
  # scale = TRUE scales the variables by their robust scale
  scaled <- macropca(x, k = 2, scale = TRUE)
  expect_true(all(scaled$scale != 1))
  expect_false(isTRUE(all.equal(scaled$od, macropca(x, k = 2)$od)))
})

test_that("ddc reproduces cellWise::DDC", {
  skip_on_cran()
  skip_if_not_installed("cellWise")
  set.seed(4)
  f <- matrix(rnorm(80 * 3), 80) %*% matrix(rnorm(3 * 15), 3)
  x <- f + matrix(rnorm(80 * 15, sd = 0.2), 80)
  cells <- matrix(runif(80 * 15) < 0.05, 80)
  x[cells] <- x[cells] + 6
  x[3, ] <- x[3, ] + 4
  x[matrix(runif(80 * 15) < 0.03, 80)] <- NA
  colnames(x) <- paste0("V", 1:15)
  d <- ddc(x)
  ref <- suppressMessages(cellWise::DDC(x, list(silent = TRUE)))
  expect_equal(which(d$flagged_cells), sort(ref$indcells))
  expect_equal(which(d$flagged_rows), sort(ref$indrows))
  expect_equal(d$std_resid, ref$stdResid, tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(d$imputed, ref$Ximp, tolerance = 1e-8, ignore_attr = TRUE)
  # new data, as DDCpredict
  pred <- predict_ddc(d, x[1:10, ])
  ref_pred <- cellWise::DDCpredict(x[1:10, ], ref)
  expect_equal(which(pred$flagged_cells), sort(ref_pred$indcells))
  expect_equal(pred$imputed, ref_pred$Ximp, tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("ddc flags deviating cells and rows and imputes them", {
  set.seed(1)
  f <- matrix(rnorm(100 * 3), 100) %*% matrix(rnorm(3 * 30), 3)
  x <- f + matrix(rnorm(100 * 30, sd = 0.1), 100)
  x[5, 3] <- x[5, 3] + 8
  x[20, 10] <- x[20, 10] - 6
  x[40, ] <- x[40, ] + stats::rnorm(30, sd = 3)
  x[7, 4] <- NA
  d <- ddc(x)
  expect_true(d$flagged_cells[5, 3])
  expect_true(d$flagged_cells[20, 10])
  expect_true(d$flagged_rows[40])
  expect_false(d$flagged_cells[7, 4])
  # the missing and flagged cells are imputed close to the underlying values
  expect_lt(abs(d$imputed[7, 4] - f[7, 4]), 0.5)
  expect_lt(abs(d$imputed[5, 3] - f[5, 3]), 1)
  # a constant column is never flagged
  x[, 30] <- 1
  expect_false(any(ddc(x)$flagged_cells[, 30]))
})

test_that("robust PCA does not depend on the scale of the data", {
  set.seed(1)
  x <- matrix(rnorm(500), 100, 5) %*% diag(c(5, 3, 1, 0.1, 0.1))
  x[1:4, 3] <- x[1:4, 3] + 6     # orthogonal outliers
  x[5:7, 1] <- x[5:7, 1] + 25    # good leverage points
  s <- 1e-8
  set.seed(2)
  fit <- robpca(x, k = 2)
  set.seed(2)
  small <- robpca(x * s, k = 2)
  expect_identical(small$H0, fit$H0)
  expect_identical(small$H1, fit$H1)
  expect_identical(small$outlier_type, fit$outlier_type)
  expect_true(all(c("good leverage", "orthogonal outlier") %in% fit$outlier_type))
  expect_equal(small$eigenvalues, fit$eigenvalues * s^2)
  expect_equal(abs(small$loadings), abs(fit$loadings))
  expect_equal(small$center, fit$center * s)
  expect_equal(small$sd, fit$sd)
  expect_equal(small$od, fit$od * s)
  expect_equal(small$cutoff_od, fit$cutoff_od * s)

  # rospca, on block-sparse data (standardized, the sparse loadings are unit-free)
  set.seed(3)
  y <- cbind(rnorm(100, sd = 3) + matrix(rnorm(300, sd = 0.5), 100),
             rnorm(100, sd = 2) + matrix(rnorm(300, sd = 0.5), 100), matrix(rnorm(300), 100))
  y[1:5, ] <- y[1:5, ] + 8
  set.seed(4)
  fit <- rospca(y, k = 2)
  set.seed(4)
  small <- rospca(y * s, k = 2)
  expect_identical(small[c("H0", "H1", "H2", "H3", "outlier_type")], fit[c("H0", "H1", "H2", "H3", "outlier_type")])
  expect_equal(small$loadings, fit$loadings)
  expect_equal(small$scale, fit$scale * s)

  # macropca and DDC (deterministic)
  fit <- macropca(x, k = 2)
  small <- macropca(x * s, k = 2)
  expect_identical(small$flagged_cells, fit$flagged_cells)
  expect_identical(small$outlier_type, fit$outlier_type)
  expect_equal(small$eigenvalues, fit$eigenvalues * s^2)
  expect_equal(small$od, fit$od * s)
  x[, 5] <- 2  # a constant variable
  expect_equal(ddc(x * 1e-14)$std_resid, ddc(x)$std_resid)
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

  # relative distances put both cut-offs at 1
  rel <- ggplot2::ggplot_build(plot_outlier_map(fit, relative = TRUE))
  expect_equal(rel$data[[1]]$xintercept, 1)
  expect_equal(rel$data[[2]]$yintercept, 1)
  expect_equal(rel$data[[3]]$x, fit$sd / fit$cutoff_sd)
  expect_equal(rel$data[[3]]$y, fit$od / fit$cutoff_od)

  # shading adds the three outlying regions and their names before the points
  sh <- ggplot2::ggplot_build(plot_outlier_map(fit, shade = TRUE))
  expect_equal(nrow(sh$data[[1]]), 3)
  # the orthogonal outlier region stops at the score distance cut-off
  expect_equal(sh$data[[1]]$xmax[2], fit$cutoff_sd)
  expect_equal(nrow(sh$data[[2]]), 3)
  expect_equal(nrow(sh$data[[5]]), nrow(fit$scores))
  expect_s3_class(plot_outlier_map(fit, relative = TRUE, shade = TRUE, log = TRUE,
                                   newdata = d$x[1:20, ]), "ggplot")
  expect_no_warning(ggplot2::ggplot_build(plot_outlier_map(fit, shade = TRUE, log = TRUE)))
  expect_error(plot_outlier_map(fit, shade = NA), "shade")

  # a size per sample is mapped, with the expression as the legend title
  conc <- seq_along(fit$od)
  sized <- plot_outlier_map(fit, size = conc)
  expect_equal(sized$scales$get_scales("size")$name, "conc")
  expect_equal(ggplot2::ggplot_build(sized)$data[[3]]$size[which.max(conc)], 6)
  expect_error(plot_outlier_map(fit, size = 1:3), "one value per sample")
  # compact legends: three sizes, and no key for the kind of samples without new ones
  expect_equal(length(ggplot2::ggplot_build(sized)$plot$scales$get_scales("size")$get_breaks()), 3)
  expect_equal(sized$scales$get_scales("shape")$guide, "none")
  expect_false(identical(plot_outlier_map(fit, newdata = d$x[1:20, ])$scales$get_scales("shape")$guide, "none"))

  # points colored by their reduced distance from the origin, farthest on top
  rad <- plot_outlier_map(fit, colour_by = "distance", relative = TRUE)
  expect_s3_class(rad$scales$get_scales("fill"), "ScaleContinuous")
  radius <- pmax(fit$sd / fit$cutoff_sd, fit$od / fit$cutoff_od)
  expect_equal(rad$data$radius, sort(radius))
  # yellow at the cut-offs: a distance of 1
  sc <- ggplot2::ggplot_build(rad)$plot$scales$get_scales("fill")
  expect_equal(toupper(sc$map(1)), toupper(radial_rainbow[5]))
  expect_equal(toupper(sc$map(0)), toupper(radial_rainbow[1]))              # dark red at the origin
  expect_true(1 %in% sc$get_breaks())
  # on the raw distances, two cut-offs of different scales: no anchor at 1
  raw <- plot_outlier_map(fit, colour_by = "distance")
  sr <- ggplot2::ggplot_build(raw)$plot$scales$get_scales("fill")
  expect_equal(sr$get_limits(), range(radius))
  expect_false(any(grepl("cut-off", sr$get_labels())))
  expect_error(plot_outlier_map(fit, colour_by = "od"), "arg")

  # point styling is passed to geom_point(), over the defaults
  styled <- ggplot2::ggplot_build(plot_outlier_map(fit, alpha = 0.4, size = 3, color = "red"))
  expect_equal(unique(styled$data[[3]]$alpha), 0.4)
  expect_equal(unique(styled$data[[3]]$size), 3)
  expect_equal(unique(styled$data[[3]]$colour), "red")
  expect_equal(unique(styled$data[[3]]$stroke), 0.4)
  expect_error(plot_outlier_map(fit, NULL, 3, FALSE, FALSE, FALSE, "type", NULL, 0.5), "named")

  set.seed(17)
  x <- matrix(rnorm(40 * 8), 40, 8) %*% diag(8:1)
  x[10, 2] <- 40
  mfit <- macropca(x, k = 2)
  expect_s3_class(plot_outlier_map(mfit), "ggplot")
  skip_if_not_installed("patchwork")
  expect_s3_class(plot_cell_map(mfit), "ggplot")
  expect_s3_class(plot_cell_map(mfit, columns = 1:6, order = "od", profile = FALSE), "ggplot")
  # rows without flagged cells keep the extent of the map
  expect_s3_class(plot_cell_map(mfit, rows = c("1", "10")), "ggplot")
  grDevices::pdf(NULL)
  expect_no_warning(print(plot_cell_map(mfit, rows = 1:2)))
  grDevices::dev.off()
  expect_error(plot_cell_map(fit), "macropca")
  expect_error(plot_cell_map(mfit, resolution = 10), "resolution")
  expect_error(plot_cell_map(mfit, rows = 0), "rows")
  expect_error(plot_cell_map(mfit, columns = "nope"), "columns")
  expect_error(plot_cell_map(mfit, threshold = 0), "threshold")
  expect_error(plot_cell_map(mfit, labels = -1), "labels")
  expect_error(plot_cell_map(mfit, order = "name"), "arg")
})

# Spectra with a region (channels 8-9) flagged in the first 15 observations
make_flagged_spectra <- function() {
  set.seed(21)
  wl <- seq(400, by = 0.1, length.out = 20)
  line <- function(center) exp(-(wl - center)^2 / 0.02)
  x <- outer(stats::rnorm(60, 10, 2), 50 * line(400.3) + 5) +
    outer(stats::rnorm(60, 5, 1), 30 * line(401.5) + 3) +
    matrix(stats::rnorm(60 * 20, sd = 0.5), 60)
  colnames(x) <- wl
  x[1:15, 8:9] <- x[1:15, 8:9] + 20
  x
}

test_that("flagged_regions finds the channels flagged in many observations", {
  x <- make_flagged_spectra()
  fit <- macropca(x, k = 2)
  regions <- flagged_regions(fit, threshold = 0.2)
  expect_s3_class(regions, "tbl_df")
  expect_named(regions, c("start", "end", "peak", "channels", "share", "mean_share", "direction"))
  expect_equal(nrow(regions), 1)
  expect_equal(c(regions$start, regions$end), c(400.7, 400.8))
  # the 15 shifted observations, and at most a stray flagged cell
  expect_gte(regions$share, 15 / 60)
  expect_lte(regions$share, 16 / 60)
  expect_true(all(fit$flagged_cells[1:15, 8]))
  expect_equal(regions$channels, 2L)
  expect_equal(regions$direction, "higher")
  # the share is over the selected rows
  expect_gte(flagged_regions(fit, threshold = 0.2, rows = 1:30)$share[1], 15 / 30)
  expect_equal(nrow(flagged_regions(fit, threshold = 0.9)), 0)

  lines <- tibble::tibble(species = c("Ca II", "K I"), stage = c(2L, 1L),
                          wavelength = c(400.75, 405), relative_intensity = 1)
  matched <- flagged_regions(fit, threshold = 0.2, lines = lines)
  expect_equal(matched$species, "Ca II")
  expect_equal(matched$candidates, "Ca II 400.75")
  expect_error(flagged_regions(fit, threshold = 2), "threshold")
  expect_error(flagged_regions(list()), "macropca")
})

test_that("runs of flagged channels are merged across short gaps and split at segments", {
  cells <- list(flagged = matrix(FALSE, 10, 12), resid = matrix(3, 10, 12),
                wavelength = c(seq(400, by = 0.1, length.out = 6), seq(500, by = 0.1, length.out = 6)),
                segment = rep(1:2, each = 6))
  cells$flagged[1:5, c(1, 3, 6, 7)] <- TRUE        # gap of 1, then of 2, then a new segment
  regions <- find_flagged_regions(cells, 0.5, NULL, 0.1)
  expect_equal(nrow(regions), 2)
  expect_equal(sort(regions$start), c(400, 500))
  expect_equal(regions$channels[regions$start == 400], 6)
})

test_that("plot_cell_map clusters the rows and draws the mean spectrum", {
  skip_if_not_installed("patchwork")
  x <- make_flagged_spectra()
  fit <- macropca(x, k = 2)
  # the rows flagged in the same region are grouped
  cell <- ifelse(fit$flagged_cells, sign(fit$std_resid), 0)
  o <- cell_map_cluster(cell, cell_map_columns(rep(1L, 20), 20, 400))
  expect_setequal(o, 1:60)
  expect_lte(diff(range(match(1:15, o))), 15 + 3)
  p <- plot_cell_map(fit, order = "cluster", threshold = 0.2)
  expect_s3_class(p, "ggplot")
  # the profile has the mean spectrum (raw data), not with centered data
  has_line <- function(p) any(vapply(p[[1]]$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  expect_true(has_line(p))
  centered <- macropca(sweep(x, 2, colMeans(x)), k = 2)
  expect_false(has_line(plot_cell_map(centered)))
  expect_true(has_line(plot_cell_map(centered, spectra = x)))
  expect_error(plot_cell_map(centered, spectra = x[, 1:3]), "variables")
})

test_that("cell map blocks average the cells within detector segments", {
  cell <- matrix(0, 4, 6)
  cell[1, 1] <- 1
  cell[4, 6] <- -0.5
  wl <- c(400, 400.1, 400.2, 500, 500.1, 500.2)          # two segments
  col_id <- cell_map_columns(wavelength_segments(wl), 6, max_blocks = 2)
  expect_equal(col_id, c(1, 1, 1, 2, 2, 2))
  blocks <- cell_map_blocks(cell, wl, col_id, max_rows = 2)
  # 2 rows and 3 channels per block, but blocks do not cross the gap
  expect_equal(nrow(blocks), 2)
  expect_equal(blocks$value, c(1 / 6, -0.5 / 6))
  expect_equal(blocks$xmin, c(399.95, 499.95))
  expect_equal(blocks$xmax, c(400.25, 500.25))
  expect_equal(blocks$ymin, c(0.5, 2.5))
  expect_equal(blocks$fill, sign(blocks$value) * sqrt(abs(blocks$value)))
  # small maps show every cell
  single <- cell_map_blocks(cell, wl, cell_map_columns(wavelength_segments(wl), 6, 400),
                            max_rows = 200)
  expect_equal(sort(single$value), c(-0.5, 1))
})
