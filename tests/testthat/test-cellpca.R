# Spectra-like data of rank 3 with cellwise outliers, casewise outliers and
# missing cells
make_contaminated <- function(seed = 1, n = 150, p = 30) {
  set.seed(seed)
  V <- qr.Q(qr(matrix(stats::rnorm(p * 3), p, 3)))
  clean <- matrix(stats::rnorm(n * 3), n, 3) %*% diag(c(10, 7, 5)) %*% t(V) +
    matrix(stats::rnorm(n * p, sd = 0.5), n, p)
  x <- clean
  cells <- matrix(stats::runif(n * p) < 0.05, n, p)
  x[cells] <- x[cells] + sample(c(-1, 1), sum(cells), TRUE) * 8
  cases <- 1:10
  x[cases, ] <- x[cases, ] + matrix(stats::rnorm(10 * p, 6), 10)
  x[matrix(stats::runif(n * p) < 0.03, n, p)] <- NA
  colnames(x) <- paste0("V", seq_len(p))
  list(x = x, V = V, cells = cells, cases = cases)
}

subspace_angle_deg <- function(a, b) {
  acos(min(1, min(svd(crossprod(qr.Q(qr(a)), qr.Q(qr(b))))$d))) * 180 / pi
}

test_that("the tuning constants of rho_2 are those of the reference code", {
  # get_tuning_const_rho2(30) of Centofanti, Hubert and Rousseeuw
  expect_equal(cellpca_rho2_constants(30), c(0.7722239, 1.02778, 2.350072, 1.335335),
               tolerance = 1e-6)
  # the M-scale is consistent at the normal distribution
  set.seed(2)
  expect_equal(scale_tanh_cols_cpp(matrix(stats::rnorm(20000, sd = 3))), 3, tolerance = 0.03)
})

test_that("cellpca recovers the subspace and weights the outliers", {
  d <- make_contaminated()
  fit <- cellpca(d$x, k = 3, od_cutoff = "chisq")
  expect_s3_class(fit, c("specproc_cellpca", "specproc_robpca"))
  expect_true(fit$converged)
  expect_lt(subspace_angle_deg(fit$loadings, d$V), 5)
  # far better than classical PCA (of the data with the missing cells filled)
  filled <- d$x
  filled[is.na(filled)] <- fit$fitted[is.na(filled)]
  classical <- stats::prcomp(filled)$rotation[, 1:3]
  expect_gt(subspace_angle_deg(classical, d$V), 3 * subspace_angle_deg(fit$loadings, d$V))
  # casewise outliers get small weights, regular cases weight 1
  expect_true(all(fit$case_weights[d$cases] < 0.5))
  expect_gt(stats::median(fit$case_weights[-d$cases]), 0.99)
  expect_true(all(fit$outlier_type[d$cases] != "regular"))
  # outlying cells of the regular cases are flagged and get small weights
  regular <- matrix(!(seq_len(nrow(d$x)) %in% d$cases), nrow(d$x), ncol(d$x))
  outlying <- d$cells & regular & !is.na(d$x)
  expect_gt(mean(fit$flagged_cells[outlying]), 0.9)
  expect_lt(stats::median(fit$cell_weights[outlying]), 0.1)
  expect_true(all(fit$cell_weights[is.na(d$x)] == 0))
  expect_output(print(fit), "cellPCA")
})

test_that("cellpca decreases its objective and has a consistent output", {
  d <- make_contaminated(3)
  fit <- cellpca(d$x, k = 2, od_cutoff = "chisq")
  expect_true(all(diff(fit$objective) <= 1e-8 * fit$objective[1]))
  expect_equal(crossprod(fit$loadings), diag(2), tolerance = 1e-8, ignore_attr = TRUE)
  expect_false(is.unsorted(rev(fit$eigenvalues)))
  expect_true(all(apply(fit$loadings, 2, function(v) v[which.max(abs(v))] > 0)))
  fitted <- sweep(fit$scores %*% t(fit$loadings), 2, fit$center, "+")
  expect_equal(fit$fitted, fitted, ignore_attr = TRUE)
  # imputation: the fit in missing cells, the data in cells of weight 1
  miss <- is.na(d$x)
  expect_equal(fit$imputed[miss], fit$fitted[miss])
  full <- !miss & fit$cell_weights == 1
  expect_equal(fit$imputed[full], d$x[full])
  expect_true(all(fit$cell_weights >= 0 & fit$cell_weights <= 1))
  expect_true(all(fit$case_weights >= 0 & fit$case_weights <= 1))
})

test_that("cellpca predicts new data, with missing and outlying cells", {
  d <- make_contaminated(4)
  fit <- cellpca(d$x, k = 3, od_cutoff = "chisq")
  kept <- which(fit$case_weights > 0)[1:20]
  pred <- predict(fit, d$x[kept, ])
  expect_named(pred, c("PC1", "PC2", "PC3", "sd", "od", "outlier_type"))
  expect_equal(as.matrix(pred[1:3]), fit$scores[kept, ], tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(pred$od, fit$od[kept], tolerance = 1e-4)
  # an outlying cell barely moves the scores of a new case
  new <- d$x[kept[1:3], ]
  new[is.na(new)] <- 0
  shifted <- new
  shifted[, 5] <- shifted[, 5] + 50
  delta <- as.matrix(predict(fit, shifted)[1:3]) - as.matrix(predict(fit, new)[1:3])
  expect_lt(max(abs(delta)), 0.5)
  all_missing <- d$x[1:2, ]
  all_missing[1, ] <- NA
  expect_true(all(is.na(unlist(predict(fit, all_missing)[1, 1:3]))))
  expect_error(predict(fit, d$x[, 1:5]), "30 columns")
})

test_that("cellpca chooses k, simulates the cut-off and works with the plots and recipes", {
  d <- make_contaminated(5, n = 80, p = 20)
  fit <- cellpca(d$x)
  expect_true(fit$k >= 1)
  expect_true(is.finite(fit$cutoff_od) && fit$cutoff_od > 0)
  expect_s3_class(plot_outlier_map(fit), "ggplot")
  skip_if_not_installed("patchwork")
  expect_s3_class(plot_cell_map(fit), "patchwork")
  skip_if_not_installed("recipes")
  df <- as.data.frame(d$x)
  rec <- recipes::recipe(~ ., data = df) |>
    step_cellpca(recipes::all_predictors(), num_comp = 2, distances = TRUE,
                 options = list(od_cutoff = "chisq"))
  baked <- recipes::bake(recipes::prep(rec), new_data = df[1:5, ])
  expect_named(baked, c("CPC1", "CPC2", "CPC_SD", "CPC_OD"))
  expect_false(anyNA(baked))
})
