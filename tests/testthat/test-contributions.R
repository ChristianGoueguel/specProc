make_contribution_data <- function(n = 50, seed = 3) {
  set.seed(seed)
  wl <- seq(400, 420, by = 0.2)
  line <- function(center) exp(-(wl - center)^2 / 0.1)
  x <- outer(stats::rnorm(n, 10, 2), 100 * line(405) + 5) +
    outer(stats::rnorm(n, 5, 1), 60 * line(415) + 2) +
    matrix(stats::rnorm(n * length(wl), sd = 0.5), n)
  colnames(x) <- format(wl, nsmall = 1, trim = TRUE)
  # sample 1: an extra line at 410 nm, outside the model
  x[1, ] <- x[1, ] + 40 * line(410)
  x
}

test_that("Q and T-squared contributions add up to the statistics of a prcomp fit", {
  x <- make_contribution_data()
  pca <- stats::prcomp(x)
  infl <- q_residuals(pca, k = 2)
  q <- contributions(pca, k = 2)
  expect_s3_class(q, "specproc_contributions")
  expect_equal(dim(q), c(nrow(x), ncol(x) + 1))
  expect_equal(names(q), c("sample", colnames(x)))
  expect_equal(rowSums(as.matrix(q[, -1])^2), infl$q, ignore_attr = TRUE)
  expect_equal(attr(q, "total"), infl$q)
  t2 <- contributions(pca, k = 2, statistic = "t2")
  expect_equal(rowSums(as.matrix(t2[, -1])^2), infl$t2, ignore_attr = TRUE)
  # the extra line of sample 1 is its largest Q contribution
  expect_equal(colnames(x)[which.max(abs(unlist(q[1, -1])))], "410.0")

  # data given explicitly: the same as the calibration contributions
  q_data <- contributions(pca, k = 2, data = x)
  expect_equal(as.matrix(q_data[, -1]), as.matrix(q[, -1]), ignore_attr = TRUE)
  # new samples: the Q of q_residuals(newdata)
  new <- x[1:5, ] + 1
  expect_equal(attr(contributions(pca, k = 2, data = new), "total"),
               q_residuals(pca, k = 2, newdata = new)$q)
})

test_that("relative contributions subtract the mean of the reference samples", {
  x <- make_contribution_data()
  pca <- stats::prcomp(x)
  q <- contributions(pca, k = 2)
  rel <- contributions(pca, k = 2, samples = 1:3, reference = 4:10)
  expected <- sweep(as.matrix(q[1:3, -1]), 2, colMeans(as.matrix(q[4:10, -1])))
  expect_equal(as.matrix(rel[, -1]), expected, ignore_attr = TRUE)
  expect_equal(attr(rel, "total"), attr(q, "total")[1:3])
  expect_equal(attr(rel, "reference"), as.character(4:10))
  # regular samples of q_residuals()
  regular <- which(q_residuals(pca, k = 2)$outlier == "regular")
  rel_regular <- contributions(pca, k = 2, samples = 1, reference = "regular")
  expected <- unlist(q[1, -1]) - colMeans(as.matrix(q[regular, -1]))
  expect_equal(unlist(rel_regular[1, -1]), expected, ignore_attr = TRUE)
  expect_equal(attr(rel_regular, "reference"), "regular")
  # samples by name
  rownames(x) <- paste0("s", seq_len(nrow(x)))
  named <- contributions(stats::prcomp(x), k = 2, samples = c("s2", "s1"))
  expect_equal(named$sample, c("s2", "s1"))
})

test_that("contributions of robust fits decompose their distances", {
  x <- make_contribution_data()
  set.seed(4)
  fit <- robpca(x, k = 2)
  q <- contributions(fit, data = x)
  expect_equal(attr(q, "total"), fit$od^2)
  t2 <- contributions(fit, statistic = "t2", data = x)
  expect_equal(attr(t2, "total"), fit$sd^2)
  # regular samples of the robust fit
  regular <- which(fit$outlier_type == "regular")
  rel <- contributions(fit, data = x, samples = 1, reference = "regular")
  expected <- unlist(q[1, -1]) - colMeans(as.matrix(q[regular, -1]))
  expect_equal(unlist(rel[1, -1]), expected, ignore_attr = TRUE)
  # fewer components than the fit
  expect_equal(attr(contributions(fit, k = 1, data = x), "k"), 1L)
  expect_error(contributions(fit, k = 3, data = x), "at most 2")
  expect_error(contributions(fit), "data")
  # a model fitted on centered data, given the raw data
  xc <- sweep(x, 2, colMeans(x))
  set.seed(4)
  fit_c <- robpca(xc, k = 2)
  expect_silent(contributions(fit_c, data = xc, reference = "regular"))
  expect_error(suppressWarnings(contributions(fit_c, data = x + 100, reference = "regular")),
               "preprocessed")
  expect_warning(contributions(fit_c, data = x + 100), "preprocessed")
  expect_warning(contributions(stats::prcomp(xc), k = 2, data = x + 100), "preprocessed")
  expect_silent(contributions(stats::prcomp(x), k = 2, data = x))

  mfit <- macropca(x, k = 2)
  expect_equal(nrow(contributions(mfit)), nrow(x))
  expect_equal(attr(contributions(mfit), "total"), attr(contributions(mfit, data = x), "total"))
})

test_that("contributions checks its arguments", {
  x <- make_contribution_data()
  pca <- stats::prcomp(x)
  expect_error(contributions(pca), "'k' is required")
  expect_error(contributions(pca, k = 60), "smaller")
  expect_error(contributions(list(), k = 2), "prcomp")
  expect_error(contributions(pca, k = 2, samples = 0), "samples")
  expect_error(contributions(pca, k = 2, reference = "s1"), "reference")
  expect_error(contributions(pca, k = 2, data = x[, 1:5]), "variables")
  expect_error(contributions(stats::prcomp(x, rank. = 3), k = 2), "all its components")
  expect_error(contributions(pca, k = 2, statistic = "dmodx"), "arg")
})

test_that("plot_contributions draws one panel per sample", {
  x <- make_contribution_data()
  q <- contributions(stats::prcomp(x), k = 2, reference = "regular")
  p <- plot_contributions(q)
  expect_s3_class(p, "ggplot")
  # default: the three samples with the largest Q, sample 1 first
  expect_equal(nlevels(p$data$panel), 3)
  expect_match(levels(p$data$panel)[1], "^Sample 1 \\(Q = ")
  # the long title is split: the reference is in the subtitle
  expect_equal(p$labels$title, "PCA Q contributions")
  expect_equal(p$labels$subtitle, "2 components, relative to the regular samples")
  lines <- tibble::tibble(species = "Fe I", stage = 1L, wavelength = 410.02, relative_intensity = 1)
  p <- plot_contributions(q, samples = 1, lines = lines, spectra = x)
  built <- ggplot2::ggplot_build(p)
  text_layer <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomTextRepel"), logical(1)))
  expect_true(any(grepl("Fe I 410.02", built$data[[text_layer]]$label)))
  t2 <- contributions(stats::prcomp(x), k = 2, statistic = "t2")
  expect_match(plot_contributions(t2, samples = 2)$labels$title, "T² contributions")
  expect_error(plot_contributions(list()), "contributions")
  expect_error(plot_contributions(q, samples = 100), "samples")
  skip_if_not_installed("plotly")
  expect_s3_class(plot_contributions(q, samples = 1:2, interactive = TRUE), "plotly")
})
