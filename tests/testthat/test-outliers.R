set.seed(7)
x <- c(stats::rnorm(100), 15, -12)

test_that("zscore matches base scale() and flags outliers", {
  res <- zscore(x)
  expect_named(res, c("data", "score", "flag"))
  expect_equal(res$score[match(x, res$data)], as.numeric(scale(x)))
  expect_true(all(res$flag[res$data %in% c(15, -12)]))
  rob <- zscore(x, robust = TRUE)
  expect_equal(rob$score[rob$data == 15], (15 - stats::median(x)) / stats::mad(x))
  expect_equal(sum(is.na(zscore(c(x, NA))$score)), 1)
  expect_error(zscore("a"), "numeric")
  expect_error(zscore(x, cutoff = -1), "cutoff")
})

test_that("iqr_outliers flags values outside the fences", {
  res <- iqr_outliers(x)
  expect_true(all(res$flag[res$data %in% c(15, -12)]))
  q <- stats::quantile(x, c(0.25, 0.75))
  fence <- q[2] + 1.5 * diff(q)
  expect_equal(sum(res$flag & res$data > 0), sum(x > fence))
  expect_warning(iqr_outliers(x, k = 3, skew = TRUE), "only defined for k = 1.5")
  set.seed(1)
  skewed <- stats::rexp(200)
  expect_lte(sum(iqr_outliers(skewed, skew = TRUE)$flag), sum(iqr_outliers(skewed)$flag))
  expect_equal(nrow(iqr_outliers(c(x, NA))), length(x) + 1)
  expect_error(iqr_outliers(x, k = 0), "positive")
})

test_that("directional_outlyingness flags gross outliers", {
  y <- c(1, 5, 3, 9, 2, 6, 4, 8, 7, 1e3)
  res <- directional_outlyingness(y)
  expect_named(res, c("data", "score", "flag"))
  expect_true(res$flag[res$data == 1e3])
  expect_equal(sum(res$flag), 1)
  expect_no_error(res2 <- directional_outlyingness(y, maxRatio = 3))
  expect_true(res2$flag[res2$data == 1e3])
  expect_error(directional_outlyingness(y, maxRatio = 1), "at least 2")
  expect_error(directional_outlyingness("a"), "numeric")
})

test_that("generalized_boxplot estimates g and h and sensible fences", {
  set.seed(3)
  df <- data.frame(normal = stats::rnorm(5000), skewed = stats::rexp(5000))
  res <- generalized_boxplot(df, alpha = 0.05, plot = FALSE)
  st <- res$stats
  expect_lt(abs(st$g[1]), 0.05)
  expect_lt(st$h[1], 0.1)
  expect_gt(st$g[2], 0.05)
  # For normal data, the fences are close to the alpha/2 quantiles.
  expect_equal(st$lower_fence[1], stats::qnorm(0.025), tolerance = 0.1)
  expect_equal(st$upper_fence[1], stats::qnorm(0.975), tolerance = 0.1)
  # whiskers end at observations inside the fences
  expect_true(all(st$lower >= st$lower_fence & st$upper <= st$upper_fence))
  expect_true(st$upper[1] %in% df$normal)
  expect_true(all(res$outliers$out %in% c("lower", "upper")))
  expect_s3_class(generalized_boxplot(df[1:200, ]), "ggplot")
})

test_that("generalized_boxplot handles unequal numbers of outliers per tail", {
  set.seed(4)
  df <- data.frame(a = c(stats::rnorm(100), 20, 25, 30))
  res <- generalized_boxplot(df, plot = FALSE)
  expect_true(all(c(20, 25, 30) %in% res$outliers$value))
  expect_error(generalized_boxplot(df, alpha = 2), "alpha")
  expect_error(generalized_boxplot(df, p = 0.2), "'p'")
})

test_that("plot_outliers returns plots and data without touching the RNG", {
  set.seed(5)
  m <- matrix(stats::rnorm(200), 50, 4, dimnames = list(NULL, paste0("v", 1:4)))
  m[1, ] <- 10
  set.seed(99)
  before <- .Random.seed
  p <- plot_outliers(m)
  expect_identical(.Random.seed, before)
  expect_s3_class(p, "ggplot")
  expect_s3_class(plot_outliers(m, color_by = "distance"), "ggplot")
  expect_s3_class(plot_outliers(m, color_by = "both"), "ggplot")
  expect_s3_class(plot_outliers(m, type = "distance", label_outliers = TRUE), "ggplot")
  res <- plot_outliers(m, plot = FALSE)
  expect_named(res, c("row", "mahalanobis", "cutoff", "outlier", "weight", paste0("v", 1:4)))
  expect_true(res$outlier[1])
  expect_error(plot_outliers(m[, 1, drop = FALSE]), "two-dimensional")
  expect_error(plot_outliers(m, quan = 0.2), "quan")
  expect_error(plot_outliers(m, color_by = "red"), "should be one of")
  # the former show.outlier and show.mahal still work, with a warning
  lifecycle::expect_deprecated(plot_outliers(m, show.mahal = TRUE))
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_named(plot_outliers(m, show.outlier = FALSE), names(res))
  expect_s3_class(plot_outliers(m, show.outlier = FALSE, show.mahal = TRUE), "ggplot")
  expect_error(plot_outliers(m, show.outlier = "yes"), "show.outlier")
})

# arw() of the mvoutlier package (Filzmoser et al., 2005), as published.
mvoutlier_arw <- function(x, m0, c0, alpha) {
  n <- nrow(x)
  p <- ncol(x)
  pcrit <- if (p <= 10) (0.24 - 0.003 * p) / sqrt(n) else (0.252 - 0.0018 * p) / sqrt(n)
  delta <- stats::qchisq(1 - alpha, p)
  d2 <- stats::mahalanobis(x, m0, c0)
  d2ord <- sort(d2)
  dif <- stats::pchisq(d2ord, p) - (0.5:n) / n
  i <- (d2ord >= delta) & (dif > 0)
  alfan <- if (sum(i) == 0) 0 else max(dif[i])
  if (alfan < pcrit) alfan <- 0
  cn <- if (alfan > 0) max(d2ord[n - ceiling(n * alfan)], delta) else Inf
  w <- d2 < cn
  m <- apply(x[w, ], 2, mean)
  c1 <- as.matrix(x - rep(1, n) %*% t(m))
  list(m = m, c = (t(c1 * w) %*% c1) / sum(w), cn = cn, w = w)
}

test_that("plot_outliers flags the samples beyond the adaptive cutoff of Filzmoser et al.", {
  data(forageLIBS, package = "specProc", envir = environment())
  x <- as.matrix(forageLIBS[c("Ca", "Mg", "P", "K")])
  rob <- with_seed(123, robustbase::covMcd(x, alpha = 0.5))
  ref <- mvoutlier_arw(x, rob$center, rob$cov, alpha = 0.025)
  d2 <- stats::mahalanobis(x, rob$center, rob$cov)
  res <- plot_outliers(x, plot = FALSE)
  expect_true(is.finite(ref$cn))
  expect_equal(res$cutoff[1], sqrt(ref$cn))
  expect_equal(res$mahalanobis, unname(sqrt(d2)))
  expect_equal(res$outlier, unname(d2 > ref$cn)) # as aq.plot() of mvoutlier
  expect_equal(res$weight, as.numeric(ref$w))
  # robust z-scores from the reweighted location and scale
  expect_equal(res$Ca, unname((x[, "Ca"] - ref$m["Ca"]) / sqrt(ref$c["Ca", "Ca"])))
  # the fixed quantile flags more samples
  q <- plot_outliers(x, cutoff = "quantile", plot = FALSE)
  expect_equal(q$cutoff[1], sqrt(stats::qchisq(0.975, 4)))
  expect_equal(q$outlier, unname(d2 > stats::qchisq(0.975, 4)))
  expect_gt(sum(q$outlier), sum(res$outlier))
})

test_that("plot_outliers flags no sample of clean data with the adaptive cutoff", {
  set.seed(19)
  z <- data.frame(matrix(stats::rnorm(400 * 3), 400))
  res <- plot_outliers(z, plot = FALSE)
  expect_equal(sum(res$outlier), 0)
  expect_true(all(is.infinite(res$cutoff)))
  expect_gt(sum(plot_outliers(z, cutoff = "quantile", plot = FALSE)$outlier), 0)
  p <- plot_outliers(z, type = "distance")
  expect_match(p$labels$caption, "^No outliers among 400 samples")
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("plot_outliers names the samples and leaves out missing values", {
  set.seed(20)
  df <- data.frame(sample = paste0("s", 1:60), a = stats::rnorm(60), b = stats::rnorm(60),
                   c = stats::rnorm(60))
  df[5:8, c("a", "b", "c")] <- c(8, -8, 8)
  df$b[10] <- NA
  expect_message(res <- plot_outliers(df, id = sample, plot = FALSE), "1 row with missing values")
  expect_equal(nrow(res), 59)
  expect_false(10 %in% res$row)
  expect_equal(res$id, df$sample[res$row])
  expect_true(res$outlier[res$row == 5])
  p <- suppressMessages(plot_outliers(df, id = sample, label_outliers = TRUE, title = "Outliers"))
  # the labels are placed by ggrepel; the other samples have empty labels
  labels <- Filter(function(l) inherits(l$geom, "GeomTextRepel"), p$layers)[[1]]$data
  labels <- labels[labels$.label != "", ]
  expect_true("s5" %in% labels$.label)
  expect_true(all(abs(labels$score) > 2.5)) # only beyond the univariate limits
  expect_equal(p$labels$title, "Outliers")
  expect_equal(p$labels$y, "Robust z-score")
  expect_match(p$labels$caption, "1\\s+row\\s+with\\s+missing\\s+values\\s+left\\s+out")
  expect_error(plot_outliers(df, id = zz), "'id' column does not exist")
  d <- suppressMessages(plot_outliers(df, id = sample, type = "distance", label_outliers = TRUE))
  expect_equal(d$labels$y, "Robust distance")
  shown <- Filter(function(l) inherits(l$geom, "GeomTextRepel"), d$layers)[[1]]$data$.label
  expect_setequal(shown[shown != ""],
                  res$id[res$outlier])
})

test_that("plot_outliers needs several outliers for an adaptive cutoff", {
  set.seed(20)
  z <- data.frame(a = stats::rnorm(60), b = stats::rnorm(60), c = stats::rnorm(60))
  z[1, ] <- c(10, -10, 10)
  # a single gross outlier: no adaptive cutoff, but beyond the quantile
  expect_false(any(plot_outliers(z, plot = FALSE)$outlier))
  expect_true(plot_outliers(z, cutoff = "quantile", plot = FALSE)$outlier[1])
})
