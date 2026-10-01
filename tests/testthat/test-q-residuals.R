q_data <- function(n = 60, seed = 1) {
  set.seed(seed)
  loadings <- matrix(stats::rnorm(3 * 8), 3, 8)
  x <- matrix(stats::rnorm(n * 3), n) %*% loadings + matrix(stats::rnorm(n * 8, sd = 0.2), n)
  colnames(x) <- paste0("v", 1:8)
  list(x = x, loadings = loadings)
}

test_that("q_residuals computes T-squared and Q of the calibration samples", {
  d <- q_data()
  pca <- stats::prcomp(d$x)
  res <- q_residuals(pca, k = 3, conf_level = c(0.95, 0.99))
  expect_s3_class(res, "specproc_influence")
  xs <- scale(d$x, scale = FALSE)
  p <- pca$rotation[, 1:3]
  expect_equal(res$q, unname(rowSums((xs - xs %*% p %*% t(p))^2)))
  expect_equal(res$t2, hotelling_t2(pca, k = 3)$t2)
  expect_equal(res$t2_limit_95[1], 3 * 59 / 57 * stats::qf(0.95, 3, 57))
  # a data matrix gives the same PCA
  expect_equal(q_residuals(d$x, 3)$q, res$q)
  expect_equal(attr(res, "k"), 3)
  expect_true(all(res$q_limit_99 > res$q_limit_95))
})

test_that("q_residuals flags new samples off the model", {
  d <- q_data()
  pca <- stats::prcomp(d$x)
  set.seed(3)
  new <- matrix(stats::rnorm(4 * 3), 4) %*% d$loadings + matrix(stats::rnorm(4 * 8, sd = 0.2), 4)
  colnames(new) <- colnames(d$x)
  new[2, ] <- new[2, ] + c(5, -5, 0, 0, 0, 0, 0, 0)          # off the plane
  new[3, ] <- 8 * new[3, ]                                   # far within the plane
  res <- q_residuals(pca, 3, newdata = new, conf_level = c(0.95, 0.99))
  expect_true(res$q[2] > res$q_limit_99[2])
  expect_equal(as.character(res$outlier[2]), "residual")
  expect_true(res$t2[3] > res$t2_limit_99[3])
  expect_equal(res$t2_limit_95[1], 3 * 61 * 59 / (60 * 57) * stats::qf(0.95, 3, 57))
  expect_true(attr(res, "new"))
  # columns are matched by name
  expect_equal(q_residuals(pca, 3, newdata = new[, 8:1])$q, res$q)
  expect_error(q_residuals(pca, 3, newdata = new[, -1]), "lacks 1 variable")
})

test_that("the limits of Q follow Jackson-Mudholkar or Box", {
  rest <- c(2, 1, 0.5)
  theta <- c(sum(rest), sum(rest^2), sum(rest^3))
  h0 <- 1 - 2 * theta[1] * theta[3] / (3 * theta[2]^2)
  jm <- theta[1] * (stats::qnorm(0.95) * sqrt(2 * theta[2] * h0^2) / theta[1] + 1 +
                      theta[2] * h0 * (h0 - 1) / theta[1]^2)^(1 / h0)
  expect_equal(q_limit(rest, 0.95, "jackson"), jm)
  expect_equal(q_limit(rest, 0.95, "box"),
               theta[2] / theta[1] * stats::qchisq(0.95, theta[1]^2 / theta[2]))
  # new normal samples exceed the 95% limits at about the nominal rate
  d <- q_data(n = 300, seed = 4)
  pca <- stats::prcomp(d$x)
  set.seed(5)
  new <- matrix(stats::rnorm(5000 * 3), 5000) %*% d$loadings + matrix(stats::rnorm(5000 * 8, sd = 0.2), 5000)
  res <- q_residuals(pca, 3, newdata = new, conf_level = 0.95)
  expect_lt(abs(mean(res$t2 > res$t2_limit_95) - 0.05), 0.015)
  expect_lt(mean(res$q > res$q_limit_95), 0.07)
})

test_that("q_residuals checks its inputs and plot_influence draws it", {
  d <- q_data()
  pca <- stats::prcomp(d$x)
  expect_error(q_residuals(pca, k = 8), "smaller than")
  expect_error(q_residuals(stats::prcomp(d$x, rank. = 3), k = 2), "keep all")
  res <- q_residuals(pca, k = 2)
  p <- plot_influence(res, label = paste0("s", 1:60))
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "2 components")
  expect_s3_class(plot_influence(res, log = TRUE), "ggplot")
  expect_error(plot_influence(res, label = 1:3), "one value per sample")
  expect_error(plot_influence(pca), "q_residuals")
})

test_that("plot_influence is drawn like plot_outlier_map", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  d <- q_data()
  pca <- stats::prcomp(d$x)
  res <- q_residuals(pca, k = 2, conf_level = c(0.95, 0.99))
  top <- c(t2 = res$t2_limit_99[1], q = res$q_limit_99[1])
  built <- ggplot2::ggplot_build(plot_influence(res, labels = 2))
  # outlined points filled by type, and the limits of each level
  pts <- built$data[[3]]
  expect_equal(unique(pts$shape), 21)
  expect_equal(unique(pts$colour), "black")
  expect_setequal(built$data[[1]]$xintercept, c(res$t2_limit_95[1], top[["t2"]]))
  # at most `labels` labels, of samples beyond a limit
  labelled <- built$data[[4]]$label != ""
  radius <- pmax(res$t2 / top[["t2"]], res$q / top[["q"]])
  expect_lte(sum(labelled), 2)
  expect_true(all(radius[order(radius, decreasing = TRUE)[seq_len(sum(labelled))]] > 1))

  # relative distances put the highest limits at 1
  rel <- ggplot2::ggplot_build(plot_influence(res, relative = TRUE))
  expect_equal(max(rel$data[[1]]$xintercept), 1)
  expect_equal(max(rel$data[[2]]$yintercept), 1)
  expect_equal(rel$data[[3]]$x, res$t2 / top[["t2"]])

  # shading, log axes, colors by distance and a size per sample
  sh <- ggplot2::ggplot_build(plot_influence(res, shade = TRUE, log = TRUE))
  expect_equal(nrow(sh$data[[1]]), 3)
  expect_equal(sh$data[[1]]$xmax[2], log10(top[["t2"]]))
  expect_s3_class(plot_influence(res, colour_by = "distance", relative = TRUE), "ggplot")
  conc <- seq_len(nrow(res))
  sized <- plot_influence(res, size = conc, alpha = 0.5)
  expect_equal(sized$scales$get_scales("size")$name, "conc")
  expect_error(plot_influence(res, size = 1:3), "one value per sample")
  expect_error(plot_influence(res, shade = NA), "shade")
  expect_error(plot_influence(res, 0.5), "one value per sample")
})

test_that("q_residuals takes any confidence levels and a single one", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  d <- q_data()
  pca <- stats::prcomp(d$x)
  one <- q_residuals(pca, 3, conf_level = 0.9)
  expect_true(all(c("t2_limit_90", "q_limit_90") %in% names(one)))
  expect_false("q_limit_95" %in% names(one))
  expect_equal(one$q_limit_90[1], q_limit(pca$sdev[-(1:3)]^2, 0.9, "jackson"))
  # the classification uses the highest level
  both <- q_residuals(pca, 3, conf_level = c(0.9, 0.999))
  expect_equal(as.character(both$outlier) != "regular",
               both$t2 > both$t2_limit_99.9 | both$q > both$q_limit_99.9)
  beta <- q_residuals(pca, 3, t2_method = "beta", conf_level = 0.95)
  expect_equal(beta$t2_limit_95[1], 59^2 / 60 * stats::qbeta(0.95, 1.5, 28))
  expect_s3_class(plot_influence(one), "ggplot")
  p <- plot_influence(both)
  expect_setequal(levels(p$layers[[1]]$data$level), c("90%", "99.9%"))
})

test_that("dmodx follows its formulas", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  d <- q_data()
  pca <- stats::prcomp(d$x)
  a <- 3
  n <- 60
  kvar <- 8
  xs <- scale(d$x, scale = FALSE)
  p <- pca$rotation[, 1:a]
  e <- xs - xs %*% p %*% t(p)
  s0 <- sqrt(sum(e^2) / ((n - a - 1) * (kvar - a)))
  si <- sqrt(rowSums(e^2) / (kvar - a)) * sqrt(n / (n - a - 1))
  nominal <- dmodx(pca, a, df = "nominal", conf_level = c(0.95, 0.99))
  expect_equal(nominal$dmodx, unname(si / s0))
  expect_equal(nominal$dmodx_limit_95[1], sqrt(stats::qf(0.95, kvar - a, (n - a - 1) * (kvar - a))))
  absolute <- dmodx(pca, a, normalized = FALSE, df = "nominal", conf_level = c(0.95, 0.99))
  expect_equal(absolute$dmodx, unname(si))
  expect_equal(absolute$dmodx_limit_99[1], s0 * sqrt(stats::qf(0.99, kvar - a, (n - a - 1) * (kvar - a))))
  # normalized DModX is a scaled square root of Q
  q <- q_residuals(pca, a)
  expect_equal(nominal$dmodx^2, n * q$q / sum(q$q))
  # effective degrees of freedom from the eigenvalues left out
  rest <- pca$sdev[-(1:a)]^2
  nu <- sum(rest)^2 / sum(rest^2)
  eff <- dmodx(pca, a, conf_level = 0.95)
  expect_equal(eff$dmodx, nominal$dmodx)
  expect_equal(eff$dmodx_limit_95[1], sqrt(stats::qf(0.95, nu, (n - a - 1) * nu)))
  expect_equal(eff$t2, q$t2)
  expect_equal(attr(eff, "distance"), "dmodx")
})

test_that("dmodx of new samples uses the calibration s0", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  d <- q_data()
  pca <- stats::prcomp(d$x)
  set.seed(8)
  new <- matrix(stats::rnorm(3 * 3), 3) %*% d$loadings + matrix(stats::rnorm(3 * 8, sd = 0.2), 3)
  colnames(new) <- colnames(d$x)
  new[2, ] <- new[2, ] + c(4, -4, 0, 0, 0, 0, 0, 0)
  res <- dmodx(pca, 3, newdata = new, conf_level = 0.99)
  xs <- scale(d$x, scale = FALSE)
  p <- pca$rotation[, 1:3]
  s0 <- sqrt(sum((xs - xs %*% p %*% t(p))^2) / (56 * 5))
  xn <- scale(new, center = pca$center, scale = FALSE)
  expect_equal(res$dmodx, unname(sqrt(rowSums((xn - xn %*% p %*% t(p))^2) / 5) / s0))
  expect_equal(as.character(res$outlier[2]), "residual")
  expect_true(attr(res, "new"))
  g <- plot_influence(res, label = c("a", "b", "c"))
  expect_equal(g$labels$y, "DModX (normalized)")
  expect_error(dmodx(pca, 3, df = "other"))
})

test_that("the Jackson-Mudholkar limit is valid whatever the sign of h0", {
  h0_of <- function(rest) {
    th <- vapply(1:3, function(j) sum(rest^j), numeric(1))
    1 - 2 * th[1] * th[3] / (3 * th[2]^2)
  }
  # a large eigenvalue and a tail of small ones: h0 goes from positive to
  # negative as the tail grows
  for (m in c(2, 5, 9, 10, 11, 20, 60)) {
    rest <- c(10, rep(1, m))
    limits <- vapply(c(0.9, 0.95, 0.99), function(l) q_limit(rest, l, "jackson"), numeric(1))
    expect_true(all(limits > sum(rest)))   # above the mean of Q
    expect_true(!is.unsorted(limits))      # increasing with the level
  }
  # continuous through h0 = 0
  f <- function(m) q_limit(c(10, rep(1, m)), 0.99, "jackson")
  root <- stats::uniroot(function(m) h0_of(c(10, rep(1, round(m)))), c(5, 10))$root
  near <- vapply(c(floor(root), ceiling(root)), f, numeric(1))
  expect_lt(abs(diff(near)) / mean(near), 0.1)
  # on the forage spectra (h0 close to 0 with 3 components), most spectra
  # are within the limits
  data(forageLIBS, envir = environment())
  pca <- stats::prcomp(forageLIBS[-(1:14)])
  expect_gt(mean(q_residuals(pca, k = 3, conf_level = 0.99)$outlier == "regular"), 0.9)
})
