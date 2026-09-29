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

test_that("dmodx follows the SIMCA formulas", {
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
  simca <- dmodx(pca, a, df = "simca", conf_level = c(0.95, 0.99))
  expect_equal(simca$dmodx, unname(si / s0))
  expect_equal(simca$dmodx_limit_95[1], sqrt(stats::qf(0.95, kvar - a, (n - a - 1) * (kvar - a))))
  absolute <- dmodx(pca, a, normalized = FALSE, df = "simca", conf_level = c(0.95, 0.99))
  expect_equal(absolute$dmodx, unname(si))
  expect_equal(absolute$dmodx_limit_99[1], s0 * sqrt(stats::qf(0.99, kvar - a, (n - a - 1) * (kvar - a))))
  # normalized DModX is a scaled square root of Q
  q <- q_residuals(pca, a)
  expect_equal(simca$dmodx^2, n * q$q / sum(q$q))
  # effective degrees of freedom from the eigenvalues left out
  rest <- pca$sdev[-(1:a)]^2
  nu <- sum(rest)^2 / sum(rest^2)
  eff <- dmodx(pca, a, conf_level = 0.95)
  expect_equal(eff$dmodx, simca$dmodx)
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
