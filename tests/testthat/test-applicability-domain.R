domain_data <- function(n = 100, seed = 3) {
  set.seed(seed)
  scores <- matrix(stats::rnorm(n * 2), n) %*% diag(c(5, 2))
  loadings <- matrix(stats::rnorm(2 * 30), 2)
  x <- scores %*% loadings + matrix(stats::rnorm(n * 30, sd = 0.1), n)
  colnames(x) <- paste0("w", 1:30)
  x
}

test_that("applicability_domain flags extreme and foreign spectra", {
  all <- domain_data(105)
  x <- all[1:100, ]
  domain <- applicability_domain(x, k = 2)
  expect_s3_class(domain, "specproc_domain")
  expect_equal(domain$k, 2)
  pca <- domain$pca
  # a spectrum far along the first component, and one off the model plane
  extreme <- pca$center + 6 * pca$sdev[1] * pca$rotation[, 1]
  foreign <- pca$center + 20 * pca$sdev[3] * pca$rotation[, 3]
  new <- rbind(all[101:105, ], extreme, foreign)
  out <- predict(domain, new)
  expect_s3_class(out, "specproc_influence")
  expect_equal(out$inside, c(rep(TRUE, 5), FALSE, FALSE))
  expect_equal(as.character(out$outlier[6:7]), c("extreme", "residual"))
  # the distances and limits of q_residuals()
  ref <- q_residuals(pca, 2, newdata = new, conf_level = 0.99)
  expect_equal(out$t2, ref$t2)
  expect_equal(out$q, ref$q)
  expect_equal(unique(out$t2_limit_99), unname(domain$limits[["t2"]]))
  expect_equal(unique(out$q_limit_99), unname(domain$limits[["q"]]))
  expect_s3_class(plot_influence(out), "ggplot")
  expect_output(print(domain), "2 \\(")
})

test_that("applicability_domain chooses the components by explained variance", {
  x <- domain_data()
  explained <- cumsum(stats::prcomp(x)$sdev^2) / sum(stats::prcomp(x)$sdev^2)
  domain <- applicability_domain(x, variance = 0.95)
  expect_equal(domain$k, which(explained >= 0.95)[1])
  expect_equal(domain$explained, explained[domain$k])
  expect_error(applicability_domain(x, k = 30), "smaller than")
  expect_error(applicability_domain(x[, 1, drop = FALSE]), "2 principal components")
  x[1, 1] <- NA
  expect_error(applicability_domain(x), "missing")
})
