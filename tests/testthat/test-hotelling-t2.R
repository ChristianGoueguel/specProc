test_that("hotelling_t2 matches the Mahalanobis distance and the F limits", {
  set.seed(4)
  x <- data.frame(PC1 = rnorm(40), PC2 = rnorm(40), PC3 = rnorm(40), other = 1:40)
  x[1, 1:3] <- c(6, 6, 6)
  t2 <- hotelling_t2(x, k = 3)
  s <- as.matrix(x[1:3])
  expect_equal(t2$t2, unname(stats::mahalanobis(s, colMeans(s), stats::cov(s))))
  expect_equal(t2$limit_95[1], 3 * 39 / 37 * stats::qf(0.95, 3, 37))
  expect_equal(t2$limit_99[1], 3 * 39 / 37 * stats::qf(0.99, 3, 37))
  expect_true(t2$outlier_99[1])
  expect_equal(t2$outlier_95, t2$t2 > t2$limit_95)
  expect_equal(unique(t2$n), 40L)
  # explicit columns, and the columns of the default k
  expect_equal(hotelling_t2(x, columns = c("PC1", "PC2"))$t2, hotelling_t2(x)$t2)
  expect_error(hotelling_t2(x, columns = "PC1"), "at least 2")
  expect_error(hotelling_t2(x, k = 1))
})

test_that("hotelling_t2 works within groups", {
  set.seed(5)
  x <- data.frame(PC1 = c(rnorm(20), rnorm(20, 10), 1, 2),
                  PC2 = c(rnorm(20), rnorm(20, 10), 1, 2),
                  g = rep(c("a", "b", "c"), c(20, 20, 2)))
  expect_warning(t2 <- hotelling_t2(x, group = g), "group c")
  a <- as.matrix(x[x$g == "a", 1:2])
  expect_equal(t2$t2[t2$group == "a"], unname(stats::mahalanobis(a, colMeans(a), stats::cov(a))))
  expect_true(all(is.na(t2$t2[t2$group == "c"])))
  expect_false(any(t2$outlier_99[t2$group == "c"]))
  expect_equal(t2$sample, seq_len(nrow(x)))
  expect_equal(hotelling_t2(x, group = rep(1:2, 21))$group, rep(1:2, 21))
  # a prcomp fit
  expect_equal(nrow(hotelling_t2(stats::prcomp(x[1:40, 1:2]))), 40)
})

test_that("plot_embedding draws T-squared ellipses and labels outliers", {
  skip_if_not_installed("ConfidenceEllipse")
  set.seed(6)
  x <- data.frame(PC1 = rnorm(40), PC2 = rnorm(40), group = rep(c("a", "b"), 20),
                  id = paste0("s", 1:40))
  x[3, 1:2] <- c(8, -8)
  p <- plot_embedding(x, hotelling = "all", label = id)
  paths <- p$layers[vapply(p$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))][[1]]$data
  # the 95% contour lies at the T-squared limit
  s <- as.matrix(x[1:2])
  on95 <- as.matrix(paths[paths$limit == "T² 95%", c("x", "y")])
  expect_equal(range(stats::mahalanobis(on95, colMeans(s), stats::cov(s))),
               rep(2 * 39 / 38 * stats::qf(0.95, 2, 38), 2))
  labels <- p$layers[vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))][[1]]$data
  expect_equal(labels$.label, "s3")
  expect_match(p$labels$subtitle, "1 sample")
  g <- plot_embedding(x, colour = group, hotelling = "group")
  gp <- g$layers[vapply(g$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))][[1]]$data
  expect_setequal(unique(as.character(gp$.colour)), c("a", "b"))
  expect_error(plot_embedding(x, hotelling = "group"), "discrete")
  expect_error(plot_embedding(x, hotelling = "all", k = 3), "fewer than 3")
})

test_that("hotelling_t2 takes any confidence levels and the Beta limits", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  set.seed(7)
  x <- data.frame(PC1 = rnorm(30), PC2 = rnorm(30), PC3 = rnorm(30))
  one <- hotelling_t2(x, k = 3, conf_level = 0.975)
  expect_named(one, c("sample", "t2", "limit_97.5", "outlier_97.5", "n"))
  expect_equal(one$limit_97.5[1], 3 * 29 / 27 * stats::qf(0.975, 3, 27))
  three <- hotelling_t2(x, k = 3, conf_level = c(0.99, 0.9, 0.95))
  expect_true(all(c("limit_90", "limit_95", "limit_99") %in% names(three)))
  beta <- hotelling_t2(x, k = 3, method = "beta")
  expect_equal(beta$limit_95[1], 29^2 / 30 * stats::qbeta(0.95, 1.5, 13))
  expect_equal(beta$t2, one$t2)
  # the same T-squared as HotellingEllipse
  ref <- HotellingEllipse::ellipseParam(x, k = 3, conf.limit = 0.95)
  expect_equal(one$t2, ref$Tsquare$value)
  expect_error(hotelling_t2(x, conf_level = 1), "between 0 and 1")
  expect_error(hotelling_t2(x, method = "t"))
})
