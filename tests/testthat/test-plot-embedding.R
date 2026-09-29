test_that("plot_embedding picks the embedding axes and colours", {
  set.seed(1)
  df <- data.frame(Sample = letters[1:10], group = rep(c("a", "b"), 5), value = 1:10,
                   UMAP1 = rnorm(10), UMAP2 = rnorm(10))
  p <- plot_embedding(df, colour = group)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "UMAP1")
  expect_equal(p$labels$y, "UMAP2")
  built <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(built$data[[1]]$colour)), 2)
  # a numeric colour gets a continuous scale; a vector works too
  expect_s3_class(plot_embedding(df, colour = "value")$scales$get_scales("colour"), "ScaleContinuous")
  outside <- rep(c("x", "y"), each = 5)
  expect_s3_class(plot_embedding(df, colour = outside), "ggplot")
  expect_error(plot_embedding(df, colour = 1:3), "one value per sample")
  # explicit axes; without coordinate names, the first numeric columns
  expect_equal(plot_embedding(df, x = UMAP2, y = "value")$labels$x, "UMAP2")
  expect_equal(plot_embedding(df[c("value", "group", "UMAP2")])$labels$y, "UMAP2")
  expect_error(plot_embedding(df, x = group), "numeric column")
  expect_error(plot_embedding(df["value"]), "two numeric columns")
})

test_that("plot_embedding takes prcomp fits, robust PCA and matrices", {
  set.seed(1)
  x <- matrix(rnorm(200), 40, 5)
  expect_equal(plot_embedding(stats::prcomp(x))$labels$x, "PC1")
  fake_robpca <- structure(list(scores = x[, 1:3]), class = "specproc_robpca")
  expect_equal(plot_embedding(fake_robpca, y = PC3)$labels$y, "PC3")
  expect_equal(plot_embedding(x)$labels$x, "Dim1")
  expect_error(plot_embedding("a"), "data frame")
})

test_that("plot_embedding draws confidence ellipses", {
  skip_if_not_installed("ConfidenceEllipse")
  set.seed(3)
  df <- data.frame(PC1 = c(rnorm(20), rnorm(20, 5), rnorm(3, 10)),
                   PC2 = c(rnorm(20), rnorm(20, 5), rnorm(3, 10)),
                   group = factor(rep(c("a", "b", "c"), c(20, 20, 3))))
  expect_warning(p <- plot_embedding(df, colour = group, ellipse = TRUE), "fewer than 4 samples: c")
  layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% layers)
  poly <- p$layers[[which(layers == "GeomPolygon")]]$data
  expect_setequal(unique(as.character(poly$.colour)), c("a", "b"))
  # the ellipse of group a matches ConfidenceEllipse
  ref <- ConfidenceEllipse::confidence_ellipse(data.frame(.x = df$PC1[1:20], .y = df$PC2[1:20]),
                                               ".x", ".y", distribution = "hotelling")
  p2 <- suppressWarnings(plot_embedding(df, colour = group, ellipse = TRUE, conf_level = 0.95,
                                        distribution = "hotelling"))
  poly2 <- p2$layers[[which(layers == "GeomPolygon")]]$data
  expect_equal(poly2$x[poly2$.colour == "a"], ref$x)
  # one ellipse without discrete groups
  one <- plot_embedding(df, ellipse = TRUE, robust = TRUE)
  expect_true(any(vapply(one$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))))
  expect_error(plot_embedding(df, ellipse = TRUE, distribution = "t"))
  expect_error(plot_embedding(df, ellipse = TRUE, conf_level = 1), "conf_level")
  # several confidence levels: one ellipse per group and level
  two <- suppressWarnings(plot_embedding(df, colour = group, ellipse = TRUE, conf_level = c(0.9, 0.95)))
  poly_two <- two$layers[vapply(two$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))][[1]]$data
  expect_setequal(unique(poly_two$.level), c(0.9, 0.95))
})

test_that("plot_embedding draws T-squared ellipses at the chosen levels", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  set.seed(9)
  x <- data.frame(PC1 = rnorm(40), PC2 = 0.7 * rnorm(40), id = paste0("s", 1:40))
  x$PC2 <- x$PC2 + 0.8 * x$PC1                                # correlated components
  x[5, 1:2] <- c(4, -4)
  p <- plot_embedding(x, hotelling = "all", conf_level = 0.9, label = id)
  path <- p$layers[vapply(p$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))][[1]]$data
  expect_equal(levels(path$limit), "T² 90%")
  s <- as.matrix(x[1:2])
  expect_equal(range(stats::mahalanobis(as.matrix(path[c("x", "y")]), colMeans(s), stats::cov(s))),
               rep(2 * 39 / 38 * stats::qf(0.9, 2, 38), 2), tolerance = 1e-6)
  expect_match(p$labels$subtitle, "90% limit")
  b <- plot_embedding(x, hotelling = "all", t2_method = "beta", conf_level = c(0.95, 0.999))
  bp <- b$layers[vapply(b$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))][[1]]$data
  on <- as.matrix(bp[bp$limit == "T² 99.9%", c("x", "y")])
  expect_equal(range(stats::mahalanobis(on, colMeans(s), stats::cov(s))),
               rep(39^2 / 40 * stats::qbeta(0.999, 1, 18.5), 2), tolerance = 1e-6)
  expect_error(plot_embedding(x, hotelling = "all", conf_level = 2), "between 0 and 1")
})

test_that("plot_embedding uses one conf_level for both kinds of ellipses", {
  skip_if_not_installed("ConfidenceEllipse")
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  set.seed(10)
  x <- data.frame(PC1 = rnorm(30), PC2 = rnorm(30))
  both <- plot_embedding(x, ellipse = TRUE, hotelling = "all")
  poly <- both$layers[vapply(both$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))][[1]]$data
  path <- both$layers[vapply(both$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))][[1]]$data
  expect_equal(unique(poly$.level), 0.975)                           # the default of both kinds
  expect_equal(as.character(unique(path$limit)), "T\u00b2 97.5%")
  same <- plot_embedding(x, ellipse = TRUE, hotelling = "all", conf_level = 0.9)
  poly9 <- same$layers[vapply(same$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))][[1]]$data
  path9 <- same$layers[vapply(same$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))][[1]]$data
  expect_equal(unique(poly9$.level), 0.9)
  expect_equal(as.character(unique(path9$limit)), "T\u00b2 90%")
  expect_false("t2_level" %in% names(formals(plot_embedding)))
})
