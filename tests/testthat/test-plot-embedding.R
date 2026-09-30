# the built data of the first point layer (the samples)
point_data <- function(p) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]
  ggplot2::ggplot_build(p)$data[[i]]
}

# the confidence ellipses (the polygons with a level), not the white insides
ellipse_data <- function(p) {
  Filter(function(l) inherits(l$geom, "GeomPolygon") && ".level" %in% names(l$data), p$layers)[[1]]$data
}

test_that("plot_embedding picks the embedding axes and colours", {
  set.seed(1)
  df <- data.frame(Sample = letters[1:10], group = rep(c("a", "b"), 5), value = 1:10,
                   UMAP1 = rnorm(10), UMAP2 = rnorm(10))
  p <- plot_embedding(df, colour = group)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "UMAP1")
  expect_equal(p$labels$y, "UMAP2")
  built <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(point_data(p)$colour)), 2)
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
  expect_match(plot_embedding(stats::prcomp(x))$labels$x, "^PC1 \\([0-9.]+%\\)$")
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
  poly <- ellipse_data(p)
  expect_setequal(unique(as.character(poly$.colour)), c("a", "b"))
  # the ellipse of group a matches ConfidenceEllipse
  ref <- ConfidenceEllipse::confidence_ellipse(data.frame(.x = df$PC1[1:20], .y = df$PC2[1:20]),
                                               ".x", ".y", distribution = "hotelling")
  p2 <- suppressWarnings(plot_embedding(df, colour = group, ellipse = TRUE, conf_level = 0.95,
                                        distribution = "hotelling"))
  poly2 <- ellipse_data(p2)
  expect_equal(poly2$x[poly2$.colour == "a"], ref$x)
  # one ellipse without discrete groups
  one <- plot_embedding(df, ellipse = TRUE, robust = TRUE)
  expect_true(any(vapply(one$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))))
  expect_error(plot_embedding(df, ellipse = TRUE, distribution = "t"))
  expect_error(plot_embedding(df, ellipse = TRUE, conf_level = 1), "conf_level")
  # several confidence levels: one ellipse per group and level
  two <- suppressWarnings(plot_embedding(df, colour = group, ellipse = TRUE, conf_level = c(0.9, 0.95)))
  poly_two <- ellipse_data(two)
  expect_setequal(unique(poly_two$.level), c(0.9, 0.95))
})

test_that("plot_embedding draws T-squared ellipses at the chosen levels", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  set.seed(9)
  x <- data.frame(PC1 = rnorm(40), PC2 = 0.7 * rnorm(40), id = paste0("s", 1:40))
  x$PC2 <- x$PC2 + 0.8 * x$PC1                                # correlated components
  x[5, 1:2] <- c(4, -4)
  p <- plot_embedding(x, hotelling = "all", conf_level = 0.9, flag = TRUE, label = id)
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
  x <- data.frame(PC1 = rnorm(30), PC2 = rnorm(30), g = rep(c("a", "b"), 15))
  both <- plot_embedding(x, colour = g, ellipse = TRUE, hotelling = "all")
  poly <- ellipse_data(both)
  path <- both$layers[vapply(both$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))][[1]]$data
  expect_equal(unique(poly$.level), 0.975)                           # the default of both kinds
  expect_equal(as.character(unique(path$limit)), "T\u00b2 97.5%")
  same <- plot_embedding(x, colour = g, ellipse = TRUE, hotelling = "all", conf_level = 0.9)
  poly9 <- ellipse_data(same)
  path9 <- same$layers[vapply(same$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))][[1]]$data
  expect_equal(unique(poly9$.level), 0.9)
  expect_equal(as.character(unique(path9$limit)), "T\u00b2 90%")
  expect_false("t2_level" %in% names(formals(plot_embedding)))
  # without groups, only the T-squared ellipse, with a warning
  expect_warning(alone <- plot_embedding(x, ellipse = TRUE, hotelling = "all"), "left out")
  expect_length(Filter(function(l) inherits(l$geom, "GeomPolygon") && ".level" %in% names(l$data),
                       alone$layers), 0)
  expect_length(Filter(function(l) inherits(l$geom, "GeomPath"), alone$layers), 1)
  expect_silent(plot_embedding(x, ellipse = TRUE))
})

test_that("plot_embedding fills the insides of the ellipses in white on a grey panel", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  set.seed(11)
  x <- data.frame(PC1 = rnorm(30), PC2 = rnorm(30), g = rep(c("a", "b"), 15))
  p <- plot_embedding(x, hotelling = "all", conf_level = c(0.95, 0.99))
  white <- Filter(function(l) inherits(l$geom, "GeomPolygon") && ".shape" %in% names(l$data),
                  p$layers)[[1]]
  expect_equal(white$aes_params$fill, "white")
  # the outermost (99%) ellipse only
  path <- Filter(function(l) inherits(l$geom, "GeomPath"), p$layers)[[1]]$data
  expect_equal(white$data$x, path$x[path$limit == "T\u00b2 99%"])
  expect_equal(p$theme$panel.background$fill, "grey90")
  expect_equal(p$theme$aspect.ratio, 0.7)
  # one white inside per group
  g <- plot_embedding(x, colour = g, hotelling = "group")
  white_g <- Filter(function(l) ".shape" %in% names(l$data), g$layers)[[1]]$data
  expect_equal(length(unique(white_g$.shape)), 2)
  # no ellipse: a white panel
  plain <- plot_embedding(x, aspect_ratio = NULL)
  expect_equal(plain$theme$panel.background$fill, "white")
  expect_null(plain$theme$aspect.ratio)
  expect_error(plot_embedding(x, aspect_ratio = 0), "aspect_ratio")
  # lines through the origin, only when it is in the range of the samples
  geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(all(c("GeomHline", "GeomVline") %in% geoms(p)))
  shifted <- plot_embedding(transform(x, PC1 = PC1 + 10))
  expect_false("GeomVline" %in% geoms(shifted))
  expect_true("GeomHline" %in% geoms(shifted))
})

test_that("plot_embedding maps the point size to a variable", {
  set.seed(12)
  x <- data.frame(PC1 = rnorm(20), PC2 = rnorm(20), conc = runif(20, 1, 10))
  p <- plot_embedding(x, size = conc)
  expect_equal(p$scales$get_scales("size")$name, "conc")
  expect_s3_class(p$scales$get_scales("size"), "ScaleContinuous")
  expect_equal(point_data(p)$size[which.max(x$conc)], 6)
  v <- plot_embedding(x, size = x$conc * 2)
  expect_false(is.null(v$scales$get_scales("size")))
  expect_null(plot_embedding(x, size = 3)$scales$get_scales("size"))
  expect_error(plot_embedding(x, size = letters[1:20]), "numeric")
  expect_error(plot_embedding(x, size = 1:3), "one value per sample")
  expect_error(plot_embedding(x, size = -1), "size")
})

test_that("plot_embedding draws a biplot of a PCA", {
  pca <- stats::prcomp(iris[1:4])
  p <- plot_embedding(pca, colour = iris$Species, biplot = TRUE, biplot_top = 2)
  arrows <- Filter(function(l) inherits(l$geom, "GeomSegment") && !is.null(l$geom_params$arrow),
                   p$layers)[[1]]$data
  expect_equal(nrow(arrows), 2)
  # the longest loadings in the plane, scaled to the scores of each axis
  norm <- sqrt(pca$rotation[, 1]^2 + pca$rotation[, 2]^2)
  expect_setequal(arrows$label, names(sort(norm, decreasing = TRUE))[1:2])
  fx <- 0.8 * max(abs(pca$x[, 1])) / max(abs(pca$rotation[, 1]))
  expect_equal(arrows$x, unname(pca$rotation[arrows$label, 1]) * fx)
  expect_error(plot_embedding(as.data.frame(pca$x), biplot = TRUE), "prcomp")
  expect_error(plot_embedding(pca, x = "PC1", y = "PC3", biplot = TRUE, biplot_top = -1), "biplot_top")
  # spectra: one arrow per line (local peaks of the loading length)
  set.seed(13)
  wl <- seq(400, 420, by = 0.1)
  line <- function(center) exp(-(wl - center)^2 / 0.05)
  s <- outer(rnorm(30), line(405)) + outer(rnorm(30), line(415)) + matrix(rnorm(30 * 201, sd = 0.01), 30)
  colnames(s) <- wl
  sp <- plot_embedding(stats::prcomp(s), biplot = TRUE, biplot_top = 2)
  sa <- Filter(function(l) inherits(l$geom, "GeomSegment") && !is.null(l$geom_params$arrow),
               sp$layers)[[1]]$data
  expect_setequal(round(as.numeric(sa$label)), c(405, 415))
})

test_that("plot_embedding gives the explained variance in the axis titles", {
  pca <- stats::prcomp(iris[1:4])
  share <- 100 * pca$sdev^2 / sum(pca$sdev^2)
  p <- plot_embedding(pca)
  expect_equal(p$labels$x, sprintf("PC1 (%.1f%%)", share[1]))
  expect_equal(plot_embedding(pca, x = "PC3", y = "PC2")$labels$x, sprintf("PC3 (%.1f%%)", share[3]))
  # robust fits: share of the variance of their components
  set.seed(14)
  fit <- robpca(as.matrix(iris[1:4]), k = 2)
  rp <- plot_embedding(fit)
  expect_equal(rp$labels$y, sprintf("PC2 (%.1f%%)", 100 * fit$eigenvalues[2] / sum(fit$eigenvalues)))
  # other embeddings keep the column names
  expect_equal(plot_embedding(as.data.frame(pca$x))$labels$x, "PC1")
})

test_that("plot_embedding fills the T-squared ellipses of the groups", {
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  set.seed(15)
  x <- data.frame(PC1 = rnorm(30), PC2 = rnorm(30), g = rep(c("a", "b"), 15))
  p <- plot_embedding(x, colour = g, hotelling = "group")
  filled <- Filter(function(l) inherits(l$geom, "GeomPolygon") && "limit" %in% names(l$data), p$layers)
  expect_length(filled, 1)
  expect_equal(rlang::as_label(filled[[1]]$mapping$fill), ".colour")
  # one group-free T-squared ellipse is not filled
  expect_length(Filter(function(l) inherits(l$geom, "GeomPolygon") && "limit" %in% names(l$data),
                       plot_embedding(x, hotelling = "all")$layers), 0)
})

test_that("plot_embedding draws the T-squared ellipses of a PCA model", {
  set.seed(16)
  x <- matrix(rnorm(60 * 6), 60, 6) %*% diag(6:1)
  x[1:4, ] <- x[1:4, ] + 12
  # prcomp: the model ellipse, centered at 0 with the axes of the components
  pca <- stats::prcomp(x)
  p <- plot_embedding(pca, hotelling = "all", conf_level = 0.95)
  path <- Filter(function(l) inherits(l$geom, "GeomPath"), p$layers)[[1]]$data
  lambda <- pca$sdev[1:2]^2
  limit <- t2_limit(0.95, 2, 60, "f")
  expect_equal(path$x^2 / lambda[1] + path$y^2 / lambda[2], rep(limit, nrow(path)))
  expect_equal(range(path$x), c(-1, 1) * sqrt(lambda[1] * limit))
  # the flags: T-squared of the model on k components
  t2 <- rowSums(sweep(pca$x[, 1:3]^2, 2, pca$sdev[1:3]^2, "/"))
  p3 <- plot_embedding(pca, hotelling = "all", k = 3)
  expect_match(p3$labels$subtitle, paste0(sum(t2 > t2_limit(0.975, 3, 60, "f")), " sample"))

  # robust fit: its eigenvalues and chi-square limit, the leverage points of the outlier map
  set.seed(17)
  fit <- robpca(x, k = 3)
  r <- plot_embedding(fit, hotelling = "all", k = 3, flag = TRUE, label = TRUE)
  rp <- Filter(function(l) inherits(l$geom, "GeomPath"), r$layers)[[1]]$data
  expect_equal(rp$x^2 / fit$eigenvalues[1] + rp$y^2 / fit$eigenvalues[2],
               rep(stats::qchisq(0.975, 2), nrow(rp)))
  flagged <- Filter(function(l) inherits(l$geom, "GeomText") && ".label" %in% names(l$data),
                    r$layers)[[1]]$data$.label
  leverage <- which(fit$outlier_type %in% c("good leverage", "bad leverage"))
  expect_setequal(as.integer(flagged), leverage)
  # other embeddings: from the mean and covariance of the samples
  skip_if_not_installed("HotellingEllipse", minimum_version = "1.3.0")
  shifted <- as.data.frame(pca$x[, 1:3] + 100)
  sp <- plot_embedding(shifted, hotelling = "all")
  spath <- Filter(function(l) inherits(l$geom, "GeomPath"), sp$layers)[[1]]$data
  expect_equal(mean(range(spath$x)), 100, tolerance = 1e-3)
})

test_that("the circles and labels of the flagged samples are optional", {
  set.seed(18)
  x <- data.frame(PC1 = rnorm(30), PC2 = rnorm(30), id = paste0("s", 1:30))
  x[1, 1:2] <- c(8, 8)
  pca <- stats::prcomp(x[1:2])
  texts <- function(p) Filter(function(l) inherits(l$geom, "GeomText") && ".label" %in% names(l$data),
                              p$layers)
  circles <- function(p) Filter(function(l) inherits(l$geom, "GeomPoint") && identical(l$aes_params$shape, 21),
                                p$layers)
  # by default, neither circles nor labels, but the count in the subtitle
  p <- plot_embedding(pca, hotelling = "all")
  expect_length(texts(p), 0)
  expect_length(circles(p), 0)
  expect_match(p$labels$subtitle, "1 sample")
  # labels without circles, circles without labels, or both
  lab <- plot_embedding(pca, hotelling = "all", label = TRUE)
  expect_equal(texts(lab)[[1]]$data$.label, 1)
  expect_length(circles(lab), 0)
  expect_equal(texts(plot_embedding(pca, hotelling = "all", label = x$id))[[1]]$data$.label, "s1")
  f <- plot_embedding(pca, hotelling = "all", flag = TRUE)
  expect_length(circles(f), 1)
  expect_length(texts(f), 0)
  expect_length(texts(plot_embedding(pca, hotelling = "all", flag = TRUE, label = FALSE)), 0)
  expect_equal(texts(plot_embedding(pca, hotelling = "all", flag = TRUE, label = TRUE))[[1]]$data$.label, 1)
  expect_equal(texts(plot_embedding(pca, hotelling = "all", flag = TRUE, label = x$id))[[1]]$data$.label, "s1")
  expect_error(plot_embedding(pca, hotelling = "all", flag = NA), "flag")
})
