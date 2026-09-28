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
