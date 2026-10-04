test_that("plot_coomans() draws the distances to two classes", {
  d <- make_simca(n = 25, p = 30)
  set.seed(1)
  fit <- rsimca(d$train$x, d$train$g, ncomp = 2)
  p <- plot_coomans(fit, labels = 0)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "Coomans plot")
  expect_equal(p$labels$x, "Distance to a")
  expect_equal(p$labels$y, "Distance to b")
  expect_equal(p$data$x, fit$distances$a)
  expect_equal(p$data$y, fit$distances$b)
  expect_identical(as.character(p$data$class), as.character(d$train$g))
  # the same range on both axes, so that the diagonal is at 45 degrees
  panel <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  expect_equal(panel$x.range, panel$y.range)
})

test_that("plot_coomans() adds new observations, log axes and shaded regions", {
  d <- make_simca(n = 25, p = 30, seed = 2)
  set.seed(3)
  fit <- rsimca(d$train$x, d$train$g, ncomp = 2)
  p <- plot_coomans(fit, newdata = d$test$x, group = d$test$g, log = TRUE, shade = TRUE)
  expect_equal(nrow(p$data), nrow(d$train$x) + nrow(d$test$x))
  new <- p$data$set == "new"
  expect_equal(p$data$x[new], log10(predict(fit, d$test$x, type = "distances")$a))
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1))))
  # new observations without classes are unknown
  p <- plot_coomans(fit, newdata = d$test$x)
  expect_true(all(p$data$class[p$data$set == "new"] == "unknown"))
  # the axes can show other classes, in either order
  expect_equal(plot_coomans(fit, classes = c("b", "a"))$labels$x, "Distance to b")
  expect_error(plot_coomans(fit, classes = c("a", "z")), "two different classes")
  expect_error(plot_coomans(fit, classes = c("a", "a")), "two different classes")
  expect_error(plot_coomans(fit, group = d$test$g), "newdata")
  expect_error(plot_coomans(fit, newdata = d$test$x, group = d$test$g[-1]), "one class per row")
  expect_error(plot_coomans(list()), "rsimca")
})
