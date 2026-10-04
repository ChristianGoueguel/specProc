test_that("rsimca classifies high-dimensional data and resists outliers", {
  d <- make_simca()
  x <- d$train$x
  set.seed(2)
  x[1:5, ] <- x[1:5, ] + matrix(rnorm(5 * ncol(x), sd = 3), 5)
  set.seed(3)
  fit <- rsimca(x, d$train$g, ncomp = 2)
  expect_s3_class(fit, "specproc_rsimca")
  expect_identical(fit$group, d$train$g)
  expect_equal(fit$ncomp, c(a = 2L, b = 2L))
  expect_true(all(fit$weights[1:5] == 0))
  expect_true(all(fit$outlying[1:5]))
  expect_gt(mean(predict(fit, d$test$x) == d$test$g), 0.95)
  dist <- predict(fit, d$test$x, type = "distances")
  expect_named(dist, c("a", "b", "outlying"))
  expect_identical(as.character(predict(fit, d$test$x)),
                   c("a", "b")[max.col(-as.matrix(dist[1:2]), ties.method = "first")])
  # a spectrum far from both classes is outlying for both
  far <- d$test$x[1, , drop = FALSE] + 20
  expect_true(predict(fit, far, type = "distances")$outlying)
  expect_equal(fit$misclassification$class, c("a", "b", "overall"))
  expect_output(print(fit), "RSIMCA")
})

test_that("rsimca takes the number of components and the rule of each class", {
  d <- make_simca(seed = 4)
  set.seed(5)
  fit <- rsimca(d$train$x, d$train$g, ncomp = c(b = 3, a = 2), squared = FALSE, gamma = 0.8)
  expect_equal(fit$ncomp, c(a = 2L, b = 3L))
  expect_false(fit$squared)
  expect_gt(mean(fit$fitted == d$train$g), 0.95)
  set.seed(5)
  expect_true(all(rsimca(d$train$x, d$train$g)$ncomp >= 1))
  expect_error(rsimca(d$train$x, d$train$g, ncomp = 1:3), "'ncomp'")
  expect_error(rsimca(d$train$x, d$train$g, gamma = 2), "'gamma'")
  expect_error(rsimca(d$train$x[c(1:4, 41:80), ], d$train$g[c(1:4, 41:80)]), "at least 5")
})
