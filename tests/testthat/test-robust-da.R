# Two Gaussian classes in two dimensions.
make_classes <- function(n = 60, seed = 1) {
  set.seed(seed)
  x <- rbind(matrix(rnorm(n * 2), n),
             matrix(rnorm(n * 2), n) + matrix(c(3, 1), n, 2, byrow = TRUE))
  colnames(x) <- c("u", "v")
  list(x = x, g = factor(rep(c("a", "b"), each = n)))
}

test_that("robust_da resists outliers of the training data", {
  d <- make_classes()
  test <- make_classes(seed = 2)
  x <- d$x
  x[1:10, ] <- matrix(c(15, 15), 10, 2, byrow = TRUE) + rnorm(20, sd = 0.1)
  set.seed(3)
  fit <- robust_da(x, d$g)
  expect_s3_class(fit, "specproc_robust_da")
  expect_true(all(fit$weights[1:10] == 0))
  expect_equal(unname(fit$center["a", ]), c(0, 0), tolerance = 0.3)
  robust_error <- mean(predict(fit, test$x) != test$g)
  classical_error <- mean(predict(MASS::lda(x, d$g), test$x)$class != test$g)
  expect_lt(robust_error, 0.15)
  expect_lt(robust_error, classical_error)
  prob <- predict(fit, test$x, type = "prob")
  expect_named(prob, c("a", "b"))
  expect_equal(rowSums(prob), rep(1, nrow(test$x)))
  expect_identical(as.character(predict(fit, test$x)),
                   c("a", "b")[max.col(as.matrix(prob), ties.method = "first")])
  expect_equal(fit$misclassification$class, c("a", "b", "overall"))
  expect_output(print(fit), "linear discriminant")
})

test_that("robust_da has a quadratic rule and membership probabilities", {
  d <- make_classes(seed = 4)
  set.seed(5)
  fit <- robust_da(d$x, d$g, method = "quadratic", prior = c(b = 0.3, a = 0.7))
  expect_length(fit$cov, 2)
  expect_equal(fit$prior, c(a = 0.7, b = 0.3))
  expect_lt(mean(fit$fitted != d$g), 0.15)
  expect_output(print(fit), "quadratic")
  # default prior: proportions of the regular observations
  set.seed(5)
  fit2 <- robust_da(d$x, d$g)
  expect_equal(sum(fit2$prior), 1)
  expect_error(robust_da(d$x[1:62, ], d$g[1:62]), "more observations than variables")
  expect_error(robust_da(d$x, rep("a", 120)), "two classes")
  expect_error(robust_da(d$x, d$g, prior = c(1, 0)), "'prior'")
})

test_that("the mcd engines of parsnip::discrim_linear() and discrim_quad() fit robust_da()", {
  skip_if_not_installed("parsnip")
  d <- make_classes(seed = 6)
  dat <- data.frame(g = d$g, d$x)
  for (spec in list(parsnip::discrim_linear(), parsnip::discrim_quad())) {
    set.seed(7)
    fit <- parsnip::fit(spec |> parsnip::set_engine("mcd"), g ~ ., data = dat)
    engine_fit <- parsnip::extract_fit_engine(fit)
    expect_s3_class(engine_fit, "specproc_robust_da")
    set.seed(7)
    ref <- robust_da(d$x, d$g, method = engine_fit$method)
    expect_equal(predict(fit, dat)$.pred_class, predict(ref, d$x))
    expect_equal(predict(fit, dat, type = "prob")$.pred_b,
                 predict(ref, d$x, type = "prob")$b)
  }
  expect_identical(engine_fit$method, "quadratic")
})
