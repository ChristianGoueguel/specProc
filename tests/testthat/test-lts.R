set.seed(1)
n <- 80
lts_x <- matrix(rnorm(n * 2), n, dimnames = list(NULL, c("a", "b")))
lts_y <- drop(1 + lts_x %*% c(2, -1)) + rnorm(n, sd = 0.2)
lts_y[1:8] <- lts_y[1:8] + 10

test_that("lts_fit is the reweighted LTS regression of robustbase", {
  set.seed(2)
  fit <- lts_fit(lts_x, lts_y, alpha = 0.75)
  set.seed(2)
  ref <- robustbase::ltsReg(lts_x, lts_y, alpha = 0.75)
  expect_equal(unname(fit$coefficients), unname(ref$coefficients))
  expect_named(fit$coefficients, c("(Intercept)", "a", "b"))
  expect_equal(fit$scale, ref$scale)
  expect_true(all(fit$weights[1:8] == 0))
  expect_equal(unname(fit$coefficients), c(1, 2, -1), tolerance = 0.1)
  expect_equal(predict(fit, lts_x[1:3, ]), fit$fitted[1:3])
  expect_output(print(fit), "LTS")
  expect_error(lts_fit(lts_x, cbind(lts_y, lts_y)), "single response")
})

test_that("the lts engine of parsnip::linear_reg() fits lts_fit()", {
  skip_if_not_installed("parsnip")
  dat <- data.frame(y = lts_y, lts_x)
  set.seed(3)
  fit <- parsnip::fit(parsnip::linear_reg() |> parsnip::set_engine("lts", alpha = 0.9),
                      y ~ ., data = dat)
  set.seed(3)
  ref <- lts_fit(lts_x, lts_y, alpha = 0.9)
  expect_s3_class(parsnip::extract_fit_engine(fit), "specproc_lts")
  expect_equal(predict(fit, dat[1:5, ])$.pred, unname(predict(ref, lts_x[1:5, ])))
})

test_that("step_robpca() and the lts engine give a robust PCR workflow", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  d <- make_regression(seed = 25)
  train <- data.frame(y = d$train$y, d$train$x)
  train$y[1:8] <- train$y[1:8] + 15
  test <- data.frame(y = d$test$y, d$test$x)
  wf <- workflows::workflow(
    recipes::recipe(y ~ ., data = train) |> step_robpca(recipes::all_predictors(), num_comp = 3),
    parsnip::linear_reg() |> parsnip::set_engine("lts")
  )
  set.seed(26)
  fit <- parsnip::fit(wf, train)
  expect_lt(sqrt(mean((predict(fit, test)$.pred - test$y)^2)), 0.6)
})
