skip_if_not_installed("parsnip")

# Two classes lying near different two-dimensional subspaces of a
# 30-dimensional space.
set.seed(1)
p <- 30
basis_a <- qr.Q(qr(matrix(rnorm(p * 2), p, 2)))
basis_b <- qr.Q(qr(matrix(rnorm(p * 2), p, 2)))
draw <- function(m) {
  scores <- function() matrix(rnorm(m * 2), m) %*% diag(c(5, 3))
  noise <- function() matrix(rnorm(m * p, sd = 0.2), m)
  x <- rbind(scores() %*% t(basis_a) + noise(), scores() %*% t(basis_b) + noise() + 1)
  colnames(x) <- paste0("w", seq_len(p))
  data.frame(class = factor(rep(c("a", "b"), each = m)), x)
}
train <- draw(30)
test <- draw(20)

test_that("simca() is a parsnip model with the rsimca engine", {
  spec <- simca(num_comp = 2) |> parsnip::set_engine("rsimca", gamma = 0.7)
  expect_s3_class(spec, c("simca", "model_spec"))
  expect_output(print(spec), "SIMCA Model Specification")
  expect_true("rsimca" %in% parsnip::show_engines("simca")$engine)
  expect_match(paste(capture.output(parsnip::translate(spec)), collapse = " "),
               "rsimca\\(x = missing_arg\\(\\), group = missing_arg\\(\\), ncomp = 2")
  expect_error(simca(mode = "regression"), "classification")
  updated <- update(simca(num_comp = 2), num_comp = 4)
  expect_equal(rlang::eval_tidy(updated$args$num_comp), 4)
  updated <- update(simca(num_comp = 2), parameters = data.frame(num_comp = 3))
  expect_equal(rlang::eval_tidy(updated$args$num_comp), 3)
})

test_that("a fitted simca model predicts like rsimca()", {
  spec <- simca(num_comp = 2) |> parsnip::set_engine("rsimca", gamma = 0.7)
  set.seed(2)
  fit <- parsnip::fit(spec, class ~ ., data = train)
  set.seed(2)
  ref <- rsimca(train[-1], train$class, ncomp = 2, gamma = 0.7)
  engine_fit <- parsnip::extract_fit_engine(fit)
  expect_s3_class(engine_fit, "specproc_rsimca")
  expect_equal(engine_fit$gamma, 0.7)
  pred <- predict(fit, test)
  expect_named(pred, ".pred_class")
  expect_identical(pred$.pred_class, predict(ref, test[-1]))
  expect_gt(mean(pred$.pred_class == test$class), 0.95)
  expect_equal(predict(fit, test, type = "raw"), predict(ref, test[-1], type = "distances"))
  expect_error(predict(fit, test, type = "prob"), "prob")
  # without num_comp, robpca() chooses the number of components
  set.seed(3)
  expect_true(all(parsnip::extract_fit_engine(parsnip::fit(simca(), class ~ ., data = train))$ncomp >= 1))
})

test_that("simca() can be tuned with class metrics", {
  skip_if_not_installed("tune")
  skip_if_not_installed("workflows")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")
  wf <- workflows::workflow(class ~ ., simca(num_comp = tune::tune()))
  set.seed(4)
  folds <- rsample::vfold_cv(train, v = 3, strata = class)
  res <- tune::tune_grid(wf, folds, grid = data.frame(num_comp = 1:3),
                         metrics = yardstick::metric_set(yardstick::accuracy))
  metrics <- tune::collect_metrics(res)
  expect_equal(metrics$num_comp, 1:3)
  best <- tune::select_best(res, metric = "accuracy")
  final <- tune::finalize_workflow(wf, best)
  set.seed(5)
  fit <- parsnip::fit(final, train)
  expect_equal(unname(parsnip::extract_fit_engine(fit)$ncomp), rep(best$num_comp, 2))
})
