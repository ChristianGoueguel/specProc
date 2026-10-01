skip_if_not_installed("recipes", "1.1.0")

set.seed(1)
n <- 90
x <- matrix(rnorm(n * 10), n, 10) %*% diag(10:1)
x[1:4, ] <- x[1:4, ] + 25
colnames(x) <- paste0("v", 1:10)
dat <- data.frame(id = seq_len(n), x)
train <- dat[1:70, ]
test <- dat[71:90, ]
x_train <- x[1:70, ]
x_test <- x[71:90, ]

# The seed is set after the step is created, because creating a step draws
# a random id.
prep_step <- function(step_fun, ..., data = train, seed = 2) {
  rec <- recipes::recipe(~ ., data = data) |>
    recipes::update_role(id, new_role = "id") |>
    step_fun(recipes::all_predictors(), ...)
  set.seed(seed)
  recipes::prep(rec, training = data)
}

test_that("step_robust_bcyj applies the training transformation to new data", {
  set.seed(3)
  pos <- data.frame(id = 1:80, a = rlnorm(80), b = rexp(80), c = rnorm(80))
  rec <- recipes::recipe(~ ., data = pos[1:60, ]) |>
    recipes::update_role(id, new_role = "id") |>
    step_robust_bcyj(recipes::all_predictors())
  prepped <- recipes::prep(rec)
  ref <- robust_transformation(as.matrix(pos[1:60, 2:4]))
  out <- recipes::bake(prepped, new_data = pos[61:80, ])
  expect_equal(as.matrix(out[2:4]), apply_transformation(as.matrix(pos[61:80, 2:4]), ref),
               ignore_attr = TRUE)
  td <- recipes::tidy(prepped, number = 1)
  expect_named(td, c("terms", "lambda", "method", "id"))
  expect_equal(td$lambda, unname(vapply(ref$fits, `[[`, numeric(1), "lambda")))
  expect_equal(out$id, 61:80)
})

test_that("step_robpca adds robust scores and removes the originals", {
  prepped <- prep_step(step_robpca, num_comp = 3)
  out <- recipes::bake(prepped, new_data = test)
  expect_named(out, c("id", "RPC1", "RPC2", "RPC3"))
  set.seed(2)
  fit <- robpca(x_train, k = 3)
  expect_equal(as.matrix(out[2:4]), as.matrix(predict(fit, x_test)[1:3]), ignore_attr = TRUE)
})

test_that("step_robpca can keep the originals and add distances", {
  prepped <- prep_step(step_robpca, num_comp = 2, distances = TRUE, keep_original_cols = TRUE)
  out <- recipes::bake(prepped, new_data = test)
  expect_true(all(c(colnames(x), "RPC1", "RPC2", "RPC_SD", "RPC_OD") %in% names(out)))
  set.seed(2)
  pred <- predict(robpca(x_train, k = 2), x_test)
  expect_equal(out$RPC_SD, pred$sd)
  expect_equal(out$RPC_OD, pred$od)
})

test_that("step_rospca and step_macropca produce scores", {
  prepped <- prep_step(step_rospca, num_comp = 2, lambda = 0.5)
  out <- recipes::bake(prepped, new_data = test)
  expect_named(out, c("id", "RSPC1", "RSPC2"))

  test_na <- test
  test_na$v3[2] <- NA
  prepped <- prep_step(step_macropca, num_comp = 2)
  out <- recipes::bake(prepped, new_data = test_na)
  expect_named(out, c("id", "MPC1", "MPC2"))
  expect_false(anyNA(out$MPC1))
})

test_that("robust PCA steps reject missing values when they cannot handle them", {
  train_na <- train
  train_na$v1[3] <- NA
  expect_error(prep_step(step_robpca, data = train_na), "step_macropca")
})

test_that("robust steps have tidy, tunable, print and required_pkgs methods", {
  rec <- recipes::recipe(~ ., data = train) |>
    recipes::update_role(id, new_role = "id") |>
    step_robpca(recipes::all_predictors(), num_comp = 2) |>
    step_rospca(recipes::all_predictors(), num_comp = 2)
  td <- recipes::tidy(rec, number = 1)
  expect_named(td, c("terms", "value", "component", "id"))

  set.seed(4)
  prepped <- recipes::prep(recipes::recipe(~ ., data = train) |>
    recipes::update_role(id, new_role = "id") |>
    step_robpca(recipes::all_predictors(), num_comp = 2))
  td <- recipes::tidy(prepped, number = 1)
  expect_equal(nrow(td), 10 * 2)
  expect_equal(unique(td$component), c("RPC1", "RPC2"))

  tn <- generics::tunable(rec)
  expect_equal(tn$name, c("num_comp", "num_comp", "lambda"))
  expect_equal(tn$call_info[[3]]$fun, "penalty")

  printed <- cli::cli_fmt(print(prepped))
  expect_true(any(grepl("Robust PCA \\(ROBPCA\\) on", printed)))
  expect_true("specProc" %in% generics::required_pkgs(rec))
  expect_false("cellWise" %in% generics::required_pkgs(rec))
})
