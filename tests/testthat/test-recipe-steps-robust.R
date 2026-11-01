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

set.seed(3)
pos <- data.frame(id = 1:80, a = rlnorm(80), b = rexp(80), c = rnorm(80))

transformation_recipe <- function(step_fun, data, ...) {
  recipes::recipe(~ ., data = data) |>
    recipes::update_role(id, new_role = "id") |>
    step_fun(recipes::all_predictors(), ...)
}

test_that("step_robust_boxcox applies the training transformation to new data", {
  prepped <- recipes::prep(transformation_recipe(step_robust_boxcox, pos[1:60, 1:3]))
  fit <- robust_boxcox(pos[1:60, 2:3])
  out <- recipes::bake(prepped, new_data = pos[61:80, 1:3])
  expect_equal(out[2:3], predict(fit, pos[61:80, 2:3]))
  expect_equal(out$id, 61:80)
  td <- recipes::tidy(prepped, number = 1)
  expect_named(td, c("terms", "lambda", "id"))
  expect_equal(td$lambda, fit$summary$lambda)
  # variables with negative values are left unchanged, with a warning
  expect_warning(prepped <- recipes::prep(transformation_recipe(step_robust_boxcox, pos[1:60, ])),
                 "strictly positive.*\\(c\\)")
  expect_equal(recipes::tidy(prepped, number = 1)$lambda[3], NA_real_)
  expect_equal(recipes::bake(prepped, new_data = pos[61:80, ])$c, pos$c[61:80])
})

test_that("step_robust_yeojohnson applies the training transformation to new data", {
  rec <- transformation_recipe(step_robust_yeojohnson, pos[1:60, ], standardize = FALSE)
  expect_no_warning(prepped <- recipes::prep(rec))
  fit <- robust_yeojohnson(pos[1:60, 2:4], standardize = FALSE)
  out <- recipes::bake(prepped, new_data = pos[61:80, ])
  expect_equal(out[2:4], predict(fit, pos[61:80, 2:4]))
  td <- recipes::tidy(prepped, number = 1)
  expect_equal(td$terms, c("a", "b", "c"))
  expect_equal(td$lambda, fit$summary$lambda)
  # before training
  expect_equal(recipes::tidy(rec, number = 1)$lambda, NA_real_)
  # missing values are ignored by the fit and stay missing
  na <- pos
  na$a[c(2, 65)] <- NA
  prepped <- recipes::prep(transformation_recipe(step_robust_yeojohnson, na[1:60, ]))
  fit <- robust_yeojohnson(na[1:60, 2:4])
  out <- recipes::bake(prepped, new_data = na[61:80, ])
  expect_equal(out[2:4], predict(fit, na[61:80, 2:4]))
  expect_true(is.na(out$a[5]))
})

test_that("the transformation steps have print, tunable and required_pkgs methods", {
  rec <- transformation_recipe(step_robust_boxcox, pos[1:60, 1:3]) |>
    step_robust_yeojohnson(recipes::all_predictors())
  prepped <- recipes::prep(rec)
  printed <- cli::cli_fmt(print(prepped))
  expect_true(any(grepl("Robust Box-Cox transformation on", printed)))
  expect_true(any(grepl("Robust Yeo-Johnson transformation on", printed)))
  expect_equal(nrow(generics::tunable(rec)), 0)
  expect_true("specProc" %in% generics::required_pkgs(rec))
  rec <- transformation_recipe(step_robust_boxcox, pos[1:60, 1:3], quantile = 2)
  expect_error(recipes::prep(rec), "'quantile'")
  rec <- transformation_recipe(step_robust_yeojohnson, pos[1:60, 1:3], nbsteps = 0)
  expect_error(recipes::prep(rec), "'nbsteps'")
})

test_that("step_robust_bcyj is deprecated but still works", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(rec <- transformation_recipe(step_robust_bcyj, pos[1:60, ]),
                 "step_robust_boxcox")
  prepped <- recipes::prep(rec)
  ref <- robust_transformation(as.matrix(pos[1:60, 2:4]))
  out <- recipes::bake(prepped, new_data = pos[61:80, ])
  expect_equal(as.matrix(out[2:4]), apply_transformation(as.matrix(pos[61:80, 2:4]), ref),
               ignore_attr = TRUE)
  td <- recipes::tidy(prepped, number = 1)
  expect_named(td, c("terms", "lambda", "method", "id"))
  expect_equal(td$lambda, unname(vapply(ref$fits, `[[`, numeric(1), "lambda")))
  expect_true(any(grepl("Robust Box-Cox/Yeo-Johnson transformation on",
                        cli::cli_fmt(print(prepped)))))
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

test_that("step_rsimpls adds the robust PLS scores of a model of the outcome", {
  set.seed(5)
  y <- drop(x %*% c(1, -1, 0.5, rep(0, 7))) + rnorm(n, sd = 0.5)
  dat_y <- data.frame(dat, y = y)
  rec <- recipes::recipe(y ~ ., data = dat_y[1:70, ]) |>
    recipes::update_role(id, new_role = "id") |>
    step_rsimpls(recipes::all_predictors(), num_comp = 3, distances = TRUE)
  set.seed(6)
  prepped <- recipes::prep(rec)
  out <- recipes::bake(prepped, new_data = dat_y[71:90, ])
  expect_named(out, c("id", "y", "RPLS1", "RPLS2", "RPLS3", "RPLS_SD", "RPLS_OD"))
  set.seed(6)
  fit <- rsimpls(x_train, y[1:70], ncomp = 3)
  pred <- predict(fit, x_test, type = "scores")
  expect_equal(as.matrix(out[3:5]), as.matrix(pred[1:3]), ignore_attr = TRUE)
  expect_equal(out$RPLS_SD, pred$sd)
  expect_equal(out$RPLS_OD, pred$od)
  # the outcome is not needed to bake
  expect_named(recipes::bake(prepped, new_data = test),
               c("id", "RPLS1", "RPLS2", "RPLS3", "RPLS_SD", "RPLS_OD"))

  td <- recipes::tidy(prepped, number = 1)
  expect_named(td, c("terms", "value", "component", "id"))
  expect_equal(td$value, as.vector(fit$x_weights))
  expect_equal(unique(td$component), c("RPLS1", "RPLS2", "RPLS3"))
  expect_equal(recipes::tidy(rec, number = 1)$value, rep(NA_real_, 1))
  expect_equal(generics::tunable(rec)$name, "num_comp")
  expect_true(any(grepl("Robust PLS \\(RSIMPLS\\) on", cli::cli_fmt(print(prepped)))))
  expect_true("specProc" %in% generics::required_pkgs(rec))
})

test_that("step_rsimpls models several outcomes and needs one", {
  set.seed(7)
  dat_y <- data.frame(dat, y1 = drop(x %*% c(1, -1, rep(0, 8))) + rnorm(n, sd = 0.5),
                      y2 = drop(x %*% c(0, 0, 1, 1, rep(0, 6))) + rnorm(n, sd = 0.5))
  rec <- recipes::recipe(y1 + y2 ~ ., data = dat_y[1:70, ]) |>
    recipes::update_role(id, new_role = "id") |>
    step_rsimpls(recipes::all_predictors(), num_comp = 2)
  set.seed(8)
  prepped <- recipes::prep(rec)
  expect_equal(prepped$steps[[1]]$outcome, c("y1", "y2"))
  expect_equal(dim(prepped$steps[[1]]$res$coefficients), c(10L, 2L))
  # a chosen outcome
  rec1 <- recipes::recipe(y1 + y2 ~ ., data = dat_y[1:70, ]) |>
    recipes::update_role(id, new_role = "id") |>
    step_rsimpls(recipes::all_predictors(), num_comp = 2, outcome = "y2")
  set.seed(8)
  expect_equal(recipes::prep(rec1)$steps[[1]]$outcome, "y2")
  expect_error(prep_step(step_rsimpls), "needs an outcome")
  train_na <- train
  train_na$v1[3] <- NA
  expect_error(prep_step(step_rsimpls, data = train_na), "impute")
})
