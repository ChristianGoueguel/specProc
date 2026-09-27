skip_if_not_installed("recipes", "1.1.0")

s <- make_spectra(n = 16, p = 200)
x <- s$x
train_x <- x[1:10, ]
new_x <- x[11:16, ]
dat <- data.frame(id = seq_len(nrow(x)), x, check.names = FALSE)
train <- dat[1:10, ]
test <- dat[11:16, ]
channels <- colnames(x)

# Preps a one-step recipe on `train` and bakes `test`; returns the spectra.
bake_step <- function(step_fun, ...) {
  rec <- recipes::recipe(~ ., data = train) |>
    recipes::update_role(id, new_role = "id")
  prepped <- recipes::prep(step_fun(rec, recipes::all_predictors(), ...), training = train)
  list(
    train = as.matrix(recipes::bake(prepped, new_data = NULL)[channels]),
    new = as.matrix(recipes::bake(prepped, new_data = test)[channels]),
    prepped = prepped
  )
}

test_that("step_baseline reproduces the baseline functions", {
  res <- bake_step(step_baseline, method = "arpls", lambda = 1e4)
  expect_equal(res$new, as.matrix(baseline_arpls(new_x, lambda = 1e4)$correction), ignore_attr = TRUE)

  res <- bake_step(step_baseline, method = "als", lambda = 1e4, options = list(p = 0.01))
  expect_equal(res$new, as.matrix(baseline_als(new_x, lambda = 1e4, p = 0.01)$correction), ignore_attr = TRUE)

  res <- bake_step(step_baseline, method = "lsp", degree = 3)
  expect_equal(res$train, as.matrix(baseline_lsp(train_x, degree = 3)$correction), ignore_attr = TRUE)
})

test_that("step_snv reproduces snv()", {
  res <- bake_step(step_snv)
  expect_equal(res$new, as.matrix(snv(new_x)$correction), ignore_attr = TRUE)
})

test_that("step_msc corrects new spectra against the training reference", {
  res <- bake_step(step_msc)
  cal <- msc(train_x)
  expect_equal(res$train, as.matrix(cal$correction), ignore_attr = TRUE)
  expect_equal(res$new, as.matrix(msc(new_x, xref = cal$reference)$correction), ignore_attr = TRUE)
  expect_equal(recipes::tidy(res$prepped, number = 1)$reference, cal$reference)
})

test_that("step_emsc corrects new spectra with the training model", {
  res <- bake_step(step_emsc, degree = 2)
  cal <- emsc(train_x, degree = 2)
  expect_equal(res$train, as.matrix(cal$correction), ignore_attr = TRUE)
  expect_equal(res$new, as.matrix(predict(cal, new_x)), ignore_attr = TRUE)

  interferent <- train_x[1, ] - train_x[2, ]
  res <- bake_step(step_emsc, degree = 1, interferents = rbind(interferent))
  cal <- emsc(train_x, degree = 1, interferents = interferent)
  expect_equal(res$new, as.matrix(predict(cal, new_x)), ignore_attr = TRUE)
})

test_that("step_pareto_scale and step_poisson_scale use the training scales", {
  res <- bake_step(step_pareto_scale)
  expect_equal(res$train, pareto_scale(train_x), ignore_attr = TRUE)
  expect_equal(res$new, sweep(new_x, 2, sqrt(apply(train_x, 2, sd)), "/"), ignore_attr = TRUE)

  res <- bake_step(step_poisson_scale, offset = 5)
  sc <- poisson_scale(train_x, options = list(offset = 5))$sc
  expect_equal(res$new, poisson_scale(new_x, sc = sc), ignore_attr = TRUE)
  expect_equal(recipes::tidy(res$prepped, number = 1)$scale, unname(sc))
})

test_that("preprocessing and orthogonalization steps chain in one recipe", {
  y <- as.numeric(x[, 50]) + seq_len(nrow(x))
  d <- data.frame(y = y, x, check.names = FALSE)
  rec <- recipes::recipe(y ~ ., data = d[1:10, ]) |>
    step_baseline(recipes::all_predictors(), lambda = 1e4) |>
    step_snv(recipes::all_predictors()) |>
    step_osc(recipes::all_predictors(), method = "fearn", num_comp = 1)
  out <- recipes::bake(recipes::prep(rec), new_data = d[11:16, -1])
  expected <- snv(baseline_arpls(new_x, lambda = 1e4)$correction)$correction
  fit <- osc(snv(baseline_arpls(train_x, lambda = 1e4)$correction)$correction, y[1:10],
             method = "fearn", ncomp = 1)
  expect_equal(as.matrix(out[channels]), as.matrix(predict(fit, expected)), ignore_attr = TRUE)
})

test_that("preprocessing steps have tidy, tunable, print and required_pkgs methods", {
  rec <- recipes::recipe(~ ., data = train) |>
    recipes::update_role(id, new_role = "id") |>
    step_baseline(recipes::all_predictors()) |>
    step_baseline(recipes::all_predictors(), method = "lsp") |>
    step_snv(recipes::all_predictors()) |>
    step_emsc(recipes::all_predictors()) |>
    step_pareto_scale(recipes::all_predictors())
  expect_equal(recipes::tidy(rec, number = 1)$method[1], "arpls")
  expect_named(recipes::tidy(rec, number = 5), c("terms", "scale", "id"))

  tn <- generics::tunable(rec)
  expect_equal(tn$name, c("lambda", "degree", "degree"))
  expect_equal(tn$call_info[[1]]$fun, "baseline_lambda")

  printed <- cli::cli_fmt(print(recipes::prep(rec)))
  expect_true(any(grepl("Extended multiplicative signal correction on", printed)))
  expect_true("specProc" %in% generics::required_pkgs(rec))
})

test_that("baseline_lambda is a log10 dials parameter", {
  skip_if_not_installed("dials")
  expect_equal(dials::value_seq(baseline_lambda(), 4), 10^c(2, 4, 6, 8))
})
