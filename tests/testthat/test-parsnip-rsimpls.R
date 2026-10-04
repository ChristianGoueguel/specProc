skip_if_not_installed("parsnip")

# Spectra-like data with three latent variables, and two responses linear in
# them, with a few wrong reference values.
set.seed(1)
n <- 90
p <- 40
latent <- matrix(rnorm(n * 3), n, 3) %*% diag(c(8, 5, 3))
x <- latent %*% t(qr.Q(qr(matrix(rnorm(p * 3), p, 3)))) + matrix(rnorm(n * p, sd = 0.3), n, p)
colnames(x) <- paste0("w", seq_len(p))
dat <- data.frame(y1 = drop(latent %*% c(0.3, -0.5, 0.8)) + rnorm(n, sd = 0.3),
                  y2 = drop(latent %*% c(-0.2, 0.4, 0.1)) + rnorm(n, sd = 0.3), x)
dat$y1[1:5] <- dat$y1[1:5] + 10
train <- dat[1:70, ]
test <- dat[71:90, ]

# parsnip captures its arguments unevaluated, so the value of num_comp (a
# number, or the call tune()) is injected
rsimpls_spec <- function(num_comp, ...) {
  parsnip::pls(num_comp = !!num_comp) |>
    parsnip::set_engine("rsimpls", ...) |>
    parsnip::set_mode("regression")
}

test_that("the rsimpls engine of parsnip::pls() fits and predicts like rsimpls()", {
  expect_true("rsimpls" %in% parsnip::show_engines("pls")$engine)
  set.seed(2)
  fit <- parsnip::fit(rsimpls_spec(3), y1 ~ . - y2, data = train)
  set.seed(2)
  ref <- rsimpls(train[-(1:2)], train$y1, ncomp = 3)
  engine_fit <- parsnip::extract_fit_engine(fit)
  expect_s3_class(engine_fit, "specproc_rsimpls")
  expect_equal(engine_fit$coefficients, ref$coefficients, ignore_attr = TRUE)
  pred <- predict(fit, test)
  expect_named(pred, ".pred")
  expect_equal(pred$.pred, unname(predict(ref, test[-(1:2)])))
  expect_match(paste(capture.output(parsnip::translate(rsimpls_spec(3))), collapse = " "),
               "ncomp = 3")
})

test_that("engine arguments are passed to rsimpls()", {
  set.seed(3)
  fit <- parsnip::fit(rsimpls_spec(2, kmax = 4, alpha = 0.9), y1 ~ . - y2, data = train)
  engine_fit <- parsnip::extract_fit_engine(fit)
  expect_equal(engine_fit$kmax, 4L)
  expect_equal(engine_fit$alpha, 0.9)
})

test_that("multi_predict() gives the models with fewer components of the same fit", {
  set.seed(4)
  fit <- parsnip::fit(rsimpls_spec(4), y1 ~ . - y2, data = train)
  mp <- parsnip::multi_predict(fit, test, num_comp = c(3, 1, 2))
  expect_equal(nrow(mp), nrow(test))
  expect_named(mp$.pred[[1]], c("num_comp", ".pred"))
  expect_identical(mp$.pred[[1]]$num_comp, 1:3)
  set.seed(4)
  fit2 <- parsnip::fit(rsimpls_spec(2), y1 ~ . - y2, data = train)
  expect_equal(vapply(mp$.pred, function(d) d$.pred[2], numeric(1)), predict(fit2, test)$.pred)
  # default: the number of components of the fit
  expect_identical(parsnip::multi_predict(fit, test[1:2, ])$.pred[[1]]$num_comp, 4L)
  expect_error(parsnip::multi_predict(fit, newdata = test), "new_data")
  expect_error(parsnip::multi_predict(fit, test, type = "class"), "numeric")
})

test_that("the rsimpls engine handles several outcomes", {
  set.seed(5)
  fit <- parsnip::fit(rsimpls_spec(3), cbind(y1, y2) ~ ., data = train)
  pred <- predict(fit, test)
  expect_named(pred, c(".pred_y1", ".pred_y2"))
  mp <- parsnip::multi_predict(fit, test[1:3, ], num_comp = 1:3)
  expect_named(mp$.pred[[1]], c("num_comp", ".pred_y1", ".pred_y2"))
  expect_equal(mp$.pred[[2]]$.pred_y2[3], pred$.pred_y2[2])
})

test_that("tune_grid() fits one rsimpls() model per resample for num_comp", {
  skip_if_not_installed("tune")
  skip_if_not_installed("workflows")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")
  wf <- workflows::workflow(y1 ~ . - y2, rsimpls_spec(tune::tune()))
  set.seed(6)
  folds <- rsample::vfold_cv(train, v = 3)
  # count the fits: the counter is an environment inlined in the traced code
  fits <- new.env()
  fits$n <- 0
  suppressMessages(trace("rsimpls", bquote(assign("n", .(fits)$n + 1, envir = .(fits))),
                         where = asNamespace("specProc"), print = FALSE))
  on.exit(suppressMessages(untrace("rsimpls", where = asNamespace("specProc"))), add = TRUE)
  res <- tune::tune_grid(wf, folds, grid = data.frame(num_comp = 1:3),
                         metrics = yardstick::metric_set(yardstick::rmse))
  expect_equal(fits$n, 3)
  expect_equal(nrow(tune::collect_metrics(res)), 3L)
})
