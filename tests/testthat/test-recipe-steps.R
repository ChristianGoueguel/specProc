skip_if_not_installed("recipes", "1.1.0")

d <- make_xy(n = 40, p = 25)
cal <- 1:30
dat <- data.frame(y = d$y, d$x)
train <- dat[cal, ]
test <- dat[-cal, ]
x_cal <- d$x[cal, ]
y_cal <- d$y[cal]
x_new <- d$x[-cal, ]
clutter <- x_cal[1:5, ] - x_cal[6:10, ]

baked <- function(step_fun, ..., new_data = test) {
  rec <- recipes::recipe(y ~ ., data = train)
  prepped <- recipes::prep(step_fun(rec, recipes::all_predictors(), ...), training = train)
  list(
    train = as.matrix(recipes::bake(prepped, new_data = NULL)[colnames(x_cal)]),
    new = as.matrix(recipes::bake(prepped, new_data = new_data)[colnames(x_cal)]),
    prepped = prepped
  )
}

test_that("response-orthogonal steps reproduce the filters and predict()", {
  cases <- list(
    list(step_osc, list(method = "fearn", num_comp = 2), osc(x_cal, y_cal, method = "fearn", ncomp = 2)),
    list(step_osc, list(method = "wold", num_comp = 2), osc(x_cal, y_cal, method = "wold", ncomp = 2)),
    list(step_direct_orthogonal, list(num_comp = 2), direct_orthogonal(x_cal, y_cal, ncomp = 2)),
    list(step_direct_osc, list(num_comp = 2), direct_osc(x_cal, y_cal, ncomp = 2)),
    list(step_projected_osc, list(num_comp = 2), projected_osc(x_cal, y_cal, ncomp = 3)),
    list(step_opls, list(num_comp = 2), opls(x_cal, y_cal, ncomp.ortho = 2, permutation = 0)),
    list(step_o2pls, list(num_comp = 2), o2pls(x_cal, y_cal, ncomp = 1, nx = 2)),
    list(step_opls, list(num_comp = 1, options = list(scale = "pareto")),
         opls(x_cal, y_cal, scale = "pareto", ncomp.ortho = 1, permutation = 0))
  )
  for (case in cases) {
    res <- do.call(baked, c(list(case[[1]]), case[[2]]))
    fit <- case[[3]]
    expect_equal(res$train, as.matrix(fit$correction), ignore_attr = TRUE)
    expect_equal(res$new, as.matrix(predict(fit, x_new)), ignore_attr = TRUE)
  }
})

test_that("step_epo reproduces epo() with and without clutter", {
  res <- baked(step_epo, clutter = clutter, num_comp = 2)
  fit <- epo(x_cal, ncomp = 2, clutter = clutter)
  expect_equal(res$new, as.matrix(predict(fit, x_new)), ignore_attr = TRUE)

  res <- baked(step_epo, num_comp = 1)
  fit <- epo(x_cal, ncomp = 1)
  expect_equal(res$train, as.matrix(fit$correction), ignore_attr = TRUE)
})

test_that("step_epo matches the clutter columns by name", {
  shuffled <- clutter[, rev(colnames(clutter))]
  expect_equal(baked(step_epo, clutter = shuffled)$new, baked(step_epo, clutter = clutter)$new)
  expect_error(baked(step_epo, clutter = clutter[, -1]), "no column")
})

test_that("step_glsw equals the glsw() filter with a relative alpha", {
  x1 <- x_cal[1:5, ]
  x2 <- x_cal[6:10, ]
  res <- baked(step_glsw, clutter = x2 - x1, alpha = 0.01)
  lambda <- max(svd(scale(x2 - x1, scale = FALSE))$d^2)
  G <- as.matrix(glsw(x1, x2, alpha = 0.01 * lambda))
  expect_equal(res$new, x_new %*% G, ignore_attr = TRUE)
  expect_equal(res$prepped$steps[[1]]$res$alpha, 0.01 * lambda)
})

test_that("step_y_gradient_glsw equals y_gradient_glsw()", {
  res <- baked(step_y_gradient_glsw, alpha = 0.05)
  alpha_abs <- res$prepped$steps[[1]]$res$alpha
  G <- as.matrix(y_gradient_glsw(x_cal, y_cal, alpha = alpha_abs))
  expect_equal(res$train, x_cal %*% G, ignore_attr = TRUE)
  expect_equal(res$new, x_new %*% G, ignore_attr = TRUE)
})

test_that("baking new data does not need the outcome", {
  res <- baked(step_direct_osc, num_comp = 2, new_data = test[-1])
  expect_equal(res$new, baked(step_direct_osc, num_comp = 2)$new)
})

test_that("the outcome can be given explicitly and must be single and numeric", {
  two <- data.frame(y2 = d$y^2, dat)[cal, ]
  rec <- recipes::recipe(y + y2 ~ ., data = two)
  expect_error(recipes::prep(step_osc(rec, recipes::all_predictors())), "single outcome")
  prepped <- recipes::prep(step_osc(rec, recipes::all_predictors(), outcome = y, num_comp = 1))
  expect_equal(prepped$steps[[1]]$outcome, "y")
})

test_that("step_o2pls filters the predictors against several outcomes", {
  set.seed(4)
  two <- data.frame(y2 = x_cal[, 2] - x_cal[, 3] + stats::rnorm(30, sd = 0.1), train)
  new_two <- data.frame(y2 = 0, test)
  rec <- recipes::recipe(y + y2 ~ ., data = two) |>
    step_o2pls(recipes::all_predictors(), num_comp = 2, joint_comp = 2)
  prepped <- recipes::prep(rec)
  fit <- o2pls(x_cal, cbind(y_cal, two$y2), ncomp = 2, nx = 2)
  cols <- colnames(x_cal)
  expect_equal(prepped$steps[[1]]$outcome, c("y", "y2"))
  expect_equal(as.matrix(recipes::bake(prepped, new_data = NULL)[cols]), as.matrix(fit$correction),
               ignore_attr = TRUE)
  baked_new <- recipes::bake(prepped, new_data = new_two)
  expect_equal(as.matrix(baked_new[cols]), as.matrix(predict(fit, x_new)), ignore_attr = TRUE)
  # the outcomes are not filtered, and are not needed to bake new data
  expect_equal(baked_new$y2, new_two$y2)
  expect_equal(recipes::bake(prepped, new_data = test[cols])[cols], baked_new[cols])

  # a subset of the outcomes
  one <- recipes::prep(step_o2pls(recipes::recipe(y + y2 ~ ., data = two), recipes::all_predictors(),
                                  outcome = y, num_comp = 2))
  expect_equal(one$steps[[1]]$outcome, "y")

  td <- recipes::tidy(prepped, number = 1)
  expect_named(td, c("terms", "num_comp", "joint_comp", "id"))
  expect_equal(unique(td$joint_comp), 2)
  expect_equal(generics::tunable(rec)$name, c("num_comp", "joint_comp"))
  expect_true(any(grepl("O2PLS filter on", cli::cli_fmt(print(prepped)))))
  expect_error(
    recipes::prep(step_o2pls(recipes::recipe(y + y2 ~ ., data = two), recipes::all_predictors(),
                             joint_comp = 3)),
    "cannot exceed"
  )
})

test_that("step_o2pls uses outcomes of another role, not needed to bake", {
  set.seed(4)
  two <- data.frame(y2 = x_cal[, 2] - x_cal[, 3] + stats::rnorm(30, sd = 0.1), train)
  # one outcome for the model, the filter estimated against both
  rec <- recipes::recipe(y ~ ., data = two) |>
    recipes::update_role(y2, new_role = "reference") |>
    recipes::update_role_requirements("reference", bake = FALSE) |>
    step_o2pls(recipes::all_predictors(), outcome = c(recipes::all_outcomes(), recipes::has_role("reference")),
               num_comp = 2, joint_comp = 2)
  prepped <- recipes::prep(rec)
  expect_equal(prepped$steps[[1]]$outcome, c("y", "y2"))
  fit <- o2pls(x_cal, cbind(y_cal, two$y2), ncomp = 2, nx = 2)
  baked_new <- recipes::bake(prepped, new_data = test[colnames(x_cal)])
  expect_equal(as.matrix(baked_new[colnames(x_cal)]), as.matrix(predict(fit, x_new)), ignore_attr = TRUE)
})

test_that("step_o2pls works in a workflow predicting several outcomes", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("plsmod")
  skip_if_not_installed("mixOmics")
  set.seed(4)
  two <- data.frame(y2 = d$x[, 2] - d$x[, 3] + stats::rnorm(40, sd = 0.1), dat)
  rec <- recipes::recipe(y + y2 ~ ., data = two[cal, ]) |>
    step_o2pls(recipes::all_predictors(), num_comp = 1, joint_comp = 2)
  model <- parsnip::set_engine(parsnip::pls(num_comp = 2, mode = "regression"), "mixOmics", scale = FALSE)
  wf_fit <- generics::fit(workflows::workflow(rec, model), data = two[cal, ])
  pred <- predict(wf_fit, new_data = two[-cal, ])
  expect_named(pred, c(".pred_y", ".pred_y2"))
  expect_gt(stats::cor(pred$.pred_y, two$y[-cal]), 0.9)
  expect_gt(stats::cor(pred$.pred_y2, two$y2[-cal]), 0.9)
})

test_that("unselected columns are left untouched", {
  rec <- recipes::recipe(y ~ ., data = train) |>
    step_direct_osc(dplyr::all_of(colnames(x_cal)[1:10]), num_comp = 1)
  out <- recipes::bake(recipes::prep(rec), new_data = test)
  expect_equal(out[colnames(x_cal)[11:25]], tibble::as_tibble(test[colnames(x_cal)[11:25]]))
})

test_that("steps have tidy, tunable, print and required_pkgs methods", {
  rec <- recipes::recipe(y ~ ., data = train) |>
    step_osc(recipes::all_predictors(), num_comp = 2) |>
    step_glsw(recipes::all_predictors(), clutter = clutter)
  prepped <- recipes::prep(rec)

  td <- recipes::tidy(prepped, number = 1)
  expect_named(td, c("terms", "num_comp", "id"))
  expect_equal(td$terms, colnames(x_cal))
  expect_named(recipes::tidy(rec, number = 2), c("terms", "alpha", "id"))

  tn <- generics::tunable(rec)
  expect_equal(tn$name, c("num_comp", "alpha"))
  expect_equal(tn$call_info[[2]]$fun, "glsw_alpha")

  printed <- cli::cli_fmt(print(prepped))
  expect_true(any(grepl("Orthogonal signal correction on", printed)))
  expect_true(any(grepl("GLSW filter on", printed)))
  expect_equal(generics::required_pkgs(rec), c("recipes", "specProc"))
})

test_that("glsw_alpha is a log10 dials parameter", {
  skip_if_not_installed("dials")
  p <- glsw_alpha()
  expect_s3_class(p, "quant_param")
  expect_equal(dials::value_seq(p, 3), c(1e-4, 1e-2, 1))
})
