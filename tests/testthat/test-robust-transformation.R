set.seed(1)
test_data <- tibble::tibble(
  lognormal = stats::rlnorm(100),
  gamma = stats::rgamma(100, shape = 2),
  normal = stats::rnorm(100),
  label = sample(letters, 100, replace = TRUE)
)
positive <- test_data[c("lognormal", "gamma")]

test_that("robust_boxcox transforms positive variables toward normality", {
  fit <- robust_boxcox(positive)
  expect_s3_class(fit, c("specproc_boxcox", "specproc_transformation"))
  expect_named(fit, c("transformation", "summary", "fits", "standardize"))
  expect_named(fit$summary, c("variable", "lambda", "objective"))
  expect_equal(fit$summary$variable, c("lognormal", "gamma"))
  expect_equal(dim(fit$transformation), c(100L, 2L))
  expect_named(fit$transformation, c("lognormal", "gamma"))
  # close to the log for the log-normal variable
  expect_lt(abs(fit$summary$lambda[1]), 0.3)
  expect_lt(abs(medcouple(fit$transformation$lognormal)), abs(medcouple(positive$lognormal)))
  ref <- robust_transformation(as.matrix(positive), type = "BC")
  expect_equal(fit$summary$lambda, unname(vapply(ref$fits, `[[`, numeric(1), "lambda")))
  expect_equal(as.matrix(fit$transformation), apply_transformation(as.matrix(positive), ref))
  # a vector, and a matrix without column names
  v <- robust_boxcox(positive$lognormal)
  expect_equal(v$summary$variable, "V1")
  expect_equal(v$transformation$V1, fit$transformation$lognormal)
  expect_equal(robust_boxcox(unname(as.matrix(positive)))$summary$variable, c("V1", "V2"))
})

test_that("robust_boxcox leaves variables with zero or negative values unchanged", {
  expect_warning(fit <- robust_boxcox(test_data[1:3]), "strictly positive.*\\(normal\\)")
  expect_equal(fit$summary$lambda[3], NA_real_)
  expect_equal(fit$summary$objective[3], NA_real_)
  expect_equal(fit$transformation$normal, test_data$normal)
  # many variables are listed in short
  x <- matrix(stats::rnorm(70), 10, dimnames = list(NULL, paste0("v", 1:7)))
  expect_warning(robust_boxcox(x), "7 variable\\(s\\).*\\(v1, v2, v3, v4, v5, \\.\\.\\.\\)")
})

test_that("robust_yeojohnson transforms variables with negative values", {
  set.seed(2)
  x <- data.frame(normal = stats::rnorm(150), negskew = -stats::rlnorm(150),
                  rightskew = stats::rlnorm(150) - 2)
  expect_no_warning(fit <- robust_yeojohnson(x))
  expect_s3_class(fit, c("specproc_yeojohnson", "specproc_transformation"))
  expect_equal(fit$summary$variable, names(x))
  expect_lt(abs(fit$summary$lambda[1] - 1), 0.3)
  expect_gt(fit$summary$lambda[2], 1)
  expect_lt(fit$summary$lambda[3], 1)
  mc <- medcouple(fit$transformation)
  expect_true(all(abs(mc[2:3]) < abs(medcouple(x)[2:3])))
  ref <- robust_transformation(as.matrix(x), type = "YJ")
  expect_equal(as.matrix(fit$transformation), apply_transformation(as.matrix(x), ref),
               ignore_attr = TRUE)
})

test_that("standardize = FALSE keeps the scale of the transformation", {
  fit <- robust_yeojohnson(positive, standardize = FALSE)
  expect_false(fit$standardize)
  v <- positive$gamma
  f <- fit$fits$gamma
  expect_equal(fit$transformation$gamma,
               yj_transform((v - stats::median(v)) / stats::mad(v), f$lambda))
  std <- robust_yeojohnson(positive)
  expect_equal(std$transformation$gamma, (fit$transformation$gamma - f$mean) / f$sd)
})

test_that("predict applies the transformation of the fit to new data", {
  fit <- robust_boxcox(positive[1:70, ])
  new <- predict(fit, positive[71:100, ])
  expect_s3_class(new, "tbl_df")
  expect_equal(as.matrix(new), apply_transformation(as.matrix(positive[71:100, ]), fit),
               ignore_attr = TRUE)
  # the training data give back the transformation
  expect_equal(predict(fit, positive[1:70, ]), fit$transformation)
  # the columns are matched by name, and can be a subset of the variables
  expect_equal(predict(fit, positive[71:100, 2:1]), new)
  expect_equal(predict(fit, positive[71:100, "gamma"]), new["gamma"])
  # zero, negative and missing values
  x <- positive[71:73, ]
  x$lognormal <- c(0, -1, NA)
  expect_true(all(is.na(predict(fit, x)$lognormal)))
  expect_false(anyNA(predict(fit, x)$gamma))
  expect_error(predict(fit, unname(as.matrix(positive[, 1]))), "2 column\\(s\\)")
  expect_error(predict(fit, data.frame(a = 1, gamma = 2)), "not found: a\\.")
  m <- as.matrix(positive[1:3, c(1, 1)])
  expect_error(predict(fit, m), "duplicated")
  expect_error(predict(fit), "newdata")
  # a single variable
  one <- robust_yeojohnson(positive$gamma)
  expect_equal(predict(one, positive$gamma)$V1, one$transformation$V1)
})

test_that("predict transforms values back to the original scale", {
  set.seed(4)
  x <- data.frame(gamma = stats::rgamma(80, 2), constant = 1, negskew = -stats::rlnorm(80),
                  rightskew = stats::rlnorm(80) - 2)
  x$gamma[3] <- NA
  bc <- suppressWarnings(robust_boxcox(x))
  expect_equal(predict(bc, bc$transformation, inverse = TRUE), tibble::as_tibble(x))
  for (standardize in c(TRUE, FALSE)) {
    yj <- robust_yeojohnson(x, standardize = standardize)
    expect_equal(predict(yj, yj$transformation, inverse = TRUE), tibble::as_tibble(x))
  }
  # a subset of the variables, and a vector for a single variable
  expect_equal(predict(yj, yj$transformation["negskew"], inverse = TRUE), tibble::as_tibble(x["negskew"]))
  one <- robust_boxcox(x$gamma)
  expect_equal(predict(one, one$transformation$V1, inverse = TRUE)$V1, x$gamma)
})

test_that("the inverse transformations are exact and NA beyond their range", {
  for (lambda in c(-1, -0.5, 0, 0.5, 1, 2, 3)) {
    v <- c(0.1, 0.5, 1, 2, 5)
    expect_equal(bc_inverse(bc_transform(v, lambda), lambda), v)
    v <- c(-5, -1, -0.2, 0, 0.3, 1, 4)
    expect_equal(yj_inverse(yj_transform(v, lambda), lambda), v)
  }
  # bounded above (at -1 / lambda) for lambda < 0
  expect_equal(bc_inverse(c(1, 2, 3, NA), -0.5), c(4, NA, NA, NA))
  expect_equal(is.na(yj_inverse(c(-1, 1, 3), -0.5)), c(FALSE, FALSE, TRUE))
  # and below (at -1 / (lambda - 2)) for lambda > 2
  expect_equal(is.na(yj_inverse(c(-3, -1, 1), 2.5)), c(TRUE, FALSE, FALSE))

  fit <- robust_boxcox(positive)
  fit$fits$lognormal$lambda <- -0.5
  expect_warning(back <- predict(fit, data.frame(lognormal = c(0, 1e6, NA)), inverse = TRUE),
                 "1 value\\(s\\) are beyond the range")
  expect_equal(is.na(back$lognormal), c(FALSE, TRUE, TRUE))
})

test_that("estimate = 'mean' gives the smearing estimate", {
  fit <- robust_boxcox(positive)
  z <- data.frame(lognormal = c(-1, 0, 1.5))
  e <- c(-0.6, -0.1, 0.2, 0.5, NA)
  avg <- predict(fit, z, inverse = TRUE, estimate = "mean", residuals = e)
  f <- fit$fits$lognormal
  manual <- vapply(z$lognormal, function(zi) mean(invert_values(zi + e[1:4], f, TRUE)), numeric(1))
  expect_equal(avg$lognormal, manual)
  # the mean is above the median for lambda < 1, and equal without errors
  med <- predict(fit, z, inverse = TRUE)
  expect_lt(f$lambda, 1)
  expect_true(all(predict(fit, z, inverse = TRUE, estimate = "mean",
                          residuals = c(-0.5, 0.5))$lognormal > med$lognormal))
  expect_equal(predict(fit, z, inverse = TRUE, estimate = "mean", residuals = 0), med)
  # residuals per variable, matched by name or by position
  z2 <- data.frame(gamma = c(0, 1), lognormal = c(0, 1))
  r <- data.frame(lognormal = c(-0.3, 0.3), gamma = c(-1, 1))
  both <- predict(fit, z2, inverse = TRUE, estimate = "mean", residuals = r)
  expect_named(both, c("lognormal", "gamma"))
  expect_equal(both$gamma, predict(fit, z2["gamma"], inverse = TRUE, estimate = "mean",
                                   residuals = c(-1, 1))$gamma)
  expect_equal(predict(fit, z2, inverse = TRUE, estimate = "mean", residuals = as.matrix(unname(r[2:1]))),
               predict(fit, z2, inverse = TRUE, estimate = "mean", residuals = r[2:1]))
  # a variable left unchanged: the values plus the mean of the residuals
  un <- robust_boxcox(data.frame(a = positive$gamma, constant = 1))
  expect_equal(predict(un, data.frame(constant = 2), inverse = TRUE, estimate = "mean",
                       residuals = c(0.1, 0.5))$constant, 2.3)
  # a residual that passes the bound
  fit$fits$lognormal$lambda <- -0.5
  bound <- (2 - f$mean) / f$sd
  expect_warning(out <- predict(fit, data.frame(lognormal = c(bound - 2, bound - 0.1)),
                                inverse = TRUE, estimate = "mean", residuals = c(-0.2, 0.2)),
                 "1 value\\(s\\) have no mean")
  expect_equal(is.na(out$lognormal), c(FALSE, TRUE))
})

test_that("predict checks the arguments of the back-transformation", {
  fit <- robust_boxcox(positive)
  z <- fit$transformation
  expect_error(predict(fit, z, inverse = NA), "'inverse'")
  expect_error(predict(fit, z, estimate = "mean"), "only with inverse = TRUE")
  expect_error(predict(fit, z, residuals = 1), "only with inverse = TRUE")
  expect_error(predict(fit, z, inverse = TRUE, estimate = "mode"), "should be one of")
  expect_error(predict(fit, z, inverse = TRUE, estimate = "mean"), "needs the 'residuals'")
  expect_error(predict(fit, z, inverse = TRUE, residuals = 1), "only with estimate = \"mean\"")
  expect_error(predict(fit, z, inverse = TRUE, estimate = "mean",
                       residuals = data.frame(gamma = 1)), "no column for: lognormal")
  expect_error(predict(fit, z, inverse = TRUE, estimate = "mean", residuals = matrix(1, 2, 3)),
               "one column per variable")
  expect_error(predict(fit, z, inverse = TRUE, estimate = "mean", residuals = NA_real_),
               "no values")
})

test_that("the transformations print a summary", {
  out <- capture.output(print(robust_boxcox(positive)))
  expect_equal(out[1], "Robust Box-Cox transformation")
  expect_true(any(grepl("^Transformed: +2$", out)))
  expect_true(any(grepl("^Lambda: +-?[0-9.]+ to -?[0-9.]+$", out)))
  out <- capture.output(print(robust_yeojohnson(test_data[1:3])))
  expect_equal(out[1], "Robust Yeo-Johnson transformation")
  x <- data.frame(a = positive$gamma, constant = 1)
  out <- capture.output(print(robust_boxcox(x, standardize = FALSE)))
  expect_true(any(grepl("^Transformed: +1 \\(1 left unchanged\\)$", out)))
  expect_true(any(grepl("^Standardized: +no$", out)))
})

test_that("robust_boxcox and robust_yeojohnson check their arguments", {
  for (f in list(robust_boxcox, robust_yeojohnson)) {
    expect_error(f(), "Missing 'x' argument")
    expect_error(f(test_data), "only numeric columns")
    expect_error(f("a"), "numeric matrix or data frame")
    expect_error(f(positive, quantile = 1), "'quantile'")
    expect_error(f(positive, quantile = -0.5), "'quantile'")
    expect_error(f(positive, quantile = c(0.9, 0.99)), "'quantile'")
    expect_error(f(positive, nbsteps = 0), "'nbsteps'")
    expect_error(f(positive, nbsteps = 1.5), "'nbsteps'")
    expect_error(f(positive, standardize = NA), "'standardize'")
  }
})

test_that("the transformations agree with cellWise", {
  skip_on_cran()
  skip_if_not_installed("cellWise")
  set.seed(1)
  x <- cbind(lognormal = stats::rlnorm(200), gamma = stats::rgamma(200, 2),
             normal = stats::rnorm(200), negskew = -stats::rlnorm(200))
  x[1:5, 1] <- x[1:5, 1] * 50
  compare <- function(mine, ref, x) {
    expect_equal(unname(vapply(mine$fits, `[[`, character(1), "type")), unname(ref$ttypes))
    expect_equal(unname(vapply(mine$fits, `[[`, numeric(1), "lambda")), unname(ref$lambdahats),
                 tolerance = 0.25)
    y <- apply_transformation(x, mine)
    for (j in seq_len(ncol(x))) expect_gt(stats::cor(y[, j], ref$Y[, j]), 0.995)
  }
  transfo <- function(x, type) {
    cellWise::transfo(x, type = type, robust = TRUE, standardize = TRUE,
                      checkPars = list(silent = TRUE))
  }
  compare(robust_boxcox(x[, 1:2]), transfo(x[, 1:2], "BC"), x[, 1:2])
  compare(robust_yeojohnson(x), transfo(x, "YJ"), x)
  compare(robust_transformation(x), transfo(x, "bestObj"), x)
})

test_that("the transformation handles edge cases", {
  x <- cbind(constant = rep(1, 20), short = c(1:4, rep(NA, 16)), positive = seq_len(20)^2)
  fit <- robust_transformation(x)
  expect_equal(unname(vapply(fit$fits, `[[`, character(1), "type"))[1:2], c("none", "none"))
  expect_equal(apply_transformation(x, fit)[, 1], x[, 1])
  # missing values are ignored by the fit and stay missing
  expect_equal(is.na(apply_transformation(x, fit)[, 2]), is.na(x[, 2]))
  # Box-Cox is undefined for non-positive new values
  bc <- robust_transformation(x[, 3, drop = FALSE], type = "BC")
  expect_true(is.na(apply_transformation(cbind(positive = c(-1, 4)), bc)[1, 1]))
  expect_warning(none <- robust_transformation(cbind(v = stats::rnorm(30)), type = "BC"),
                 "strictly positive")
  expect_equal(none$fits[[1]]$type, "none")
})

test_that("robust_bcyj is deprecated but still works", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(res <- robust_bcyj(test_data), "deprecated")
  expect_named(res, c("summary", "transformation"))
  expect_equal(res$summary$variable, c("lognormal", "gamma", "normal"))
  expect_equal(res$summary$method[3], "YJ")
  expect_warning(bc <- robust_bcyj(test_data, var = c("lognormal", "gamma"), type = "BC"),
                 "robust_boxcox")
  expect_equal(bc$transformation, robust_boxcox(positive)$transformation)
  expect_warning(yj <- robust_bcyj(test_data, var = c("gamma", "normal"), type = "YJ"),
                 "robust_yeojohnson")
  expect_equal(yj$transformation, robust_yeojohnson(test_data[c("gamma", "normal")])$transformation)
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_error(robust_bcyj(), "Missing 'data' argument.")
  expect_error(robust_bcyj(as.matrix(test_data[1:3])), "must be a data frame or tibble")
  expect_error(robust_bcyj(test_data, var = "nope"), "not present")
  expect_error(robust_bcyj(test_data, type = 1.5), "Invalid type of transformation")
})
