set.seed(1)
test_data <- tibble::tibble(
  lognormal = stats::rlnorm(100),
  gamma = stats::rgamma(100, shape = 2),
  normal = stats::rnorm(100),
  label = sample(letters, 100, replace = TRUE)
)

test_that("robust_bcyj transforms the numeric variables", {
  res <- robust_bcyj(test_data)
  expect_type(res, "list")
  expect_named(res, c("summary", "transformation"))
  expect_equal(res$summary$variable, c("lognormal", "gamma", "normal"))
  expect_equal(dim(res$transformation), c(100L, 3L))
  # the log-normal variable is made (nearly) symmetric
  expect_lt(abs(moments::skewness(res$transformation$lognormal)), abs(moments::skewness(test_data$lognormal)))
  expect_equal(res$summary$method[3], "YJ")
  sub <- robust_bcyj(test_data, var = c("gamma", "normal"), type = "YJ")
  expect_equal(sub$summary$variable, c("gamma", "normal"))
  expect_true(all(sub$summary$method == "YJ"))
})

test_that("robust_bcyj validates its inputs", {
  expect_error(robust_bcyj(), "Missing 'data' argument.")
  expect_error(robust_bcyj(as.matrix(test_data[1:3])), "Input 'data' must be a data frame or tibble.")
  expect_error(robust_bcyj(test_data, var = 1), "The 'var' argument must be a character vector or NULL.")
  expect_error(robust_bcyj(test_data, var = "nope"), "not present")
  expect_error(robust_bcyj(test_data, type = 1.5), "Invalid type of transformation. Available method types are: BC, YJ and bestObj.")
  expect_error(robust_bcyj(test_data, quantile = -0.5), "'quantile' must be a numeric value between 0 and 1")
  expect_error(robust_bcyj(test_data, nbsteps = 0), "'nbsteps' must be a positive integer")
})

test_that("the transformations agree with cellWise", {
  skip_on_cran()
  skip_if_not_installed("cellWise")
  set.seed(1)
  x <- cbind(lognormal = stats::rlnorm(200), gamma = stats::rgamma(200, 2),
             normal = stats::rnorm(200), negskew = -stats::rlnorm(200))
  x[1:5, 1] <- x[1:5, 1] * 50
  mine <- robust_transformation(x)
  ref <- cellWise::transfo(x, type = "bestObj", robust = TRUE, standardize = TRUE,
                           checkPars = list(silent = TRUE))
  expect_equal(unname(vapply(mine$fits, `[[`, character(1), "type")), unname(ref$ttypes))
  expect_equal(unname(vapply(mine$fits, `[[`, numeric(1), "lambda")), unname(ref$lambdahats),
               tolerance = 0.25)
  y <- apply_transformation(x, mine)
  for (j in seq_len(ncol(x))) expect_gt(stats::cor(y[, j], ref$Y[, j]), 0.995)
})

test_that("the transformation handles edge cases and new data", {
  x <- cbind(constant = rep(1, 20), short = c(1:4, rep(NA, 16)), positive = seq_len(20)^2)
  fit <- robust_transformation(x)
  expect_equal(unname(vapply(fit$fits, `[[`, character(1), "type"))[1:2], c("none", "none"))
  expect_equal(apply_transformation(x, fit)[, 1], x[, 1])
  # Box-Cox is undefined for non-positive new values
  bc <- robust_transformation(x[, 3, drop = FALSE], type = "BC")
  expect_true(is.na(apply_transformation(cbind(positive = c(-1, 4)), bc)[1, 1]))
  # YJ is fitted when BC is not possible
  expect_equal(robust_transformation(cbind(v = stats::rnorm(30)), type = "BC")$fits[[1]]$type, "none")
})
