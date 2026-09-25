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
