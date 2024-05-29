data(glass, package = "chemometrics")
test_data <- glass |> tibble::as_tibble()
oxide <- names(test_data)

test_that("robustBCYJ function works as expected", {
  expect_error(robustBCYJ(), "Missing 'data' argument.")
  expect_error(robustBCYJ(glass), "Input 'data' must be a data frame or tibble.")
  expect_type(robustBCYJ(test_data), "list")
  expect_named(robustBCYJ(test_data), c("summary", "transformation"), ignore.order = TRUE, ignore.case = TRUE)
  expect_no_error(robustBCYJ(test_data, var = oxide))
  expect_error(robustBCYJ(test_data, var = 1), "The 'var' argument must be a character vector or NULL.")
  expect_error(robustBCYJ(test_data, type = 1.5), "Invalid type of transformation. Available method types are: BC, YJ and bestObj.")
  expect_error(robustBCYJ(test_data, quant = -0.5), "'quant' must be a numeric value between 0 and 1")
  expect_error(robustBCYJ(test_data, nbsteps = 0), "'nbsteps' must be a positive integer")
  expect_error(robustBCYJ(test_data, nbsteps = -2), "'nbsteps' must be a positive integer")
  }
  )
