data(cereal, package = "chemometrics")
test_data <- cereal$X |> as.data.frame()
mat_data <- cereal$X

test_that("baselineRemoval function works as expected", {
  expect_error(baselineRemoval(), "Missing 'data' argument.")
  expect_error(baselineRemoval(mat_data), "'data' must be a data frame or tibble.")
  expect_error(baselineRemoval(test_data, degree = "8"), "'degree' must be numeric.")
  expect_error(baselineRemoval(test_data, tol = NULL), "'tol' must be numeric.")
  expect_error(baselineRemoval(test_data, rep = NULL), "'rep' must be numeric.")
}
)



