data(cereal, package = "chemometrics")
test_data <- cereal$X |> as.data.frame()
mat_data <- cereal$X

test_that("polyBaselineFit function works as expected", {
  expect_error(polyBaselineFit(), "Missing 'x' argument.")
  expect_error(polyBaselineFit(test_data, degree = "8"), "'degree' must be numeric.")
  expect_error(polyBaselineFit(test_data, tol = NULL), "'tol' must be numeric.")
  expect_error(polyBaselineFit(test_data, rep = NULL), "'rep' must be numeric.")
}
)



