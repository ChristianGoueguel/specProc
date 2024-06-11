data(cereal, package = "chemometrics")
test_data <- cereal$X |> as.data.frame()
mat_data <- cereal$X

test_that("lsp function works as expected", {
  expect_error(lsp(), "Missing 'x' argument.")
  expect_error(lsp(test_data, degree = "8"), "'degree' must be a single numeric value.")
  expect_error(lsp(test_data, tol = NULL), "'tol must be a single numeric value.")
  expect_error(lsp(test_data, max.iter = NULL), "'max.iter' must be a single numeric value.")
}
)



