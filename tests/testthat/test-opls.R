test_x <- tibble::tibble(
  x1 = c(1, 2, 3, 4, 5),
  x2 = c(2, 4, 6, 8, 10)
)

test_y <- tibble::tibble(
  y1 = c(10, 20, 30, 40, 50)
)

test_that("opls validates its inputs", {
  expect_error(opls(NULL, test_y), "x-data must be provided")
  expect_error(opls(test_x, NULL), "y-data must be provided")
  expect_error(opls(list(1, 2, 3), test_y), "x-data must be of class data.frame, tbl_df, or tbl")
  expect_error(opls(test_x, list(1, 2, 3)), "y-data must be of class data.frame, tbl_df, or tbl")
})

test_that("opls wraps ropls and returns the model components", {
  skip_if_not_installed("ropls")
  d <- make_xy()
  res <- opls(as.data.frame(d$x), data.frame(y = d$y), ncomp.ortho = 2, permutation = 0)
  expect_named(res, c("x_scores", "x_loadings", "x_weights", "orthoScores", "orthoLoadings",
                      "orthoWeights", "y_weights", "y_scores", "summary", "model"))
  expect_equal(dim(res$x_scores), c(40L, 1L))
  expect_equal(dim(res$orthoScores), c(40L, 2L))
  # the OPLS-filtered data agree with the dependency-free implementation
  xc <- scale(d$x, scale = FALSE)
  filtered <- xc - as.matrix(res$orthoScores) %*% t(as.matrix(res$orthoLoadings))
  expect_equal(filtered, as.matrix(projected_osc(d$x, d$y, ncomp = 3)$correction),
               tolerance = 1e-8, ignore_attr = TRUE)
})
