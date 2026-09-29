test_that("step_spectral_norm normalizes each spectrum", {
  skip_if_not_installed("recipes")
  set.seed(1)
  x <- abs(matrix(stats::rnorm(5 * 20), 5, dimnames = list(NULL, 300 + 1:20)))
  d <- data.frame(y = 1:5, x, check.names = FALSE)
  for (m in c("l1", "area", "l2", "max")) {
    out <- recipes::recipe(y ~ ., data = d) |>
      step_spectral_norm(recipes::all_predictors(), method = m) |>
      recipes::prep() |> recipes::bake(new_data = NULL)
    expect_equal(as.matrix(out[colnames(x)]), as.matrix(normalize(x, m)), ignore_attr = TRUE)
    expect_equal(out$y, d$y)
  }
  l2 <- recipes::recipe(y ~ ., data = d) |>
    step_spectral_norm(recipes::all_predictors(), method = "l2") |>
    recipes::prep()
  expect_equal(unname(rowSums(as.matrix(recipes::bake(l2, new_data = d[1:2, ])[colnames(x)])^2)), c(1, 1))
  expect_equal(unique(recipes::tidy(l2, 1)$method), "l2")
  expect_match(paste(utils::capture.output(print(l2), type = "message"), collapse = " "), "l2")
  expect_error(recipes::recipe(y ~ ., data = d) |>
                 step_spectral_norm(recipes::all_predictors(), method = "snv"))
})

test_that("the method of step_spectral_norm can be tuned", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("dials")
  skip_if_not_installed("tune")
  d <- data.frame(y = 1:5, matrix(1:50, 5, dimnames = list(NULL, 300 + 1:10)), check.names = FALSE)
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_spectral_norm(recipes::all_predictors(), method = tune::tune())
  params <- tune::extract_parameter_set_dials(rec)
  expect_equal(params$name, "method")
  expect_s3_class(spectral_norm_method(), "qual_param")
  expect_equal(spectral_norm_method()$values, c("l1", "area", "l2", "max"))
  expect_match(paste(utils::capture.output(print(rec), type = "message"), collapse = " "), "tuned")
})
