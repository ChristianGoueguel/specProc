test_that("savitzky_golay matches prospectr inside and is exact on polynomials", {
  skip_if_not_installed("prospectr")
  set.seed(1)
  x <- matrix(rnorm(3 * 60), 3, 60, dimnames = list(NULL, 400 + (1:60) * 0.1))
  for (d in 0:2) {
    ours <- savitzky_golay(x, window = 9, order = 3, derivative = d)
    ref <- prospectr::savitzkyGolay(x, m = d, p = 3, w = 9)
    expect_equal(unname(ours[, 5:56]), unname(ref))
  }
  z <- 1:40
  cubic <- matrix(0.5 + 0.2 * z - 0.03 * z^2 + 0.001 * z^3, 1, dimnames = list(NULL, z))
  expect_equal(savitzky_golay(cubic, 7, 3)[1, ], cubic[1, ])                       # edges included
  expect_equal(unname(savitzky_golay(cubic, 7, 3, 1)[1, ]), 0.2 - 0.06 * z + 0.003 * z^2)
  expect_equal(unname(savitzky_golay(cubic, 7, 3, 2)[1, ]), -0.06 + 0.006 * z)
})

test_that("savitzky_golay keeps shapes and filters segments separately", {
  v <- stats::setNames(c(1:30, 101:130), c(seq(400, 402.9, 0.1), seq(500, 502.9, 0.1)))
  d1 <- savitzky_golay(v, 5, 2, derivative = 1)
  expect_named(d1, names(v))
  expect_equal(unname(d1), rep(1, 60))                       # the jump is not differentiated
  expect_gt(max(savitzky_golay(v, 5, 2, 1, segments = FALSE)), 1)
  df <- as.data.frame(rbind(v, v), optional = TRUE)
  names(df) <- names(v)
  out <- savitzky_golay(df, 5, 2)
  expect_s3_class(out, "tbl_df")
  expect_equal(dim(out), c(2L, 60L))
  short <- stats::setNames(c(1:30, 1:3), c(seq(400, 402.9, 0.1), 500, 500.1, 500.2))
  expect_warning(s <- savitzky_golay(short, 5, 2), "shorter than the window")
  expect_true(all(is.na(s[31:33])))
})

test_that("savitzky_golay checks its parameters", {
  x <- matrix(1:20, 1)
  expect_error(savitzky_golay(x, window = 4), "odd")
  expect_error(savitzky_golay(x, window = 5, order = 5), "smaller than")
  expect_error(savitzky_golay(x, window = 5, order = 1, derivative = 2), "exceed")
  expect_error(savitzky_golay(x, window = 5, derivative = 3), "0, 1 or 2")
})

test_that("step_savgol filters the spectra in a recipe", {
  skip_if_not_installed("recipes")
  set.seed(2)
  x <- matrix(rnorm(4 * 50), 4, 50, dimnames = list(NULL, 300 + (1:50) * 0.2))
  d <- data.frame(y = 1:4, x, check.names = FALSE)
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_savgol(recipes::all_predictors(), window = 7, derivative = 1)
  out <- recipes::bake(recipes::prep(rec), new_data = NULL)
  expect_equal(unname(as.matrix(out[names(d)[-1]])), unname(savitzky_golay(x, 7, 2, 1)))
  expect_equal(out$y, d$y)
  # an even window from a tuning grid is rounded up
  even <- recipes::recipe(y ~ ., data = d) |> step_savgol(recipes::all_predictors(), window = 6) |>
    recipes::prep()
  expect_equal(even$steps[[1]]$window, 7)
  expect_equal(recipes::tidy(rec, 1)$derivative[1], 1)
  expect_equal(generics::tunable(rec$steps[[1]])$name, c("window", "order", "derivative"))
  expect_match(paste(utils::capture.output(print(rec), type = "message"), collapse = " "),
               "first derivative")
  skip_if_not_installed("dials")
  expect_s3_class(savgol_derivative(), "quant_param")
})
