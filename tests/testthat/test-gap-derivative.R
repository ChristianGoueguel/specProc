test_that("gap_derivative matches prospectr inside and is exact on polynomials", {
  skip_if_not_installed("prospectr")
  set.seed(1)
  x <- matrix(rnorm(3 * 60), 3, 60, dimnames = list(NULL, 400 + (1:60) * 0.1))
  for (d in 1:2) for (g in c(1, 5)) for (s in c(1, 3)) {
    ours <- gap_derivative(x, derivative = d, gap = g, segment = s)
    ref <- prospectr::gapDer(x, m = d, w = g, s = s)
    h <- (ncol(x) - ncol(ref)) / 2
    expect_equal(unname(ours[, (h + 1):(ncol(x) - h)]), unname(ref))
  }
  z <- 1:40
  line <- matrix(2 + 0.5 * z, 1, dimnames = list(NULL, z))
  parabola <- matrix(1 + 0.3 * z - 0.02 * z^2, 1, dimnames = list(NULL, z))
  expect_equal(unname(gap_derivative(line, 1, 3, 3)[1, ]), rep(0.5, 40))       # edges included
  expect_equal(unname(gap_derivative(line, 2, 3, 3)[1, ]), rep(0, 40))
  expect_equal(unname(gap_derivative(parabola, 2, 3, 3)[1, ]), rep(-0.04, 40))
  # inside, the first derivative of the parabola is exact at each channel
  expect_equal(unname(gap_derivative(parabola, 1, 3, 3)[1, 5:36]), 0.3 - 0.04 * z[5:36])
})

test_that("gap_derivative keeps shapes and filters segments separately", {
  v <- stats::setNames(c(1:30, 101:130), c(seq(400, 402.9, 0.1), seq(500, 502.9, 0.1)))
  d1 <- gap_derivative(v, gap = 3, segment = 3)
  expect_named(d1, names(v))
  expect_equal(unname(d1), rep(1, 60))                       # the jump is not differentiated
  expect_gt(max(gap_derivative(v, gap = 3, segment = 3, segments = FALSE)), 1)
  df <- as.data.frame(rbind(v, v), optional = TRUE)
  names(df) <- names(v)
  out <- gap_derivative(df)
  expect_s3_class(out, "tbl_df")
  expect_equal(dim(out), c(2L, 60L))
  # the filter spans 2 * 3 + 5 = 11 channels: a segment of 10 is too short
  short <- stats::setNames(c(1:30, 1:10), c(seq(400, 402.9, 0.1), seq(500, 500.9, 0.1)))
  expect_warning(s <- gap_derivative(short), "shorter than the filter")
  expect_true(all(is.na(s[31:40])))
  expect_false(anyNA(s[1:30]))
})

test_that("gap_derivative checks its parameters", {
  x <- matrix(1:40, 1)
  expect_error(gap_derivative(x, derivative = 0), "1 or 2")
  expect_error(gap_derivative(x, derivative = 3), "1 or 2")
  expect_error(gap_derivative(x, gap = 4), "'gap' must be an odd")
  expect_error(gap_derivative(x, segment = 0), "'segment' must be an odd")
  expect_error(gap_derivative(x, segments = NA), "logical")
})

test_that("step_gap_derivative differentiates the spectra in a recipe", {
  skip_if_not_installed("recipes")
  set.seed(2)
  x <- matrix(rnorm(4 * 50), 4, 50, dimnames = list(NULL, 300 + (1:50) * 0.2))
  d <- data.frame(y = 1:4, x, check.names = FALSE)
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_gap_derivative(recipes::all_predictors(), derivative = 2, gap = 3, segment = 1)
  out <- recipes::bake(recipes::prep(rec), new_data = NULL)
  expect_equal(unname(as.matrix(out[names(d)[-1]])), unname(gap_derivative(x, 2, 3, 1)))
  expect_equal(out$y, d$y)
  # even values from a tuning grid are rounded up
  even <- recipes::recipe(y ~ ., data = d) |>
    step_gap_derivative(recipes::all_predictors(), gap = 4, segment = 2) |>
    recipes::prep()
  expect_equal(c(even$steps[[1]]$gap, even$steps[[1]]$segment), c(5, 3))
  expect_equal(recipes::tidy(rec, 1)$derivative[1], 2)
  expect_equal(generics::tunable(rec$steps[[1]])$name, c("derivative", "gap", "segment"))
  expect_match(paste(utils::capture.output(print(rec), type = "message"), collapse = " "),
               "second derivative")
})
