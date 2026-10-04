line_ratio_data <- function() {
  wl <- seq(390, 400, by = 0.1)
  line <- exp(-(wl - 393.4)^2 / 0.02)
  scale <- c(1, 2, 5)
  x <- outer(scale, 100 * line + 10)
  colnames(x) <- wl
  data.frame(Sample = c("a", "b", "c"), x, check.names = FALSE)
}

test_that("step_line_ratio normalizes each spectrum to the reference line", {
  skip_if_not_installed("recipes")
  d <- line_ratio_data()
  rec <- recipes::recipe(~ ., data = d) |>
    step_line_ratio(recipes::all_numeric(), reference = 393.4, window = 0.3) |>
    recipes::prep()
  out <- recipes::bake(rec, new_data = NULL)
  # the three spectra differ by a factor only: identical once normalized
  expect_equal(out[[5]], rep(out[[5]][1], 3))
  expect_equal(as.character(out$Sample), d$Sample)
  wl <- as.numeric(names(d)[-1])
  win <- abs(wl - 393.4) <= 0.3
  area <- sum(diff(wl[win]) * (utils::head(unlist(d[1, -1])[win], -1) + utils::tail(unlist(d[1, -1])[win], -1)) / 2)
  expect_equal(unname(unlist(out[1, -1])), unname(unlist(d[1, -1])) / area)
  # height, baseline and two reference lines
  h <- recipes::recipe(~ ., data = d) |>
    step_line_ratio(recipes::all_numeric(), reference = 393.4, method = "height") |>
    recipes::prep() |> recipes::bake(new_data = d)
  expect_equal(h$`393.4`, rep(1, 3))
  b <- recipes::recipe(~ ., data = d) |>
    step_line_ratio(recipes::all_numeric(), reference = 393.4, window = 1, method = "height", baseline = TRUE) |>
    recipes::prep() |> recipes::bake(new_data = d)
  expect_equal(b$`393.4`, rep(110 / 100, 3), tolerance = 1e-6)   # peak over the baseline
  two <- recipes::recipe(~ ., data = d) |>
    step_line_ratio(recipes::all_numeric(), reference = c(393.4, 393.4), method = "height") |>
    recipes::prep() |> recipes::bake(new_data = d)
  expect_equal(two$`393.4`, rep(0.5, 3))
})

test_that("step_line_ratio methods and errors", {
  skip_if_not_installed("recipes")
  d <- line_ratio_data()
  rec <- recipes::recipe(~ ., data = d) |>
    step_line_ratio(recipes::all_numeric(), reference = 393.4)
  expect_match(paste(utils::capture.output(print(rec), type = "message"), collapse = " "), "393.4")
  expect_equal(recipes::tidy(rec, 1)$reference, 393.4)
  expect_equal(nrow(generics::tunable(rec$steps[[1]])), 0)
  expect_equal(generics::required_pkgs(rec$steps[[1]]), "specProc")
  expect_error(recipes::recipe(~ ., data = d) |> step_line_ratio(recipes::all_numeric()), "reference")
  expect_error(
    recipes::recipe(~ ., data = d) |>
      step_line_ratio(recipes::all_numeric(), reference = 500) |> recipes::prep(),
    "No channel"
  )
  d2 <- d
  names(d2)[2] <- "foo"
  expect_error(
    recipes::recipe(~ ., data = d2) |>
      step_line_ratio(recipes::all_numeric(), reference = 393.4) |> recipes::prep(),
    "named by their wavelengths"
  )
  zero <- d
  zero[2, -1] <- 0
  prepped <- recipes::prep(rec)
  expect_warning(out <- recipes::bake(prepped, new_data = zero), "non-positive")
  expect_true(all(is.na(unlist(out[2, -1]))))
})

test_that("step_reject_shots removes rejected shots from the training data only", {
  skip_if_not_installed("recipes")
  d <- shots_data()
  rec <- recipes::recipe(~ ., data = d) |>
    recipes::update_role(Sample, Location, new_role = "id") |>
    step_reject_shots(recipes::all_numeric_predictors(), sample = Sample)
  expect_match(paste(utils::capture.output(print(rec), type = "message"), collapse = " "),
               "Shot rejection")
  prepped <- recipes::prep(rec)
  train <- recipes::bake(prepped, new_data = NULL)
  expect_equal(nrow(train), 14)
  expect_equal(sort(setdiff(1:16, match(paste(train$Sample, train$Location), paste(d$Sample, d$Location)))),
               c(2, 12))
  expect_equal(nrow(recipes::bake(prepped, new_data = d)), 16)   # skip = TRUE
  td <- recipes::tidy(prepped, 1)
  expect_equal(unique(td$sample), "Sample")
  expect_equal(unique(td$method), "intensity, correlation")
  expect_equal(unique(recipes::tidy(rec, 1)$sample), "Sample")
  # skip = FALSE filters new data too
  strict <- recipes::recipe(~ ., data = d) |>
    recipes::update_role(Sample, Location, new_role = "id") |>
    step_reject_shots(recipes::all_numeric_predictors(), sample = Sample, method = "intensity",
                      skip = FALSE) |>
    recipes::prep()
  expect_equal(nrow(recipes::bake(strict, new_data = d)), 15)
  expect_error(recipes::recipe(~ ., data = d) |> step_reject_shots(recipes::all_numeric()), "sample")
  expect_equal(nrow(generics::tunable(rec$steps[[1]])), 0)
})

test_that("step_reject_shots passes the scale of the z-scores", {
  skip_if_not_installed("recipes")
  d <- shots_data()
  rec <- recipes::recipe(~ ., data = d) |>
    step_reject_shots(recipes::all_numeric(), sample = Sample, scale = "sample")
  expect_equal(recipes::tidy(rec, 1)$scale, "sample")
  prepped <- recipes::prep(rec)
  expect_equal(nrow(recipes::bake(prepped, new_data = NULL)),
               sum(!reject_shots(d, Sample, scale = "sample")$.rejected))
  expect_error(recipes::recipe(~ ., data = d) |>
                 step_reject_shots(recipes::all_numeric(), sample = Sample, scale = "x"),
               "should be one of")
})
