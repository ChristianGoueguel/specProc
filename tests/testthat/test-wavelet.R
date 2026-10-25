test_that("the wavelet filters are orthonormal and the DWT is invertible", {
  set.seed(1)
  x <- matrix(stats::rnorm(2 * 64), 2)
  for (w in names(wavelet_filters)) {
    h <- wavelet_filters[[w]]
    expect_equal(sum(h), sqrt(2), tolerance = 1e-10)
    expect_equal(sum(h^2), 1, tolerance = 1e-10)
    d <- dwt_matrix(x, h, 3)
    expect_equal(idwt_matrix(d, h), x, tolerance = 1e-9)
    energy <- sum(d$approximation^2) + sum(vapply(d$details, function(m) sum(m^2), numeric(1)))
    expect_equal(energy, sum(x^2), tolerance = 1e-9)
  }
  # Haar approximation: block sums scaled by 2^(-level/2)
  d <- dwt_matrix(x, wavelet_filters$haar, 2)
  expect_equal(unname(d$approximation[1, ]), colSums(matrix(x[1, ], 4)) / 2)
})

test_that("wavelet_features returns approximation or all coefficients", {
  set.seed(2)
  x <- matrix(stats::rnorm(3 * 100), 3, dimnames = list(NULL, 400 + 1:100))
  a <- wavelet_features(x, "d4", level = 2)
  expect_equal(dim(a), c(3L, 25L))
  expect_equal(colnames(a)[1:2], c("A2_1", "A2_2"))
  all_coef <- wavelet_features(x, "d4", level = 2, coefficients = "all")
  expect_equal(ncol(all_coef), 100)
  expect_equal(colnames(all_coef)[26], "D2_1")
  expect_equal(colnames(all_coef)[51], "D1_1")
  # a length not divisible by 2^level is padded by reflection
  odd <- wavelet_features(x[, 1:99], "haar", level = 2)
  expect_equal(ncol(odd), 25)
  expect_equal(odd[, 25], (x[, 97] + x[, 98] + x[, 99] + x[, 98]) / 2)
  expect_equal(nrow(wavelet_features(x[1, ], "la8", 1)), 1)
  expect_s3_class(tibble::as_tibble(wavelet_features(as.data.frame(x), "d6", 1)), "tbl_df")
  expect_error(wavelet_features(x, "d8", level = 5), "at most 3")
  expect_error(wavelet_features(x, "morlet"))
  x[1, 1] <- NA
  expect_error(wavelet_features(x), "missing values")
})

test_that("wavelet_features gives the wavelength at the center of each coefficient", {
  set.seed(4)
  x <- matrix(stats::rnorm(2 * 100), 2, dimnames = list(NULL, 400 + 1:100))
  # Haar: the center of the block of 2^j channels of each coefficient
  a <- wavelet_features(x, "haar", level = 2)
  expect_equal(attr(a, "wavelength"), 400 + 4 * (0:24) + 2.5)
  all_coef <- wavelet_features(x, "haar", level = 2, coefficients = "all")
  expect_equal(attr(all_coef, "wavelength"),
               c(400 + 4 * (0:24) + 2.5, 400 + 4 * (0:24) + 2.5, 400 + 2 * (0:49) + 1.5))
  # past the end of the axis: the padding and the window wrapping around
  expect_equal(tail(attr(wavelet_features(x[, 1:97], "haar", 2), "wavelength"), 1), 498.5)
  expect_equal(attr(wavelet_features(x[1, ], "haar", 1), "wavelength")[1:2], c(401.5, 403.5))
  expect_null(attr(wavelet_features(unname(x), "haar", 2), "wavelength"))
  # a line is centered on its wavelength, whatever the wavelet
  wl <- 300 + 0.1 * (1:512)
  line <- matrix(stats::dnorm(1:512, 250, 6), 1, dimnames = list(NULL, wl))
  for (w in names(wavelet_filters)) {
    coef <- wavelet_features(line, w, level = 3)
    expect_equal(sum(attr(coef, "wavelength") * coef) / sum(coef), wl[250], tolerance = 1e-6)
  }
  # the details are at the center of energy of their window
  impulse <- matrix(0, 64, 256, dimnames = list(NULL, 1:256))
  impulse[cbind(1:64, 100 + 1:64)] <- 1
  d <- wavelet_features(impulse, "la8", level = 3, coefficients = "all")
  for (j in 1:3) {
    s <- startsWith(colnames(d), paste0("D", j, "_"))
    energy <- d[, s]^2
    center <- sum(sweep(energy, 2, attr(d, "wavelength")[s], "*")) / sum(energy)
    expect_equal(center, sum((100 + 1:64) * rowSums(energy)) / sum(energy), tolerance = 1e-10)
  }
})

test_that("step_wavelet replaces spectra by coefficients", {
  skip_if_not_installed("recipes")
  set.seed(3)
  x <- matrix(stats::rnorm(6 * 64), 6, dimnames = list(NULL, 500 + (1:64) / 10))
  d <- data.frame(y = 1:6, x, check.names = FALSE)
  rec <- recipes::recipe(y ~ ., data = d) |>
    step_wavelet(recipes::all_predictors(), wavelet = "haar", level = 3)
  out <- recipes::bake(recipes::prep(rec), new_data = NULL)
  expect_equal(names(out), c("y", paste0("wav_A3_", 1:8)))
  expect_equal(unname(as.matrix(out[-1])), unname(wavelet_features(x, "haar", 3)),
               ignore_attr = "wavelength")
  # the coefficients of largest variance in the training data
  top <- recipes::recipe(y ~ ., data = d) |>
    step_wavelet(recipes::all_predictors(), coefficients = "all", num_coef = 5, level = 2) |>
    recipes::prep()
  all_coef <- wavelet_features(x, "d4", 2, "all")
  v <- apply(all_coef, 2, stats::var)
  expect_setequal(recipes::tidy(top, 1)$terms, paste0("wav_", names(sort(v, decreasing = TRUE))[1:5]))
  kept_coef <- match(sub("^wav_", "", recipes::tidy(top, 1)$terms), colnames(all_coef))
  expect_equal(recipes::tidy(top, 1)$wavelength, attr(all_coef, "wavelength")[kept_coef])
  new <- recipes::bake(top, new_data = d[1:2, ])
  expect_equal(ncol(new), 6)
  kept <- recipes::recipe(y ~ ., data = d) |>
    step_wavelet(recipes::all_predictors(), level = 1, keep_original_cols = TRUE) |>
    recipes::prep() |> recipes::bake(new_data = NULL)
  expect_equal(ncol(kept), 1 + 64 + 32)
  expect_equal(generics::tunable(rec$steps[[1]])$name, c("level", "num_coef"))
  expect_true(all(is.na(recipes::tidy(rec, 1)$variance)))
  expect_true(all(is.na(recipes::tidy(rec, 1)$wavelength)))
  # column names that are not wavelengths
  unnamed <- recipes::recipe(y ~ ., data = data.frame(y = 1:6, as.data.frame(unname(x)))) |>
    step_wavelet(recipes::all_predictors(), level = 2) |>
    recipes::prep()
  expect_true(all(is.na(recipes::tidy(unnamed, 1)$wavelength)))
  expect_match(paste(utils::capture.output(print(rec), type = "message"), collapse = " "), "haar")
  skip_if_not_installed("dials")
  expect_s3_class(wavelet_level(), "quant_param")
})
