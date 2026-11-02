test_that("the MODWT is exact and preserves the energy", {
  set.seed(1)
  x <- matrix(stats::rnorm(3 * 200), 3)
  for (wv in names(wavelet_filters)) {
    h <- wavelet_filters[[wv]]
    dec <- modwt_matrix(x, h, 4)
    expect_equal(imodwt_matrix(dec, h), x, tolerance = 1e-9)
    energy <- Reduce(`+`, lapply(dec$details, function(w) rowSums(w^2))) + rowSums(dec$smooth^2)
    expect_equal(energy, rowSums(x^2), tolerance = 1e-9)
  }
  # Haar, level 1: half the difference of neighboring channels
  d <- modwt_matrix(x, wavelet_filters$haar, 1)
  expect_equal(d$details[[1]][, 1:199], (x[, 1:199] - x[, 2:200]) / 2)
  # white noise: the details of level j have a standard deviation of sigma / 2^(j/2)
  z <- matrix(stats::rnorm(20 * 4000, sd = 3), 20)
  dz <- modwt_matrix(z, wavelet_filters$la8, 3)
  est <- vapply(dz$details, function(w) stats::median(abs(w)) / stats::qnorm(0.75), numeric(1))
  expect_equal(est, 3 / 2^((1:3) / 2), tolerance = 0.03)
})

test_that("thresholding the MODWT is translation invariant", {
  set.seed(2)
  s <- c(rep(0, 100), 10 * exp(-((1:20) - 10)^2 / 2), rep(0, 136)) + stats::rnorm(256, sd = 0.5)
  h <- wavelet_filters$la8
  denoise <- function(v) {
    dec <- modwt_matrix(matrix(v, 1), h, 4)
    dec$details <- lapply(dec$details, shrink, lambda = 1, rule = "hard")
    imodwt_matrix(dec, h)[1, ]
  }
  shift <- function(v, k) c(utils::tail(v, k), utils::head(v, -k))
  expect_equal(denoise(shift(s, 37)), shift(denoise(s), 37))
})

test_that("the SURE threshold is universal for pure noise and smaller for dense signals", {
  set.seed(3)
  expect_equal(sure_threshold(stats::rnorm(5000)), sqrt(2 * log(5000)))
  dense <- sure_threshold(c(stats::rnorm(4000), stats::rnorm(1000, sd = 4)))
  expect_lt(dense, 2)
  expect_gt(dense, 0.5)
})

test_that("wavelet_denoise removes white noise and keeps the lines", {
  set.seed(4)
  n <- 2000
  t <- seq_len(n)
  centers <- seq(100, 1900, by = 150)
  truth <- 50 + 0.01 * t + rowSums(sapply(centers, function(cc) 200 * exp(-(t - cc)^2 / (2 * 3.5^2))))
  x <- matrix(truth, 5, n, byrow = TRUE) + matrix(stats::rnorm(5 * n, sd = 10), 5)
  rmse <- function(y) sqrt(mean((y - rep(truth, each = 5))^2))
  for (lv in c(2, 4)) {
    y <- wavelet_denoise(x, level = lv)
    expect_lt(rmse(y), 0.75 * rmse(x))
    expect_equal(mean(y[, centers]), mean(truth[centers]), tolerance = 0.03)
  }
  universal <- wavelet_denoise(x, level = 4, threshold = "universal")
  expect_lt(rmse(universal), 0.5 * rmse(x))
  # soft thresholding shrinks the lines more than hard thresholding
  soft <- wavelet_denoise(x, level = 4, threshold = "universal", rule = "soft")
  expect_lt(mean(soft[, centers]), mean(universal[, centers]))
})

test_that("wavelet_denoise keeps the shape of its input", {
  data(forageShots, package = "specProc", envir = environment())
  spectra <- forageShots[1:3, -(1:5)]
  out <- wavelet_denoise(spectra)
  expect_s3_class(out, "tbl_df")
  expect_named(out, names(spectra))
  mat <- as.matrix(spectra)
  expect_equal(wavelet_denoise(mat), as.matrix(out), ignore_attr = TRUE)
  expect_equal(dimnames(wavelet_denoise(mat)), dimnames(mat))
  one <- wavelet_denoise(unlist(spectra[1, ]))
  expect_named(one, names(spectra))
  expect_equal(unname(one), unname(as.matrix(out)[1, ]))
  # a spectrum without noise is unchanged
  expect_equal(wavelet_denoise(rep(5, 100)), rep(5, 100))
})

test_that("wavelet_denoise works segment by segment", {
  set.seed(5)
  wl <- c(seq(200, 230, by = 0.1), seq(400, 420, by = 0.1))
  x <- matrix(stats::rnorm(2 * length(wl), 100, 5), 2, dimnames = list(NULL, wl))
  first <- wl < 300
  both <- wavelet_denoise(x)
  expect_equal(both[, first], wavelet_denoise(x[, first]))
  expect_equal(both[, !first], wavelet_denoise(x[, !first]))
  expect_false(isTRUE(all.equal(wavelet_denoise(x, segments = FALSE), both)))
  # a segment too short for `level` is denoised with fewer levels, one
  # shorter than the filter is returned unchanged
  short <- x[, 1:60]
  expect_equal(wavelet_denoise(short, level = 4), wavelet_denoise(short, level = 3))
  tiny <- matrix(stats::rnorm(14, 100, 5), 2, dimnames = list(NULL, 1:7))
  expect_equal(wavelet_denoise(tiny), tiny)
})

test_that("wavelet_denoise leaves the segments with missing values unchanged", {
  set.seed(6)
  x <- matrix(stats::rnorm(3 * 100, 100, 5), 3)
  x[2, 10] <- NA
  expect_warning(out <- wavelet_denoise(x), "1 spectra")
  expect_equal(out[2, ], x[2, ])
  expect_equal(out[-2, ], wavelet_denoise(x[-2, ]))
})

test_that("wavelet_denoise checks its arguments", {
  x <- matrix(stats::rnorm(200), 2)
  expect_error(wavelet_denoise(x, wavelet = "sym9"), "should be one of")
  expect_error(wavelet_denoise(x, threshold = "minimax"), "should be one of")
  expect_error(wavelet_denoise(x, rule = "garrote"), "should be one of")
  expect_error(wavelet_denoise(x, level = 0), "'level'")
  expect_error(wavelet_denoise(x, segments = NA), "'segments'")
  expect_error(wavelet_denoise(data.frame(a = "x")), "numeric")
})

test_that("step_wavelet_denoise denoises the selected columns", {
  skip_if_not_installed("recipes")
  data(forageShots, package = "specProc", envir = environment())
  df <- forageShots[1:16, -c(1:4)]
  cols <- names(df)[-1]
  rec <- recipes::recipe(K ~ ., data = df) |>
    step_wavelet_denoise(recipes::all_predictors(), level = 3, threshold = "universal")
  expect_equal(recipes::tidy(rec, number = 1)$level, 3)
  trained <- recipes::prep(rec)
  baked <- recipes::bake(trained, new_data = NULL)
  expect_equal(as.matrix(baked[cols]),
               wavelet_denoise(as.matrix(df[cols]), level = 3, threshold = "universal"),
               ignore_attr = TRUE)
  expect_equal(baked$K, df$K)
  td <- recipes::tidy(trained, number = 1)
  expect_named(td, c("terms", "wavelet", "level", "threshold", "rule", "id"))
  expect_equal(td$terms, cols)
  expect_equal(generics::tunable(trained$steps[[1]])$name, "level")
  expect_equal(generics::required_pkgs(trained$steps[[1]]), "specProc")
  expect_true(any(grepl("Wavelet denoising \\(la8, level 3\\) of", cli::cli_fmt(print(trained)))))
})
