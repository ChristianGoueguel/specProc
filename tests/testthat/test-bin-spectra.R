test_that("bin_spectra averages adjacent channels", {
  v <- stats::setNames(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 400 + 0:9 * 0.1)
  b <- bin_spectra(v, width = 3)
  expect_equal(unname(b), c(2, 5, 8, 10))                    # the last bin is smaller
  expect_equal(as.numeric(names(b)), c(400.1, 400.4, 400.7, 400.9))
  expect_identical(names(b)[4], names(v)[10])                # a lone channel keeps its name
  expect_identical(bin_spectra(v, width = 1), v)
  m <- rbind(a = v, b = 2 * v)
  bm <- bin_spectra(m, width = 2)
  expect_equal(dim(bm), c(2L, 5L))
  expect_equal(rownames(bm), c("a", "b"))
  expect_equal(bm["b", ], 2 * bm["a", ])
  df <- as.data.frame(m, optional = TRUE)
  names(df) <- names(v)
  expect_s3_class(bin_spectra(df), "tbl_df")
  # names that are not wavelengths: the name of the first channel of each bin
  expect_named(bin_spectra(c(a = 1, b = 2, c = 3, d = 4), width = 2), c("a", "c"))
  expect_null(colnames(bin_spectra(unname(m), width = 2)))
})

test_that("bin_spectra bins each detector segment on its own", {
  v <- stats::setNames(c(1:4, 101:104), c(seq(400, 400.3, 0.1), seq(500, 500.3, 0.1)))
  b <- bin_spectra(v, width = 3)
  expect_equal(unname(b), c(2, 4, 102, 104))
  expect_equal(as.numeric(names(b)), c(400.1, 400.3, 500.1, 500.3))
  expect_equal(unname(bin_spectra(v, width = 3, segments = FALSE)), c(2, 69, 103.5))
  expect_error(bin_spectra(v, width = 0), "'width'")
  expect_error(bin_spectra(v, width = 1.5), "'width'")
})

test_that("step_bin_spectra replaces the spectra by the binned channels", {
  skip_if_not_installed("recipes")
  set.seed(3)
  x <- matrix(rnorm(4 * 50), 4, 50, dimnames = list(NULL, 300 + (1:50) * 0.2))
  d <- data.frame(id = letters[1:4], y = 1:4, x, check.names = FALSE)
  rec <- recipes::recipe(y ~ ., data = d) |>
    recipes::update_role(id, new_role = "id") |>
    step_bin_spectra(recipes::all_predictors(), width = 4)
  prepped <- recipes::prep(rec)
  out <- recipes::bake(prepped, new_data = NULL)
  binned <- bin_spectra(x, width = 4)
  expect_equal(names(out), c("id", "y", colnames(binned)))
  expect_equal(unname(as.matrix(out[colnames(binned)])), unname(binned))
  info <- summary(prepped)
  expect_true(all(info$role[info$variable %in% colnames(binned)] == "predictor"))
  expect_equal(nrow(recipes::bake(prepped, new_data = d[1:2, ])), 2)
  expect_equal(recipes::tidy(prepped, 1)$width[1], 4)
  expect_equal(generics::tunable(rec$steps[[1]])$name, "width")
  expect_match(paste(utils::capture.output(print(rec), type = "message"), collapse = " "),
               "Binning")
})
