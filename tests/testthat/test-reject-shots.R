test_that("reject_shots flags weak and misshapen shots", {
  d <- shots_data()
  s <- reject_shots(d, Sample)
  expect_s3_class(s, "tbl_df")
  expect_equal(which(s$.rejected), c(2, 12))
  expect_match(s$.reason[2], "intensity")
  expect_match(s$.reason[12], "correlation")
  expect_true(all(is.na(s$.reason[-c(2, 12)])))
  expect_true(all(c(".intensity_z", ".correlation_z") %in% names(s)))
  expect_false(".distance_z" %in% names(s))
  # drop = TRUE returns the kept shots only, ready for average()
  kept <- reject_shots(d, "Sample", drop = TRUE)
  expect_equal(nrow(kept), 14)
  expect_named(kept, names(d))
  expect_equal(nrow(average(kept[-2], Sample)), 2)
})

test_that("reject_shots criteria, cutoff and wavelength range", {
  d <- shots_data()
  s <- reject_shots(d, Sample, method = "distance")
  expect_true(all(c(2, 12) %in% which(s$.rejected)))
  expect_equal(which(reject_shots(d, Sample, method = "intensity")$.rejected), 2)
  expect_equal(sum(reject_shots(d, Sample, cutoff = 1e6)$.rejected), 0)
  # the other line ratio is invisible around the first line only
  s2 <- reject_shots(d, Sample, method = "correlation", wavelength = c(390, 395))
  expect_false(s2$.rejected[12])
})

test_that("reject_shots handles small samples and bad inputs", {
  d <- shots_data()[c(1, 2, 9:16), ]            # sample a has 2 shots only
  s <- reject_shots(d, Sample)
  expect_false(any(s$.rejected[1:2]))
  expect_true(all(is.na(s$.intensity_z[1:2])))
  expect_error(reject_shots(d), "sample")
  expect_error(reject_shots(d, Batch), "not found")
  expect_error(reject_shots(as.matrix(d[-1]), Sample), "data frame")
  expect_error(reject_shots(d, Sample, method = "foo"))
  expect_error(reject_shots(d, Sample, wavelength = c(800, 900)), "at least 2")
  d$`390`[3] <- NA
  expect_error(reject_shots(d, Sample), "missing values")
  expect_equal(robust_z(rep(1, 5)), rep(0, 5))
})
