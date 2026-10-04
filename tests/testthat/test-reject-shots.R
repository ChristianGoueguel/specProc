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
  expect_equal(robust_z(rep(1, 5), rep("a", 5)), rep(0, 5))
})

test_that("reject_shots returns the raw criteria, the shot order and its settings", {
  d <- shots_data()
  s <- reject_shots(d, Sample)
  x <- as.matrix(d[-(1:2)]) # the spectral columns, not Sample and Location
  rows <- d$Sample == d$Sample[1]
  ref <- apply(x[rows, ], 2, stats::median)
  total <- rowSums(x[rows, ])
  expect_equal(s$.intensity[rows], unname(total / stats::median(total)))
  expect_equal(s$.correlation[rows], as.vector(stats::cor(t(x[rows, ]), ref)))
  expect_equal(s$.distance[rows],
               unname(sqrt(rowSums(sweep(x[rows, ], 2, ref)^2)) / sqrt(sum(ref^2))))
  expect_equal(s$.shot, stats::ave(seq_len(nrow(d)), d$Sample, FUN = seq_along))
  d$number <- rev(seq_len(nrow(d)))
  expect_equal(reject_shots(d, Sample, shot = number)$.shot, d$number)
  settings <- attr(s, "reject_shots")
  expect_equal(settings$sample, "Sample")
  expect_equal(settings$cutoff, 3.5)
  expect_equal(settings$scale, "floor")
  expect_error(reject_shots(d, Sample, shot = zz), "not found")
  expect_error(reject_shots(d, Sample, scale = "mad"), "should be one of")
})

test_that("the robust scale of a sample is floored at the pooled MAD", {
  v <- c(0, 0.01, -0.01, 0.02, -0.02, 10, 0, 1, -1, 2, -2, 0)
  g <- rep(c("a", "b"), each = 6)
  mad_of <- function(e) 1.4826 * stats::median(abs(e))
  dev <- unlist(lapply(split(v, g), function(x) x - stats::median(x)))
  pooled <- mad_of(dev)
  own_a <- mad_of(v[1:6] - stats::median(v[1:6]))
  own_b <- mad_of(v[7:12] - stats::median(v[7:12]))
  expect_equal(robust_z(v, g, "sample")[1:6], (v[1:6] - stats::median(v[1:6])) / own_a)
  expect_equal(robust_z(v, g, "pooled")[1:6], (v[1:6] - stats::median(v[1:6])) / pooled)
  expect_equal(robust_z(v, g, "floor")[1:6], (v[1:6] - stats::median(v[1:6])) / max(own_a, pooled))
  expect_equal(robust_z(v, g, "floor")[7:12], (v[7:12] - stats::median(v[7:12])) / max(own_b, pooled))
  # the outlier of the tight sample a is no longer extreme against the pooled scale
  expect_gt(robust_z(v, g, "sample")[6], robust_z(v, g, "floor")[6])
})

test_that("the correlation criterion uses Fisher's z, and clean shots are rarely rejected", {
  set.seed(30)
  wl <- seq(390, 400, length.out = 300)
  base <- 1000 * exp(-(wl - 393.4)^2 / 0.01) + 600 * exp(-(wl - 396.8)^2 / 0.01) + 50
  clean <- do.call(rbind, lapply(1:60, function(k) {
    noise <- exp(stats::rnorm(1, 0, 0.4))
    t(vapply(1:8, function(j) {
      base * exp(stats::rnorm(1, 0, 0.05)) + stats::rnorm(300, sd = noise * 2 * sqrt(base))
    }, numeric(300)))
  }))
  colnames(clean) <- format(wl, nsmall = 4)
  d <- data.frame(Sample = rep(1:60, each = 8), clean, check.names = FALSE)
  s <- reject_shots(d, Sample)
  r <- s$.correlation
  expect_equal(s$.correlation_z, -robust_z(atanh(r), d$Sample, "floor"))
  floor_rate <- mean(s$.rejected)
  sample_rate <- mean(reject_shots(d, Sample, scale = "sample")$.rejected)
  expect_lt(floor_rate, 0.01)
  expect_lt(floor_rate, sample_rate)
  # a weak plasma is still found
  d[5, -1] <- 0.5 * d[5, -1]
  expect_true(reject_shots(d, Sample)$.rejected[5])
})

test_that("forageShots holds the documented shots", {
  expect_equal(dim(forageShots), c(160, 844))
  expect_equal(as.vector(table(forageShots$Measurement)), rep(8, 20))
  expect_true(all(forageShots$Measurement %in% forageLIBS$Measurement))
  res <- reject_shots(forageShots, Measurement, shot = shot)
  expect_equal(length(unique(res$Measurement[res$.rejected])), 7)
  expect_equal(sum(apply(as.matrix(forageShots[-(1:5)]), 1, max) >= 65535), 132)
})
