test_that("specLIBS has the documented structure", {
  data(specLIBS, package = "specProc", envir = environment())
  expect_s3_class(specLIBS, "tbl_df")
  expect_equal(dim(specLIBS), c(400L, 7160L))
  expect_equal(names(specLIBS)[1:8], c("Sample", "Location", "Clay", "Sand", "Silt", "Texture", "Structure", "Type"))
  expect_equal(length(unique(specLIBS$Sample)), 50)
  expect_true(all(table(specLIBS$Sample) == 8))
  wl <- as.numeric(names(specLIBS)[-(1:8)])
  expect_false(anyNA(wl))
  expect_equal(range(wl), c(199.3771616, 822.1849), tolerance = 1e-6)
  expect_true(all(vapply(specLIBS[-(1:8)], is.integer, logical(1))))
  # particle-size fractions sum to 100%
  expect_true(all(abs(specLIBS$Clay + specLIBS$Sand + specLIBS$Silt - 100) < 0.5))
})
