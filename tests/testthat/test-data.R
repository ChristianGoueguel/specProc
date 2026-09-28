test_that("soilLIBS has the documented structure", {
  data(soilLIBS, package = "specProc", envir = environment())
  expect_s3_class(soilLIBS, "tbl_df")
  expect_equal(dim(soilLIBS), c(400L, 7160L))
  expect_equal(names(soilLIBS)[1:8], c("Sample", "Location", "Clay", "Sand", "Silt", "Texture", "Structure", "Type"))
  expect_equal(length(unique(soilLIBS$Sample)), 50)
  expect_true(all(table(soilLIBS$Sample) == 8))
  wl <- as.numeric(names(soilLIBS)[-(1:8)])
  expect_false(anyNA(wl))
  expect_equal(range(wl), c(199.3771616, 822.1849), tolerance = 1e-6)
  expect_true(all(vapply(soilLIBS[-(1:8)], is.integer, logical(1))))
  # particle-size fractions sum to 100%
  expect_true(all(abs(soilLIBS$Clay + soilLIBS$Sand + soilLIBS$Silt - 100) < 0.5))
})

test_that("forageLIBS has the documented structure", {
  data(forageLIBS, package = "specProc", envir = environment())
  expect_s3_class(forageLIBS, "tbl_df")
  expect_equal(dim(forageLIBS), c(368L, 7166L))
  expect_equal(names(forageLIBS)[1:14], c("Measurement", "Sample", "Ca", "Cl", "Cu", "Fe", "Mg",
                                          "Mn", "Mo", "P", "K", "Na", "S", "Zn"))
  expect_equal(length(unique(forageLIBS$Sample)), 365)
  expect_false(anyNA(as.numeric(names(forageLIBS)[-(1:14)])))
})
