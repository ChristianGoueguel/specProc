test_that("forageLIBS has the documented structure", {
  data(forageLIBS, package = "specProc", envir = environment())
  expect_s3_class(forageLIBS, "tbl_df")
  expect_equal(dim(forageLIBS), c(368L, 7166L))
  expect_equal(names(forageLIBS)[1:14], c("Measurement", "Sample", "Ca", "Cl", "Cu", "Fe", "Mg",
                                          "Mn", "Mo", "P", "K", "Na", "S", "Zn"))
  expect_equal(length(unique(forageLIBS$Sample)), 365)
  expect_false(anyNA(as.numeric(names(forageLIBS)[-(1:14)])))
})
