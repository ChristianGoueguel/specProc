test_that("old function names still work with a deprecation warning", {
  x <- seq(-2, 2, length.out = 20)
  expect_warning(l <- lorentzian(x, 0, 0, 1, 1), "use 'lorentzian_profile\\(\\)'")
  expect_equal(l, lorentzian_profile(x, 0, 0, 1, 1))
  expect_warning(v <- pseudo_voigt(x, 0, 0, 1, 1, 1), "deprecated")
  expect_equal(v, pseudo_voigt_profile(x, 0, 0, 1, 1, 1))
  expect_warning(s <- summaryStats(data.frame(a = 1:5)), "summary_stats")
  expect_equal(s, summary_stats(data.frame(a = 1:5)))
  expect_warning(p <- pareto(matrix(1:6, 3)), "pareto_scale")
  expect_equal(p, pareto_scale(matrix(1:6, 3)))
})

test_that("deprecated aliases forward non-standard evaluation", {
  df <- data.frame(`1` = 1:3, `2` = 2:4, grp = c("a", "b", "c"), check.names = FALSE)
  expect_warning(p <- plotSpec(df, id = grp), "plot_spectra")
  expect_s3_class(p, "ggplot")
})

test_that("gaussian no longer masks stats::gaussian for glm users", {
  expect_false("gaussian" %in% getNamespaceExports("specProc"))
  expect_s3_class(stats::gaussian(), "family")
})
