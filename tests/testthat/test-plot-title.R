test_that("split_title splits long titles at a natural break", {
  expect_equal(split_title("ROBPCA outlier map (3 components)"),
               list(title = "ROBPCA outlier map (3 components)", subtitle = NULL))
  expect_equal(split_title("Linear calibration: R2 = 0.6005, LOD = 1.45, LOQ = 4.39"),
               list(title = "Linear calibration", subtitle = "R2 = 0.6005, LOD = 1.45, LOQ = 4.39"))
  # the parenthesis closing the one at the break is dropped
  expect_equal(split_title("Contributions to Q of sample 12 (3 components), relative to the regular samples"),
               list(title = "Contributions to Q of sample 12",
                    subtitle = "3 components, relative to the regular samples"))
  # without separators, at the last space that fits
  expect_equal(split_title("Sixty-character title without any separators at all here ok")$title,
               "Sixty-character title without any separators")
  # the split part goes above an existing subtitle
  expect_equal(split_title("CF-LIBS Saha-Boltzmann plots: T = 9016 +/- 218 K", "Note")$subtitle,
               "T = 9016 +/- 218 K\nNote")
  expect_null(split_title(NULL)$title)
  expect_equal(plotly_title("Linear calibration: R2 = 0.6005, LOD = 1.45, LOQ = 4.39")$text,
               "<b>Linear calibration</b><br><sup>R2 = 0.6005, LOD = 1.45, LOQ = 4.39</sup>")
})

test_that("plots have a bold title, split when long", {
  set.seed(1)
  standards <- data.frame(concentration = rep(c(0, 0.5, 1, 2, 4, 8), each = 3))
  standards$intensity <- 50 + 1000 * standards$concentration + stats::rnorm(18, sd = 20)
  p <- plot_calibration(calibration_curve(standards, intensity, concentration))
  expect_equal(p$labels$title, "Linear calibration")
  expect_match(p$labels$subtitle, "^R2 = ")
  expect_equal(p$theme$plot.title$face, "bold")
})
