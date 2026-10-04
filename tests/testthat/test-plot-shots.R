test_that("plot_shots draws the criteria of the shots", {
  res <- reject_shots(forageShots, Measurement, shot = shot)
  p <- plot_shots(res, type = "criteria")
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$caption, "Rejected: 8 of 160 shots")
  # the labels are placed by ggrepel; the other shots have empty labels
  texts <- function(p) Filter(function(l) inherits(l$geom, "GeomTextRepel"), p$layers)
  n_shown <- function(p) sum(texts(p)[[1]]$data$.label != "")
  expect_equal(n_shown(p), 8)
  expect_equal(n_shown(plot_shots(res, type = "criteria", label = 3)), 3)
  expect_length(texts(plot_shots(res, type = "criteria", label = 0)), 0)
  tbl <- plot_shots(res, type = "criteria", plot = FALSE)
  expect_equal(nrow(tbl), 160)
  expect_true(all(c(".intensity_z", ".correlation_z") %in% names(tbl)))
  # one criterion: z-scores along the samples
  one <- reject_shots(forageShots, Measurement, method = "intensity")
  expect_equal(plot_shots(one, type = "criteria")$labels$x, "Sample (in the order of the data)")
  expect_error(plot_shots(forageShots), "result of reject_shots")
  expect_error(plot_shots(res, type = "map"), "should be one of")
})

test_that("plot_shots draws the shots of a sample", {
  res <- reject_shots(forageShots, Measurement, shot = shot)
  counts <- tapply(res$.rejected, res$Measurement, sum)
  p <- plot_shots(res, type = "spectra")
  expect_equal(p$labels$title, paste("Shots of sample", names(counts)[which.max(counts)]))
  expect_s3_class(p$facet, "FacetWrap") # the two windows
  zoom <- plot_shots(res, type = "spectra", sample = 121306, wavelength = c(764, 772))
  built <- ggplot2::ggplot_build(zoom)$data[[1]]
  expect_true(all(built$x >= 764 & built$x <= 772))
  expect_s3_class(zoom$facet, "FacetNull")
  tbl <- plot_shots(res, type = "spectra", sample = "121306", plot = FALSE)
  expect_equal(nrow(tbl), 8)
  expect_equal(sum(tbl$.rejected), 1)
  expect_error(plot_shots(res, type = "spectra", sample = 1), "No shots of sample 1")
})

test_that("plot_shots draws the trends along the shots and the samples", {
  skip_if_not_installed("patchwork")
  res <- reject_shots(forageShots, Measurement, shot = shot)
  order <- plot_shots(res, type = "order")
  expect_s3_class(order, "patchwork")
  tbl <- plot_shots(res, type = "order", plot = FALSE)
  expect_equal(tbl$shot, 1:8)
  expect_equal(tbl$n, rep(20L, 8))
  expect_equal(tbl$intensity_median[1], stats::median(res$.intensity[res$.shot == 1]))
  expect_equal(tbl$rejected_pct[7], 100 * mean(res$.rejected[res$.shot == 7]))
  samples <- plot_shots(res, type = "samples", title = "Shots")
  expect_s3_class(samples, "patchwork")
  st <- plot_shots(res, type = "samples", plot = FALSE)
  expect_equal(nrow(st), 20)
  expect_equal(sum(st$n_rejected), 8)
  one <- res$Measurement == st$Measurement[1]
  expect_equal(st$rsd_all[1], 100 * stats::sd(res$.intensity[one]) / mean(res$.intensity[one]))
  expect_true(all(st$rsd_kept[st$n_rejected > 0] < st$rsd_all[st$n_rejected > 0]))
})

test_that("plot_shots maps the samples by the shot number", {
  skip_if_not_installed("patchwork")
  res <- reject_shots(forageShots, Measurement, shot = shot)
  p <- plot_shots(res, acquisition = Measurement)
  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 1) # two panels: intensity and shape
  intensity <- p[[1]]
  rows <- levels(intensity$data$.row)
  expect_equal(rev(rows), as.character(sort(unique(forageShots$Measurement))))
  # binned at 1, 2 and the cutoff; the rejected shots outlined
  expect_equal(levels(intensity$data$.fill)[c(4, 7)], c("-1 to 1", "> 3.5"))
  cell <- intensity$data[intensity$data$sample == "121138" & intensity$data$shot == 4, ]
  expect_equal(as.character(cell$.fill), "> 3.5")
  outlined <- Filter(function(l) inherits(l$geom, "GeomTile") && isTRUE(is.na(l$aes_params$fill)),
                     intensity$layers)[[1]]$data
  expect_equal(nrow(outlined), 8)
  # the samples with the most extreme shots first
  extreme <- plot_shots(res, arrange = "extreme")[[1]]
  top <- rev(levels(extreme$data$.row))[1]
  z <- tapply(apply(abs(cbind(res$.intensity_z, res$.correlation_z)), 1, max), res$Measurement, max)
  expect_equal(top, names(z)[which.max(z)])
  # the table, without a repeated sample column
  tbl <- plot_shots(res, acquisition = Measurement, plot = FALSE)
  expect_named(tbl, c("Measurement", "shot", "intensity_z", "correlation_z", "rejected"))
  expect_type(tbl$Measurement, "integer")
  expect_equal(nrow(tbl), 160)
})

test_that("plot_shots smooths the map along the acquisition only", {
  skip_if_not_installed("patchwork")
  res <- reject_shots(forageShots, Measurement, shot = shot)
  tbl <- plot_shots(res, acquisition = Measurement, smooth = 5, plot = FALSE)
  first <- tbl[tbl$shot == 1, ]
  first <- first[order(first$Measurement), ]
  expect_equal(first$intensity_smoothed,
               as.vector(stats::runmed(first$intensity_z, 5, endrule = "median")))
  p <- plot_shots(res, acquisition = Measurement, smooth = 5)
  expect_match(p$patches$annotation$caption, "running median")
  expect_error(plot_shots(res, smooth = 5), "needs `acquisition`")
  expect_error(plot_shots(res, acquisition = Measurement, smooth = 4), "odd")
  expect_error(plot_shots(res, acquisition = when), "not found")
})
