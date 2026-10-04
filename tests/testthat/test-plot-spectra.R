test_that("plot_spectra draws one line per spectrum", {
  spec <- make_spectra(n = 3, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  p <- plot_spectra(df)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(built$data[[1]]$group)), 3)
  df$conc <- 1:3
  df$id <- c("a", "b", "c")
  expect_s3_class(plot_spectra(df, id = id, colvar = conc), "ggplot")
  expect_s3_class(plot_spectra(df, id = "id", colvar = "conc"), "ggplot")
  expect_error(plot_spectra(df), "wavelengths")
  expect_error(plot_spectra(df, id = zz), "does not exist")
})

test_that("plot_spectra offsets successive spectra", {
  spec <- make_spectra(n = 3, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  base <- ggplot2::ggplot_build(plot_spectra(df))$data[[1]]
  shifted <- ggplot2::ggplot_build(plot_spectra(df, offset = c(0.5, 10)))$data[[1]]
  third <- base$group == 3
  expect_equal(shifted$y[third], base$y[third] + 2 * 10)
  expect_equal(shifted$x[third], base$x[third] + 2 * 0.5)
  # a single number is a vertical offset, and the first spectrum stays in place
  vertical <- ggplot2::ggplot_build(plot_spectra(df, offset = 10))$data[[1]]
  expect_equal(vertical$x, base$x)
  expect_equal(vertical$y[base$group == 1], base$y[base$group == 1])
  expect_error(plot_spectra(df, offset = "a"), "offset")
  expect_error(plot_spectra(df, offset = 1:3), "offset")
})

test_that("plot_spectra draws one panel per group, with the offsets restarting in each", {
  spec <- make_spectra(n = 5, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  df$site <- c("a", "b", "a", "b", "a")
  p <- plot_spectra(df, panel = site, offset = 10)
  built <- ggplot2::ggplot_build(p)
  layout <- built$layout$layout
  expect_equal(nrow(layout), 2)
  expect_equal(max(layout$COL), 1) # vertical: one column
  wide <- ggplot2::ggplot_build(plot_spectra(df, panel = "site", layout = "horizontal"))$layout$layout
  expect_equal(max(wide$ROW), 1) # horizontal: one row
  # the third spectrum is the second of panel "a": shifted once, not twice
  base <- ggplot2::ggplot_build(plot_spectra(df[, 1:50]))$data[[1]]
  shifted <- built$data[[1]]
  expect_equal(shifted$y[shifted$group == 3], base$y[base$group == 3] + 10)
  expect_equal(shifted$y[shifted$group == 2], base$y[base$group == 2])
  expect_error(plot_spectra(df, panel = zz), "does not exist")
  expect_error(plot_spectra(df, panel = site, layout = "diagonal"), "should be one of")
})

test_that("plot_spectra draws grid lines on request", {
  spec <- make_spectra(n = 2, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  grid_of <- function(p) ggplot2::calc_element("panel.grid.major", ggplot2::complete_theme(p$theme))
  expect_match(class(grid_of(plot_spectra(df)))[1], "element_blank")
  expect_match(class(grid_of(plot_spectra(df, grid = TRUE)))[1], "element_line")
  expect_error(plot_spectra(df, grid = "yes"), "grid")
})

test_that("plot_spectra fills the area under the spectra with color_as = 'fill'", {
  spec <- make_spectra(n = 3, p = 50)
  df <- as.data.frame(spec$x, check.names = FALSE)
  df$conc <- 1:3
  p <- plot_spectra(df, colvar = conc, color_as = "fill", offset = 10)
  expect_s3_class(p$layers[[1]]$geom, "GeomRibbon")
  expect_equal(p$scales$get_scales("fill")$name, "conc")
  built <- ggplot2::ggplot_build(p)$data[[1]]
  base <- ggplot2::ggplot_build(plot_spectra(df[, 1:50]))$data[[1]]
  # stacked upwards: the top spectrum is drawn first, each filled down to its baseline
  top <- built$group == 1
  expect_equal(built$ymax[top], base$y[base$group == 3] + 20)
  expect_equal(unique(built$ymin[top]), 20)
  expect_equal(unique(built$alpha), 1)
  expect_equal(length(unique(built$fill)), 3)
  # overlaid spectra are translucent; the colors of id fill the areas without colvar
  overlaid <- ggplot2::ggplot_build(plot_spectra(df[, 1:50], color_as = "fill"))$data[[1]]
  expect_true(all(overlaid$alpha < 1))
  df$id <- c("x", "y", "z")
  expect_s3_class(plot_spectra(df[, -51], id = id, color_as = "fill"), "ggplot")
  expect_error(plot_spectra(df, colvar = conc, color_as = "area"), "should be one of")
})

spectra_df <- function(n = 3, p = 50) {
  as.data.frame(make_spectra(n = n, p = p)$x, check.names = FALSE)
}

test_that("plot_spectra shows a legend on request, with its title", {
  df <- spectra_df()
  df$conc <- 1:3
  expect_equal(plot_spectra(df, colvar = conc)$theme$legend.position, "none")
  p <- plot_spectra(df, colvar = conc, legend = "top", legend_title = "Conc. (%)")
  expect_equal(p$theme$legend.position, "top")
  expect_equal(p$scales$get_scales("colour")$name, "Conc. (%)")
  expect_equal(plot_spectra(df, colvar = conc)$scales$get_scales("colour")$name, "conc")
  expect_equal(plot_spectra(df, colvar = conc, legend = "inside")$theme$legend.position, "inside")
  expect_error(plot_spectra(df, colvar = conc, legend = "left"), "should be one of")
})

test_that("plot_spectra titles the axes and writes the intensities in plain notation", {
  df <- spectra_df()
  p <- plot_spectra(df, xlab = "Raman shift (cm-1)", ylab = "Counts", title = "Spectra")
  expect_equal(p$labels$x, "Raman shift (cm-1)")
  expect_equal(p$labels$y, "Counts")
  expect_equal(p$labels$title, "Spectra")
  expect_equal(plot_spectra(df)$labels$x, "Wavelength (nm)")
  expect_equal(plot_spectra(df)$labels$y, "Intensity (arb. units)")
  expect_equal(plain_numbers(c(0, 50000, 1e5, NA)), c("0", "50,000", "100,000", NA))
  built <- ggplot2::ggplot_build(plot_spectra(df * 1000))
  expect_false(any(grepl("e+", built$layout$panel_params[[1]]$y$get_labels(), fixed = TRUE)))
})

test_that("plot_spectra marks and labels emission lines, merging close ones", {
  df <- spectra_df() # 390 to 400 nm
  lines <- data.frame(species = c("Ca II", "Ca II", "Ca I", "K I"),
                      wavelength = c(393.37, 393.40, 396.85, 766.49))
  p <- plot_spectra(df, lines = lines)
  marker <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)[[1]]
  expect_equal(marker$data$wavelength, c(393.37, 393.40, 396.85))
  axis <- p$scales$get_scales("x")$secondary.axis
  expect_equal(axis$labels, c("Ca II 393.37, 393.40", "Ca I 396.85"))
  expect_equal(axis$breaks, c(mean(c(393.37, 393.40)), 396.85))
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  expect_warning(plot_spectra(df, lines = lines[4, ]), "None of 'lines'")
  expect_error(plot_spectra(df, lines = data.frame(w = 1)), "species")
})

test_that("plot_spectra colors with a viridis palette or the given colors", {
  df <- spectra_df()
  df$conc <- 1:3
  colour_of <- function(p, group) {
    d <- ggplot2::ggplot_build(p)$data[[1]]
    toupper(substr(unique(d$colour[d$group == group]), 1, 7))
  }
  p <- plot_spectra(df, colvar = conc)
  expect_equal(colour_of(p, 1), "#440154") # the dark end of viridis
  grey <- plot_spectra(df, colvar = conc, palette = c("#FFFFFF", "#000000"))
  expect_equal(colour_of(grey, 1), "#FFFFFF")
  expect_equal(colour_of(grey, 3), "#000000")
  # fewer colors than levels: interpolated
  df$id <- c("x", "y", "z")
  two <- ggplot2::ggplot_build(plot_spectra(df[, -51], id = id, palette = c("red", "blue")))$data[[1]]
  expect_equal(length(unique(two$colour)), 3)
  expect_s3_class(plot_spectra(df[, -51], id = id, palette = "magma"), "ggplot")
  expect_error(plot_spectra(df, colvar = conc, palette = "rainbow"), "palette")
  expect_error(plot_spectra(df, colvar = conc, palette = c("red", "nocolor")), "palette")
})

test_that("plot_spectra sizes the text and lines, and limits the wavelength range", {
  df <- spectra_df()
  p <- plot_spectra(df, base_size = 8, linewidth = 0.3)
  expect_equal(p$theme$text$size, 8)
  expect_equal(p$layers[[1]]$aes_params$linewidth, 0.3)
  expect_error(plot_spectra(df, base_size = 0), "base_size")
  expect_error(plot_spectra(df, linewidth = -1), "linewidth")
  built <- ggplot2::ggplot_build(plot_spectra(df, xlim = c(392, 395)))$data[[1]]
  expect_true(all(built$x >= 392 & built$x <= 395))
  expect_error(plot_spectra(df, xlim = c(395, 392)), "xlim")
  expect_error(plot_spectra(df, xlim = c(500, 600)), "within 'xlim'")
})

test_that("plot_spectra frees the intensity axes and tags the panels on request", {
  df <- spectra_df()
  df$site <- c("a", "b", "a")
  p <- plot_spectra(df, panel = site, scales = "free_y", panel_tags = TRUE)
  expect_true(p$facet$params$free$y)
  expect_equal(levels(ggplot2::ggplot_build(p)$layout$layout$.panel), c("(a) a", "(b) b"))
  expect_false(plot_spectra(df, panel = site)$facet$params$free$y)
  expect_error(plot_spectra(df, panel = site, scales = "free_x"), "should be one of")
})

test_that("plot_spectra labels each spectrum at its right end", {
  df <- spectra_df()
  wl <- as.numeric(names(df))
  df$conc <- c(1.234, 2, 3)
  df$id <- c("x", "y", "z")
  ends_of <- function(p) Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]$data
  ends <- ends_of(plot_spectra(df, id = id, colvar = conc, label_spectra = TRUE, offset = 10))
  expect_equal(ends$.label, c("x", "y", "z"))
  expect_equal(ends$wavelength, rep(max(wl), 3))
  expect_equal(ends$intensity, unname(unlist(df[, which.max(wl)])) + c(0, 10, 20))
  expect_equal(ends_of(plot_spectra(df[, -52], colvar = conc, label_spectra = TRUE))$.label,
               c("1.23", "2", "3"))
  expect_equal(ends_of(plot_spectra(df[, 1:50], label_spectra = TRUE))$.label, c("1", "2", "3"))
})

test_that("plot_spectra summarizes the spectra of each group", {
  spec <- make_spectra(n = 4, p = 50)
  meta <- data.frame(.row = 1:4, conc = 1:4, .panel = factor(c("a", "a", "b", "b")))
  m <- summarize_spectra(spec$x, meta, NULL, "conc", "mean")
  expect_equal(m$meta$conc, c(1.5, 3.5))
  sd_a <- apply(spec$x[1:2, ], 2, stats::sd)
  expect_equal(m$center[1, ], colMeans(spec$x[1:2, ]))
  expect_equal(m$upper[1, ] - m$center[1, ], sd_a)
  med <- summarize_spectra(spec$x, meta, NULL, "conc", "median")
  q <- apply(spec$x[3:4, ], 2, stats::quantile, probs = c(0.25, 0.5, 0.75), names = FALSE)
  expect_equal(med$center[2, ], q[2, ])
  expect_equal(med$lower[2, ], q[1, ])
  # groups by id within panels; one spectrum per group has no band
  one <- summarize_spectra(spec$x, data.frame(.row = 1:4, id = c("p", "q", "p", "q")), "id", NULL, "mean")
  expect_equal(nrow(one$center), 2)
  single <- summarize_spectra(spec$x[1, , drop = FALSE], data.frame(.row = 1), NULL, NULL, "mean")
  expect_true(all(is.na(single$lower)))

  df <- as.data.frame(spec$x, check.names = FALSE)
  df$site <- c("a", "a", "b", "b")
  built <- ggplot2::ggplot_build(plot_spectra(df, panel = site, summary = "mean"))
  band <- built$data[[1]]
  curve <- built$data[[2]]
  expect_equal(length(unique(curve$group)), 2)
  first <- curve[curve$PANEL == 1, ]
  expect_equal(first$y[order(first$x)], unname(colMeans(spec$x[1:2, ])))
  expect_equal(band$ymax[band$PANEL == 1][order(first$x)], unname(colMeans(spec$x[1:2, ]) + sd_a))
  # filled summaries have no band
  filled <- plot_spectra(df, panel = site, summary = "median", color_as = "fill")
  expect_equal(sum(vapply(filled$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))), 1)
  expect_error(plot_spectra(df, summary = "max"), "should be one of")
})

test_that("plot_spectra rasterizes the spectra within a vector plot", {
  skip_if_not_installed("ragg")
  df <- spectra_df()
  p <- plot_spectra(df, rasterize = TRUE)
  g <- ggplot2::ggplotGrob(p)
  panel <- g$grobs[[which(g$layout$name == "panel")]]
  raster <- Filter(function(x) inherits(x, "specproc_raster"), panel$children)
  expect_length(raster, 1)
  # the spectra are drawn in the image
  grDevices::pdf(NULL)
  grid::pushViewport(grid::viewport(width = grid::unit(3, "in"), height = grid::unit(2, "in")))
  image <- grid::makeContext(raster[[1]])
  grDevices::dev.off()
  expect_s3_class(image, "rastergrob")
  alpha <- bitwAnd(bitwShiftR(unclass(image$raster), 24L), 255L)
  expect_gt(sum(alpha > 0), 1000)
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, 4, 3)
  print(p)
  grDevices::dev.off()
  expect_true(length(grepRaw("/Subtype /Image", readBin(f, "raw", file.size(f)))) > 0)
  expect_error(plot_spectra(df, rasterize = -1), "rasterize")
})
