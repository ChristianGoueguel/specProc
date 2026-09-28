# Made-up NIST lines of two species
fake_nist <- function(species, wavelength = c(200, 900), ...) {
  tab <- switch(
    species,
    "Xx I" = tibble::tibble(
      species = "Xx I", wavelength = c(400, 410, 420, 430), Aki = c(1e8, 1e7, 5e7, 1e6),
      fik = NA_real_, accuracy = c("B", "C", "C", "D"), Ei = 0, Ek = c(3, 3.2, 5, 6),
      gi = 1, gk = c(3, 5, 3, 7), lower = "a", upper = "b", intensity = ""
    ),
    "Xx II" = tibble::tibble(
      species = "Xx II", wavelength = c(395, 405), Aki = c(2e8, 1e8), fik = NA_real_,
      accuracy = "C", Ei = 0, Ek = c(3.1, 3.3), gi = 2, gk = c(4, 2),
      lower = "c", upper = "d", intensity = ""
    ),
    stop("The NIST database returned no data for ", species)
  )
  tab[tab$wavelength >= min(wavelength) & tab$wavelength <= max(wavelength), ]
}
kB <- 8.617333262e-5

test_that("rank_lines computes relative LTE intensities per species", {
  raw <- rbind(fake_nist("Xx I"), fake_nist("Xx II"))
  ranked <- rank_lines(raw, temperature = 10000, top = 10, min_relative = 0)
  expect_equal(nrow(ranked), 6)
  expect_true(!is.unsorted(ranked$wavelength))
  expect_equal(unique(ranked$element), "Xx")
  expect_equal(ranked$stage[ranked$species == "Xx II"], c(2L, 2L))
  neutral <- ranked[ranked$species == "Xx I", ]
  strength <- with(neutral, gk * Aki / wavelength * exp(-Ek / (kB * 10000)))
  expect_equal(neutral$relative_intensity, strength / max(strength))
  expect_equal(max(ranked$relative_intensity[ranked$species == "Xx II"]), 1)
})

test_that("rank_lines keeps the strongest lines and applies the thresholds", {
  raw <- fake_nist("Xx I")
  top2 <- rank_lines(raw, temperature = 10000, top = 2, min_relative = 0)
  all_lines <- rank_lines(raw, temperature = 10000, top = 10, min_relative = 0)
  expect_equal(nrow(top2), 2)
  expect_setequal(top2$wavelength,
                  all_lines$wavelength[order(all_lines$relative_intensity, decreasing = TRUE)][1:2])
  strong <- rank_lines(raw, temperature = 10000, top = 10, min_relative = 0.5)
  expect_true(all(strong$relative_intensity >= 0.5))
  expect_equal(nrow(rank_lines(raw, 10000, 10, 0, wavelength = c(405, 425))), 2)
  expect_equal(nrow(rank_lines(NULL, 10000, 10, 0)), 0)
  # hotter plasmas favour lines from higher levels
  cold <- rank_lines(raw, 5000, 10, 0)
  hot <- rank_lines(raw, 30000, 10, 0)
  expect_gt(hot$relative_intensity[hot$wavelength == 430], cold$relative_intensity[cold$wavelength == 430])
})

test_that("libs_lines fetches each species and skips species without lines", {
  local_mocked_bindings(nist_lines = function(species, wavelength, ...) fake_nist(species, wavelength))
  lines <- libs_lines(c("Xx I", "Xx II", "Yy I"), wavelength = c(390, 450), top = 3)
  expect_setequal(unique(lines$species), c("Xx I", "Xx II"))
  expect_equal(sum(lines$species == "Xx I"), 3)
  expect_error(libs_lines("Xx I", wavelength = c(400)), "length 2")
  expect_error(libs_lines("Xx I", temperature = -1), "temperature")
})

test_that("network errors are not mistaken for missing lines", {
  local_mocked_bindings(nist_lines = function(...) stop("Could not download NIST data"))
  expect_error(libs_lines("Xx I"), "Could not download")
})

test_that("plot_lines draws static and interactive overlays", {
  wl <- seq(390, 440, by = 0.05)
  spectrum <- stats::setNames(100 + 1000 * exp(-(wl - 400)^2 / 0.01), wl)
  lines <- rank_lines(rbind(fake_nist("Xx I"), fake_nist("Xx II")), 10000, 10, 0)
  p <- plot_lines(spectrum, lines, interactive = FALSE)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(ggplot2::layer_data(p, 2)), nrow(lines))
  skip_if_not_installed("plotly")
  pi <- plotly::plotly_build(plot_lines(spectrum, lines, shift = 0.1))
  names <- vapply(pi$x$data, function(t) t$name %||% "", character(1))
  expect_true(all(c("spectrum", "stage I", "stage II") %in% names))
  stage1 <- pi$x$data[[which(names == "stage I")]]
  expect_equal(sum(!is.na(stage1$x)) / 2, sum(lines$stage == 1))
  expect_true(any(abs(stage1$x - 400.1) < 1e-8, na.rm = TRUE))
  expect_error(plot_lines(unname(spectrum), lines), "wavelengths")
  expect_error(plot_lines(spectrum, data.frame(a = 1)), "libs_lines")
})

test_that("finder helpers build spectra, choices and the periodic table", {
  x <- matrix(1:12, 3, 4, dimnames = list(NULL, c("400", "401", "402", "403")))
  df <- data.frame(Sample = c("a", "a", "b"), x, check.names = FALSE)
  data <- finder_spectra(df)
  expect_equal(data$wavelength, 400:403)
  expect_equal(data$group, c("a", "a", "b"))
  expect_equal(unname(finder_spectrum(data, "mean")), colMeans(x), ignore_attr = TRUE)
  expect_equal(unname(finder_spectrum(data, "group:a")), colMeans(x[1:2, ]), ignore_attr = TRUE)
  expect_equal(unname(finder_spectrum(data, "row:3")), unname(x[3, ]))
  expect_true(all(c("mean", "group:a", "row:3") %in% finder_choices(data)))
  pt <- periodic_table()
  expect_equal(nrow(pt), 103)
  expect_false(any(duplicated(pt[c("row", "col")])))
  expect_equal(pt[pt$symbol == "Ca", c("row", "col")], data.frame(row = 4, col = 2), ignore_attr = TRUE)
  expect_error(finder_spectra(data.frame(a = "x")), "wavelengths")
})

test_that("the line finder server fetches, caches and ranks the selected species", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("bslib")
  calls <- character()
  fetch <- function(species, wavelength) {
    calls <<- c(calls, species)
    fake_nist(species, wavelength)
  }
  wl <- seq(390, 440, by = 0.5)
  x <- matrix(1, 2, length(wl), dimnames = list(NULL, wl))
  app <- line_finder_app(data.frame(Sample = c("a", "b"), x, check.names = FALSE), fetch = fetch)
  shiny::testServer(app, {
    session$setInputs(spectrum = "mean", stages = c("1", "2"), temperature = 10000, top = 2,
                      min_relative = 0, wl_range = c(390, 440), shift = 0, scale = TRUE,
                      elements = list("Xx"))
    expect_setequal(calls, c("Xx I", "Xx II"))
    expect_equal(nrow(lines()), 4)
    session$setInputs(top = 1)
    expect_equal(nrow(lines()), 2)
    expect_length(calls, 2)                      # no new download
    session$setInputs(stages = "2")
    expect_equal(unique(lines()$species), "Xx II")
    session$setInputs(elements = list("Yy"))     # species without lines
    expect_equal(nrow(lines()), 0)
  })
})

test_that("the line finder UI shows the periodic table and the plot only", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("bslib")
  pt <- periodic_table()
  expect_true(all(pt$category %in% names(finder_categories)))
  wl <- seq(390, 440, by = 0.5)
  html <- as.character(finder_ui(finder_spectra(matrix(1, 2, length(wl), dimnames = list(NULL, wl)))))
  expect_equal(lengths(regmatches(html, gregexpr('class="pt-el"', html))), nrow(pt))
  expect_match(html, 'id="plot"')
  expect_no_match(html, "lines_table")
})
