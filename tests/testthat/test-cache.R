# Restores the options and removes a temporary persistent cache and the
# session caches.
drop_cache <- function(old, dir) {
  options(old)
  unlink(dir, recursive = TRUE)
  for (s in c("nist", "starkb", "exomol")) {
    e <- session_cache(s)
    rm(list = ls(e, all.names = TRUE), envir = e)
  }
}

test_that("the cache directory follows the option", {
  old <- options(specProc.cache = NULL)
  on.exit(options(old), add = TRUE)
  expect_null(cache_dir())
  options(specProc.cache = FALSE)
  expect_null(cache_dir())
  options(specProc.cache = TRUE)
  expect_equal(cache_dir(), tools::R_user_dir("specProc", which = "cache"))
  options(specProc.cache = "~/x")
  expect_equal(cache_dir(), path.expand("~/x"))
  options(specProc.cache = 1)
  expect_error(cache_dir(), "specProc.cache")
  options(specProc.cache = NULL, specProc.cache_max_age = -1)
  expect_error(cache_max_age(), "cache_max_age")
  options(specProc.cache_max_age = NULL)
  expect_equal(cache_max_age(), 180)
})

test_that("cached_query keeps the data for the session and on disk", {
  dir <- tempfile("specProc-cache")
  old <- options(specProc.cache = dir, specProc.cache_max_age = 180)
  on.exit(drop_cache(old, dir), add = TRUE)
  calls <- 0
  download <- function() {
    calls <<- calls + 1
    data.frame(value = calls)
  }
  expect_equal(cached_query("nist", "lines Xx I 1 2", "Xx I", download)$value, 1)
  expect_equal(cached_query("nist", "lines Xx I 1 2", "Xx I", download)$value, 1)
  expect_equal(calls, 1)
  file <- file.path(dir, "nist", "lines_Xx_I_1_2.rds")
  expect_true(file.exists(file))
  # a new session reads the disk
  rm(list = ls(nist_cache), envir = nist_cache)
  expect_equal(cached_query("nist", "lines Xx I 1 2", "Xx I", download)$value, 1)
  expect_equal(calls, 1)
  # an outdated entry is downloaded again
  rm(list = ls(nist_cache), envir = nist_cache)
  Sys.setFileTime(file, Sys.time() - 200 * 86400)
  expect_equal(cached_query("nist", "lines Xx I 1 2", "Xx I", download)$value, 2)
  # when the download fails, the outdated copy is used with a warning
  rm(list = ls(nist_cache), envir = nist_cache)
  Sys.setFileTime(file, Sys.time() - 200 * 86400)
  failing <- function() stop("Could not download NIST data.", call. = FALSE)
  expect_warning(value <- cached_query("nist", "lines Xx I 1 2", "Xx I", failing), "cached on")
  expect_equal(value$value, 2)
  # without a copy, the error is raised
  expect_error(cached_query("nist", "lines Yy I 1 2", "Yy I", failing), "Could not download")
})

test_that("without the option, nothing is written on disk", {
  dir <- tempfile("specProc-cache")
  old <- options(specProc.cache = dir, specProc.cache_max_age = 180)
  on.exit(drop_cache(old, dir), add = TRUE)
  options(specProc.cache = NULL)
  cached_query("nist", "ie Xx", "Xx", function() data.frame(value = 1))
  expect_false(dir.exists(dir))
  expect_message(listing <- download_cache(), "not enabled")
  expect_equal(nrow(listing), 0)
})

test_that("the NIST and STARK-B functions read the persistent cache", {
  dir <- tempfile("specProc-cache")
  old <- options(specProc.cache = dir, specProc.cache_max_age = 180)
  on.exit(drop_cache(old, dir), add = TRUE)
  lines <- tibble::tibble(species = "Ca I", wavelength = 422.673, Aki = 2.18e8, fik = 1.75,
                          accuracy = "AA", Ei = 0, Ek = 2.93, gi = 1, gk = 3,
                          lower = "4s2 1S 0", upper = "4s4p 1P* 1", intensity = "")
  dir.create(file.path(dir, "nist"), recursive = TRUE)
  saveRDS(lines, file.path(dir, "nist", "lines_Ca_I_420_425.rds"))
  expect_equal(nist_lines("Ca I", c(420, 425)), lines)
  stark <- tibble::tibble(species = "Ca II", wavelength = 393.4, upper = "4p 2P*", lower = "4s 2S",
                          perturber = "electron", temperature = 10000, density = 1e17,
                          width = 0.02, shift = 0.001, source = "test")
  dir.create(file.path(dir, "starkb"), recursive = TRUE)
  saveRDS(stark, file.path(dir, "starkb", "Ca_II.rds"))
  expect_equal(starkb_lines("Ca II")$width, 0.02)
})

test_that("molecular_spectrum keeps the ExoMol files in the persistent cache", {
  dir <- tempfile("specProc-cache")
  old <- options(specProc.cache = dir, specProc.cache_max_age = 180)
  on.exit(drop_cache(old, dir), add = TRUE)
  dir.create(file.path(dir, "exomol"), recursive = TRUE)
  con <- bzfile(file.path(dir, "exomol", "1X-1Y__test.states.bz2"), "w")
  writeLines(c("1 0.0 2 0.5", "2 25000.0 4 1.5"), con)
  close(con)
  con <- bzfile(file.path(dir, "exomol", "1X-1Y__test.trans.bz2"), "w")
  writeLines("2 1 1.0E+07", con)
  close(con)
  sticks <- molecular_spectrum("XY", wavelength = c(395, 402), isotopologue = "1X-1Y",
                               dataset = "test", lines = TRUE)
  expect_equal(nrow(sticks), 1)
})

test_that("download_cache lists and clear_download_cache removes the entries", {
  dir <- tempfile("specProc-cache")
  old <- options(specProc.cache = dir, specProc.cache_max_age = 180)
  on.exit(drop_cache(old, dir), add = TRUE)
  cached_query("nist", "ie Xx", "Xx", function() data.frame(value = 1))
  cached_query("starkb", "Xx II", "Xx II", function() data.frame(value = 2))
  listing <- download_cache()
  expect_equal(attr(listing, "dir"), dir)
  expect_equal(listing$source, c("nist", "starkb"))
  expect_equal(listing$file, c("ie_Xx.rds", "Xx_II.rds"))
  expect_true(all(listing$size > 0 & listing$age >= 0))
  # only the entries older than a number of days
  expect_length(clear_download_cache(older_than = 1), 0)
  removed <- clear_download_cache("starkb")
  expect_equal(basename(removed), "Xx_II.rds")
  expect_null(starkb_cache[["Xx II"]])
  expect_equal(download_cache()$source, "nist")
  clear_download_cache()
  expect_equal(nrow(download_cache()), 0)
  expect_error(clear_download_cache("other"), "should be one of")
})
