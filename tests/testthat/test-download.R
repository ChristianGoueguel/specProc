# download.file() with libcurl on an HTTP error: two warnings, then an error
http_failure <- function(status) {
  function(url, file) {
    warning("downloaded length 0 != reported length 160")
    warning("cannot open URL '", url, "': HTTP status was '", status, "'")
    stop("cannot open URL '", url, "'")
  }
}

test_that("download_data tries again when the server is busy", {
  calls <- 0
  local_mocked_bindings(download_file = function(url, file) {
    calls <<- calls + 1
    if (calls == 1) http_failure("504 Gateway Timeout")(url, file)
    writeLines("data", file)
    0L
  })
  file <- tempfile()
  expect_equal(download_data("http://x", file, "STARK-B", "Ca II", 10, wait = 0), file)
  expect_equal(calls, 2)
  expect_equal(readLines(file), "data")
  unlink(file)
})

test_that("download_data reports the HTTP status", {
  calls <- 0
  local_mocked_bindings(download_file = function(url, file) {
    calls <<- calls + 1
    http_failure("504 Gateway Timeout")(url, file)
  })
  expect_error(download_data("http://x", tempfile(), "STARK-B", "Ca II", 10, wait = 0),
               "STARK-B data for Ca II (HTTP status 504 Gateway Timeout)", fixed = TRUE)
  expect_equal(calls, 3)
  # client errors are not tried again
  calls <- 0
  local_mocked_bindings(download_file = function(url, file) {
    calls <<- calls + 1
    http_failure("404 Not Found")(url, file)
  })
  expect_error(download_data("http://x", tempfile(), "NIST", "Ca II", 10, wait = 0),
               "(HTTP status 404 Not Found)", fixed = TRUE)
  expect_equal(calls, 1)
})

test_that("download_data uses its timeout, even below the option", {
  used <- NULL
  local_mocked_bindings(download_file = function(url, file) {
    used <<- getOption("timeout")
    writeLines("data", file)
    0L
  })
  old <- options(timeout = 600)
  on.exit(options(old), add = TRUE)
  download_data("http://x", tempfile(), "STARK-B", "Ca II", 20, wait = 0)
  expect_equal(used, 20)
  expect_equal(getOption("timeout"), 600)
})

test_that("download_data reports other failures", {
  local_mocked_bindings(download_file = function(url, file) {
    warning("URL '", url, "': status was 'Could not resolve host'")
    stop("cannot open URL '", url, "'")
  })
  expect_error(download_data("http://x", tempfile(), "NIST", "Ca II", 10, wait = 0),
               "status was 'Could not resolve host'", fixed = TRUE)
  local_mocked_bindings(download_file = function(url, file) 0L)
  expect_error(download_data("http://x", tempfile(), "NIST", "Ca II", 10, wait = 0),
               "(empty response)", fixed = TRUE)
})
