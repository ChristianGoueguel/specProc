#' @title Persistent Cache of Downloaded Data
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Lists or clears the data downloaded from the NIST Atomic Spectra
#' Database, STARK-B and ExoMol and kept on disk, so that they are reused in
#' later sessions and without an internet connection.
#'
#' @details
#' Downloaded data are always kept for the R session. To keep them between
#' sessions, set the option `specProc.cache`, for example in your
#' `.Rprofile`:
#'  - `options(specProc.cache = TRUE)` keeps them in the user cache
#'    directory of specProc, given by [tools::R_user_dir()];
#'  - `options(specProc.cache = "path/to/dir")` keeps them in that
#'    directory;
#'  - `NULL` or `FALSE` (default) keeps them for the session only.
#'
#' The results of [nist_lines()], [nist_ionization_energy()],
#' [nist_levels()] (and so [libs_lines()] and [line_finder()]),
#' [starkb_lines()] and [molecular_spectrum()] are then read from the cache
#' when they are there. Entries older than the option
#' `specProc.cache_max_age` (in days, default 180) are downloaded again, as
#' the databases are updated; when the download fails, the older copy is used
#' with a warning. The NIST and STARK-B entries are small (a few kilobytes
#' to a megabyte); ExoMol line lists can be large, see [exomol_datasets()].
#'
#' The cache can also be cleared by deleting its directory.
#'
#' @param source The sources to clear: `"nist"`, `"starkb"` and/or
#'   `"exomol"`. Default is all of them.
#' @param older_than Clear only the entries older than this number of days,
#'   or `NULL` (default) for all of them.
#'
#' @return `download_cache()`: a tibble with one row per file of the
#'   persistent cache, with its `source`, `file` name, `size` (kB),
#'   `modified` time and `age` (days); its directory is the attribute
#'   `dir` (`NULL` when the cache is not enabled). `clear_download_cache()`:
#'   invisibly, the paths of the deleted files. It also clears the session
#'   cache of the sources.
#'
#' @seealso [nist_lines()], [starkb_lines()], [molecular_spectrum()]
#' @export download_cache
#'
#' @examples
#' # keep the downloaded data in a directory (here a temporary one)
#' old <- options(specProc.cache = file.path(tempdir(), "specProc-cache"))
#' download_cache()
#' clear_download_cache()
#' options(old)
download_cache <- function() {
  dir <- cache_dir()
  empty <- tibble::tibble(source = character(), file = character(), size = numeric(),
                          modified = as.POSIXct(character()), age = numeric())
  if (is.null(dir)) {
    message("The persistent cache is not enabled: set options(specProc.cache = TRUE).")
    return(structure(empty, dir = NULL))
  }
  files <- cache_files()
  if (length(files) == 0) return(structure(empty, dir = dir))
  info <- file.info(files)
  out <- tibble::tibble(
    source = basename(dirname(files)), file = basename(files), size = info$size / 1e3,
    modified = info$mtime,
    age = as.numeric(difftime(Sys.time(), info$mtime, units = "days"))
  )
  structure(out[order(out$source, out$file), ], dir = dir)
}

#' @rdname download_cache
#' @export
clear_download_cache <- function(source = c("nist", "starkb", "exomol"), older_than = NULL) {
  source <- match.arg(source, several.ok = TRUE)
  if (!is.null(older_than)) check_number(older_than, "older_than", lower = 0)
  for (s in source) {
    env <- session_cache(s)
    rm(list = ls(env, all.names = TRUE), envir = env)
  }
  files <- cache_files(source)
  if (!is.null(older_than) && length(files) > 0) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(files), units = "days"))
    files <- files[age > older_than]
  }
  unlink(files)
  invisible(files)
}

# ---- internals ---------------------------------------------------------------

# The directory of the persistent cache, or NULL when it is not enabled.
cache_dir <- function() {
  option <- getOption("specProc.cache")
  if (is.null(option) || isFALSE(option)) return(NULL)
  if (isTRUE(option)) return(tools::R_user_dir("specProc", which = "cache"))
  if (!is.character(option) || length(option) != 1 || is.na(option) || !nzchar(option)) {
    stop("The option specProc.cache must be TRUE, FALSE or the path of a directory.",
         call. = FALSE)
  }
  path.expand(option)
}

cache_max_age <- function() {
  age <- getOption("specProc.cache_max_age", 180)
  if (!is.numeric(age) || length(age) != 1 || is.na(age) || age < 0) {
    stop("The option specProc.cache_max_age must be a number of days.", call. = FALSE)
  }
  age
}

session_cache <- function(source) {
  switch(source, nist = nist_cache, starkb = starkb_cache, exomol = exomol_cache)
}

# The files of the persistent cache, in the folders of the sources.
cache_files <- function(source = c("nist", "starkb", "exomol")) {
  dir <- cache_dir()
  if (is.null(dir)) return(character())
  unlist(lapply(source, function(s) {
    list.files(file.path(dir, s), full.names = TRUE)
  }), use.names = FALSE)
}

# The data of a query: from the session cache, the persistent cache (when
# enabled, and if not older than the maximum age) or `download()`, then kept
# in both. When the download fails, an older copy is used with a warning.
cached_query <- function(source, key, label, download) {
  env <- session_cache(source)
  if (!is.null(env[[key]])) return(env[[key]])
  dir <- cache_dir()
  file <- if (is.null(dir)) NULL else
    file.path(dir, source, paste0(gsub("[^A-Za-z0-9.+-]+", "_", key), ".rds"))
  saved <- !is.null(file) && file.exists(file)
  if (saved && difftime(Sys.time(), file.mtime(file), units = "days") <= cache_max_age()) {
    value <- readRDS(file)
  } else {
    value <- tryCatch(download(), error = function(e) {
      if (!saved) stop(e)
      warning(conditionMessage(e), " Using the copy of ", label, " cached on ",
              format(file.mtime(file), "%Y-%m-%d"), ".", call. = FALSE)
      NULL
    })
    if (is.null(value)) {
      value <- readRDS(file)
    } else if (!is.null(file)) {
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
      saveRDS(value, file)
    }
  }
  assign(key, value, envir = env)
  value
}
