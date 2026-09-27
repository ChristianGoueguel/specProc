# Builds data/fourrage.rda from the raw CSV export.
# Run from the package root: source("data-raw/fourrage.R")
#
# The export (data-raw/fourrage.csv, ~170 MB, not tracked by git) holds one
# row per laser shot: 8 shots for each of 368 measurements. It also contains
# empty trailing rows and, after the last wavelength, stray columns from a
# semicolon-separated export; both are dropped here. To keep the package
# small, the 8 shots of each measurement are averaged.

raw <- utils::read.csv(
  "data-raw/fourrage.csv",
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE
)

meta_cols <- c(
  "ID", "spectre", "replique", "type", "Info",
  "Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Se", "Na", "S", "Zn"
)
stopifnot(identical(names(raw)[seq_along(meta_cols)], meta_cols))

# Wavelength columns are the numeric names that follow the metadata; the
# stray columns after them have non-numeric (or empty) names.
wl <- suppressWarnings(as.numeric(names(raw)))
first_stray <- which(is.na(wl) & seq_along(wl) > length(meta_cols))[1]
spec_cols <- seq(length(meta_cols) + 1, first_stray - 1)
stopifnot(length(spec_cols) == 7152)

raw <- raw[!is.na(raw$ID), ]
stopifnot(all(raw$type == "Fourrage"), all(is.na(raw$Se)))

spectra <- as.matrix(raw[spec_cols])
stopifnot(is.numeric(spectra), !anyNA(spectra), all(table(raw$spectre) == 8))

# Mean of the 8 shots, rounded to integer counts (the rounding error is far
# below the shot-to-shot variation).
shots <- round(rowsum(spectra, raw$spectre, reorder = FALSE) / 8)
storage.mode(shots) <- "integer"
first <- raw[!duplicated(raw$spectre), ]
stopifnot(identical(rownames(shots), as.character(first$spectre)))

fourrage <- tibble::as_tibble(cbind(
  data.frame(
    Measurement = as.integer(first$spectre),
    Sample = first$Info,
    Ca = first$Ca, Cl = first$Cl, Cu = first$Cu, Fe = first$Fe,
    Mg = first$Mg, Mn = first$Mn, Mo = first$Mo, P = first$P,
    K = first$K, Na = first$Na, S = first$S, Zn = first$Zn,
    stringsAsFactors = FALSE
  ),
  as.data.frame(shots, check.names = FALSE)
))

save(fourrage, file = "data/fourrage.rda", compress = "xz", version = 3)
