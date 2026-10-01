# soilLIBS is no longer shipped with specProc (removed in 0.8.0, to keep
# the package data under the CRAN size guideline). The raw export and this
# script are kept to rebuild it, for example for a separate data package.
#
# Builds data/soilLIBS.rda from the raw CSV export.
# Run from the package root: source("data-raw/soilLIBS.R")

raw <- utils::read.csv(
  "data-raw/soilLIBS.csv",
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE
)

meta_cols <- c("Sample", "Location", "Clay", "Sand", "Silt", "Texture", "Structure", "Type")
stopifnot(identical(names(raw)[seq_along(meta_cols)], meta_cols))

spectra <- raw[setdiff(names(raw), meta_cols)]
stopifnot(!anyNA(spectra), all(vapply(spectra, function(v) all(v == round(v)), logical(1))))
spectra[] <- lapply(spectra, as.integer)

soilLIBS <- tibble::as_tibble(cbind(
  data.frame(
    Sample = raw$Sample,
    Location = as.integer(raw$Location),
    Clay = raw$Clay,
    Sand = raw$Sand,
    Silt = raw$Silt,
    Texture = factor(raw$Texture),
    Structure = factor(raw$Structure),
    Type = factor(raw$Type),
    stringsAsFactors = FALSE
  ),
  spectra
))

save(soilLIBS, file = "data/soilLIBS.rda", compress = "xz", version = 3)
