# Builds data/forageShots.rda, the laser shots of 20 forage measurements, from
# the raw CSV export (see data-raw/forageLIBS.R). Run from the package root,
# with specProc installed: source("data-raw/forageShots.R")
#
# To keep the package small, only two windows are kept: 380-430 nm (Ca II H
# and K, Ca I 422.67 nm, Al I, the CN band) and 760-780 nm (K I 766.49 and
# 769.90 nm, O I 777 nm). The 20 measurements are those with a shot rejected
# by reject_shots() in these windows, completed by measurements drawn at
# random.

raw <- utils::read.csv(
  "data-raw/forageLIBS.csv",
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE
)
raw <- raw[!is.na(raw$ID), ]
meta_cols <- c(
  "ID", "spectre", "replique", "type", "Info",
  "Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Se", "Na", "S", "Zn"
)
stopifnot(identical(names(raw)[seq_along(meta_cols)], meta_cols))
wl <- suppressWarnings(as.numeric(names(raw)))
spectral <- which(!is.na(wl) & seq_along(wl) > length(meta_cols))[1:7152]
window <- spectral[(wl[spectral] >= 380 & wl[spectral] <= 430) |
                     (wl[spectral] >= 760 & wl[spectral] <= 780)]

shots <- data.frame(
  Measurement = as.integer(raw$spectre),
  Sample = raw$Info,
  shot = as.integer(raw$replique),
  Ca = raw$Ca,
  K = raw$K,
  round(as.matrix(raw[window])),
  check.names = FALSE
)
stopifnot(all(table(shots$Measurement) == 8))

res <- specProc::reject_shots(shots, Measurement, shot = shot)
flagged <- unique(res$Measurement[res$.rejected])
set.seed(2024)
others <- sample(setdiff(unique(shots$Measurement), flagged), 20 - length(flagged))
keep <- sort(c(flagged, others))
forageShots <- shots[shots$Measurement %in% keep, ]
forageShots <- forageShots[order(forageShots$Measurement, forageShots$shot), ]
for (col in names(forageShots)[-(1:5)]) forageShots[[col]] <- as.integer(forageShots[[col]])
rownames(forageShots) <- NULL
forageShots <- tibble::as_tibble(forageShots)

save(forageShots, file = "data/forageShots.rda", compress = "xz", version = 3)
