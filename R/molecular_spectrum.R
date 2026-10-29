#' @title Simulated Emission Spectra of Diatomic Molecules
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Simulates the emission spectrum of a diatomic molecule in local
#' thermodynamic equilibrium, from a line list of the ExoMol database,
#' convolved with the instrument profile, to compare with the molecular bands
#' of a measured spectrum.
#'
#' @details
#' The line list (energy levels and Einstein coefficients of the
#' rovibronic transitions) is downloaded from ExoMol. In a plasma in LTE at
#' temperature \eqn{T}, optically thin, the intensity of the line from the
#' upper level \eqn{u} (energy \eqn{E_u}, degeneracy \eqn{g_u}) to the lower
#' level \eqn{l} is
#' \deqn{I \propto g_u A_{ul}\, \nu \exp\left(-\frac{hc E_u}{k_B T}\right)}
#' in energy units (without \eqn{\nu} for photon counts), with the same
#' temperature for rotation and vibration. The lines are convolved with a
#' Gaussian profile of full width at half maximum `resolution`, and the
#' spectrum is normalized to its maximum in the range.
#'
#' By default, the empirical MoLLIST line list of Bernath (2020) is used
#' when ExoMol has one for the most abundant isotopologue (CN, C2, CH, OH,
#' NH, CaF, MgH, CaH), as they are small (about 1 MB) and their line
#' positions accurate, and the recommended line list otherwise (`AlO`:
#' 33 MB). Other line lists are much larger: hundreds of megabytes for TiO,
#' MgO or CaO. [exomol_datasets()] lists the line lists of a molecule and
#' their sizes.
#'
#' The files are kept for the R session, or in the directory `cache` to be
#' reused in later sessions: a molecule whose files are in `cache` needs no
#' internet connection when `isotopologue` and `dataset` are given. Partition
#' functions are not needed, as the spectrum is normalized. Wavelengths are
#' in air between 200 and 2000 nm.
#'
#' ExoMol data are released under the Creative Commons Attribution-ShareAlike
#' 4.0 license; cite ExoMol and the article of the line list (given on the
#' ExoMol page of the data set) when you use them.
#'
#' @param molecule The molecule, as named by ExoMol, such as `"CN"`, `"C2"`,
#'   `"OH"` or `"AlO"`.
#' @param temperature The temperature, in K. Default is 5000.
#' @param wavelength A numeric vector of length 2: the wavelength range, in
#'   nm.
#' @param resolution The full width at half maximum of the instrument
#'   profile, in nm. Default is 0.05.
#' @param isotopologue The isotopologue, as in the file names of ExoMol, such
#'   as `"12C-14N"` or `"13C-14N"`. Default is the first one listed by ExoMol,
#'   the most abundant.
#' @param dataset The line list, such as `"MoLLIST-CN"`. See Details for the
#'   default.
#' @param units The units of the intensities: `"energy"` (default) or
#'   `"photons"`.
#' @param lines A logical: return the lines instead of the convolved
#'   spectrum (`FALSE`, default).
#' @param cache A directory where the files are kept between sessions, or
#'   `NULL` (default) to keep them for the session only.
#' @param timeout The download timeout, in seconds. Default is 300.
#'
#' @return A tibble with the `wavelength` (nm) and the relative `intensity`
#'   of the spectrum, on a grid of step `resolution / 10`; with
#'   `lines = TRUE`, a tibble with one row per line: `wavelength` (nm),
#'   `wavenumber` (cm\eqn{^{-1}}), relative `intensity`, Einstein coefficient
#'   `A` (s\eqn{^{-1}}), energies `E_upper` and `E_lower` (cm\eqn{^{-1}}) and
#'   total angular momenta `J_upper` and `J_lower`. The molecule,
#'   isotopologue, line list, temperature and resolution are attributes.
#'
#' @references
#'  - Tennyson, J., Yurchenko, S.N., et al. (2020). The 2020 release of the
#'    ExoMol database: molecular line lists for exoplanet and other hot
#'    atmospheres. Journal of Quantitative Spectroscopy and Radiative
#'    Transfer, 255:107228. \url{https://exomol.com}
#'  - Bernath, P.F. (2020). MoLLIST: molecular line lists, intensities and
#'    spectra. Journal of Quantitative Spectroscopy and Radiative Transfer,
#'    240:106687.
#'
#' @seealso [molecular_bands()], [exomol_datasets()]
#' @export molecular_spectrum
#'
#' @examples
#' \donttest{
#' # needs an internet connection (about 1 MB)
#' cn <- try(molecular_spectrum("CN", temperature = 6000, wavelength = c(383, 389)))
#' if (!inherits(cn, "try-error")) {
#'   plot(cn$wavelength, cn$intensity, type = "l", xlab = "Wavelength (nm)",
#'        ylab = "Relative intensity")
#' }
#' }
molecular_spectrum <- function(molecule, temperature = 5000, wavelength, resolution = 0.05,
                               isotopologue = NULL, dataset = NULL, units = "energy",
                               lines = FALSE, cache = NULL, timeout = 300) {
  units <- match.arg(units, c("energy", "photons"))
  if (!is.character(molecule) || length(molecule) != 1 || is.na(molecule)) {
    stop("'molecule' must be one molecule, such as \"CN\".", call. = FALSE)
  }
  check_number(temperature, "temperature", lower = 0, lower_open = TRUE)
  if (missing(wavelength) || !is.numeric(wavelength) || length(wavelength) != 2 ||
      anyNA(wavelength) || any(wavelength <= 0) || wavelength[1] == wavelength[2]) {
    stop("'wavelength' must be a numeric vector of length 2 (nm).", call. = FALSE)
  }
  check_number(resolution, "resolution", lower = 0, lower_open = TRUE)
  check_flag(lines, "lines")
  wavelength <- sort(wavelength)
  source <- exomol_source(molecule, isotopologue, dataset, timeout)
  files <- exomol_files(source, cache, timeout)
  data <- exomol_read(files)

  # lines within the range, widened by the profile
  sigma <- resolution / (2 * sqrt(2 * log(2)))
  window <- air_to_vacuum(c(wavelength[1] - 5 * sigma, wavelength[2] + 5 * sigma))
  wavenumber <- data$E[data$upper] - data$E[data$lower]
  keep <- which(wavenumber >= 1e7 / window[2] & wavenumber <= 1e7 / window[1])
  wavenumber <- wavenumber[keep]
  upper <- data$upper[keep]
  intensity <- data$g[upper] * data$A[keep] * exp(-1.438777 * data$E[upper] / temperature) *
    (if (units == "energy") wavenumber else 1)
  wl <- vacuum_to_air(1e7 / wavenumber)
  if (length(keep) == 0 || all(intensity == 0)) {
    stop("The line list of ", molecule, " has no line between ", wavelength[1], " and ",
         wavelength[2], " nm.", call. = FALSE)
  }
  out <- if (lines) {
    lower <- data$lower[keep]
    tibble::tibble(wavelength = wl, wavenumber = wavenumber, intensity = intensity / max(intensity),
                   A = data$A[keep], E_upper = data$E[upper], E_lower = data$E[lower],
                   J_upper = data$J[upper], J_lower = data$J[lower])[order(wl), ]
  } else {
    gaussian_spectrum(wl, intensity, wavelength, resolution)
  }
  attr(out, "molecule") <- molecule
  attr(out, "isotopologue") <- source$isotopologue
  attr(out, "dataset") <- source$dataset
  attr(out, "temperature") <- temperature
  attr(out, "resolution") <- resolution
  out
}

#' @title Line Lists of a Molecule in the ExoMol Database
#'
#' @description
#' Lists the line lists of a molecule in the ExoMol database, for each
#' isotopologue, with the sizes of their files, to choose the `dataset` of
#' [molecular_spectrum()].
#'
#' @details
#' The list is read from the ExoMol API, and needs the jsonlite package and
#' an internet connection.
#'
#' @inheritParams molecular_spectrum
#' @param timeout The download timeout, in seconds. Default is 60.
#'
#' @return A tibble with one row per line list: the `molecule`, the
#'   `isotopologue` (as in the file names), the `dataset`, whether ExoMol
#'   `recommended` it, and the sizes of its transition and states files
#'   (`trans_mb`, `states_mb`, in MB; `NA` when not listed).
#'
#' @seealso [molecular_spectrum()]
#' @export exomol_datasets
#'
#' @examples
#' \donttest{
#' # needs an internet connection
#' if (rlang::is_installed("jsonlite")) {
#'   try(exomol_datasets("CN"))
#' }
#' }
exomol_datasets <- function(molecule, timeout = 60) {
  if (!is.character(molecule) || length(molecule) != 1 || is.na(molecule)) {
    stop("'molecule' must be one molecule, such as \"CN\".", call. = FALSE)
  }
  sets <- exomol_api(molecule, timeout)
  sets$urls <- NULL
  sets
}

# ---- internals ---------------------------------------------------------------

exomol_cache <- new.env(parent = emptyenv())

# The line lists of a molecule from the ExoMol API, with the URLs of their
# files; kept for the R session.
exomol_api <- function(molecule, timeout) {
  rlang::check_installed("jsonlite", reason = "to read the ExoMol API.")
  key <- paste0("api:", molecule)
  if (is.null(exomol_cache[[key]])) {
    file <- tempfile(fileext = ".json")
    on.exit(unlink(file), add = TRUE)
    download_data(paste0("https://exomol.com/api/?molecule=", utils::URLencode(molecule, reserved = TRUE),
                         "&datatype=linelist"), file, "ExoMol", molecule, timeout)
    text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    if (!startsWith(trimws(text), "{")) {
      stop("ExoMol has no line list for ", molecule, ".", call. = FALSE)
    }
    assign(key, exomol_parse_api(jsonlite::fromJSON(text, simplifyVector = FALSE), molecule),
           envir = exomol_cache)
  }
  exomol_cache[[key]]
}

# The small empirical line lists of the most abundant isotopologues.
exomol_defaults <- c(CN = "12C-14N/MoLLIST-CN", C2 = "12C2/MoLLIST-C2", CH = "12C-1H/MoLLIST",
                     OH = "16O-1H/MoLLIST-OH", NH = "14N-1H/MoLLIST-NH", CaF = "40Ca-19F/MoLLIST",
                     MgH = "24Mg-1H/MoLLIST-MgH", CaH = "40Ca-1H/MoLLIST-CaH",
                     AlO = "27Al-16O/ATP")

exomol_parse_api <- function(api, molecule) {
  rows <- list()
  for (iso in api) {
    for (name in names(iso$linelist)) {
      info <- iso$linelist[[name]]
      if (!is.list(info) || is.null(info$files)) next
      urls <- vapply(info$files, function(f) f$url %||% "", character(1))
      sizes <- vapply(info$files, function(f) as.numeric(f$size %||% NA_real_), numeric(1))
      trans <- grepl("\\.trans\\.bz2$", urls)
      states <- grepl("\\.states\\.bz2$", urls)
      if (!any(trans) || !any(states)) next
      slug <- sub("^.*/db/[^/]+/([^/]+)/.*$", "\\1", urls[states][1])
      rows[[length(rows) + 1]] <- tibble::tibble(
        molecule = molecule, isotopologue = slug, dataset = name,
        recommended = isTRUE(info$recommended),
        trans_mb = sum(sizes[trans]) / 1e6, states_mb = sizes[states][1] / 1e6,
        urls = list(paste0("https://", sub("^https?://", "", urls[trans | states])))
      )
    }
  }
  if (length(rows) == 0) stop("ExoMol has no line list for ", molecule, ".", call. = FALSE)
  do.call(rbind, rows)
}

# The URLs of the files of a line list: from the defaults or the given names
# when possible, otherwise from the ExoMol API.
exomol_source <- function(molecule, isotopologue, dataset, timeout) {
  base <- "https://exomol.com/db"
  standard <- function(iso, ds) {
    paste0(base, "/", molecule, "/", iso, "/", ds, "/", iso, "__", ds, c(".states.bz2", ".trans.bz2"))
  }
  if (!is.null(isotopologue) && !is.null(dataset)) {
    return(list(isotopologue = isotopologue, dataset = dataset, urls = standard(isotopologue, dataset)))
  }
  if (is.null(isotopologue) && is.null(dataset) && molecule %in% names(exomol_defaults)) {
    parts <- strsplit(exomol_defaults[[molecule]], "/", fixed = TRUE)[[1]]
    return(list(isotopologue = parts[1], dataset = parts[2], urls = standard(parts[1], parts[2])))
  }
  sets <- exomol_api(molecule, timeout)
  if (!is.null(isotopologue)) {
    sets <- sets[sets$isotopologue == isotopologue, ]
    if (nrow(sets) == 0) stop("ExoMol has no line list for ", isotopologue, ".", call. = FALSE)
  } else {
    sets <- sets[sets$isotopologue == sets$isotopologue[1], ]
  }
  if (!is.null(dataset)) {
    sets <- sets[sets$dataset == dataset, ]
    if (nrow(sets) == 0) stop("ExoMol has no line list ", dataset, " for ", molecule, ".", call. = FALSE)
  } else {
    order <- order(!grepl("^MoLLIST", sets$dataset), !sets$recommended)
    sets <- sets[order, ]
  }
  list(isotopologue = sets$isotopologue[1], dataset = sets$dataset[1], urls = sets$urls[[1]])
}

# Local paths of the files, downloaded when missing.
exomol_files <- function(source, cache, timeout) {
  dir <- cache %||% file.path(tempdir(), "specProc-exomol")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  paths <- file.path(dir, basename(source$urls))
  for (i in seq_along(paths)) {
    if (!file.exists(paths[i]) || file.size(paths[i]) == 0) {
      size <- if (grepl("trans", paths[i])) " (this may take a while for a large line list)" else ""
      message("Downloading ", basename(paths[i]), " from ExoMol", size, ".")
      download_data(source$urls[i], paths[i], "ExoMol", paste(source$isotopologue, source$dataset),
                    timeout)
    }
  }
  list(states = paths[grepl("\\.states\\.bz2$", paths)], trans = paths[grepl("\\.trans\\.bz2$", paths)])
}

# Energies (cm-1), degeneracies and J of the levels, indexed by their ID,
# and the upper and lower IDs and Einstein coefficients of the transitions;
# kept for the R session.
exomol_read <- function(files) {
  key <- paste(c(files$states, files$trans), collapse = "|")
  if (!is.null(exomol_cache[[key]])) return(exomol_cache[[key]])
  states <- scan(files$states, what = list(0L, 0, 0, ""), flush = TRUE, quiet = TRUE)
  n <- max(states[[1]])
  E <- g <- J <- rep(NA_real_, n)
  E[states[[1]]] <- states[[2]]
  g[states[[1]]] <- states[[3]]
  J[states[[1]]] <- suppressWarnings(as.numeric(states[[4]]))
  trans <- lapply(files$trans, scan, what = list(0L, 0L, 0), flush = TRUE, quiet = TRUE)
  data <- list(E = E, g = g, J = J,
               upper = unlist(lapply(trans, `[[`, 1)), lower = unlist(lapply(trans, `[[`, 2)),
               A = unlist(lapply(trans, `[[`, 3)))
  assign(key, data, envir = exomol_cache)
  data
}

# Lines convolved with a Gaussian profile of FWHM `resolution`, on a grid of
# step resolution / 10, normalized to the maximum.
gaussian_spectrum <- function(wl, intensity, range, resolution) {
  step <- resolution / 10
  grid <- seq(range[1], range[2], by = step)
  sigma <- resolution / (2 * sqrt(2 * log(2)))
  half <- ceiling(5 * sigma / step)
  # each line shared between the two nearest points of a padded grid
  pos <- (wl - range[1]) / step + half + 1
  n <- length(grid) + 2 * half
  inside <- pos >= 1 & pos < n
  i <- floor(pos[inside])
  f <- pos[inside] - i
  binned <- tabulate_weights(i, intensity[inside] * (1 - f), n) +
    tabulate_weights(i + 1, intensity[inside] * f, n)
  kernel <- exp(-((-half):half * step)^2 / (2 * sigma^2))
  smoothed <- stats::filter(binned, kernel / sum(kernel), sides = 2)[half + seq_along(grid)]
  smoothed[is.na(smoothed)] <- 0
  tibble::tibble(wavelength = grid, intensity = smoothed / max(smoothed))
}

tabulate_weights <- function(index, weights, n) {
  out <- numeric(n)
  sums <- rowsum(weights, index)
  out[as.integer(rownames(sums))] <- sums[, 1]
  out
}
