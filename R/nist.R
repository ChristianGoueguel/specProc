#' @title Atomic Line Data from the NIST Atomic Spectra Database
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Retrieves the lines of an atom or ion in a wavelength range from the NIST
#' Atomic Spectra Database (ASD): wavelengths, transition probabilities,
#' level energies and statistical weights, as needed by [boltzmann_plot()]
#' and [saha_boltzmann_plot()]. The data are downloaded on demand; they are
#' not distributed with specProc.
#'
#' @details
#' Wavelengths are in air between 200 and 2000 nm, and in vacuum outside
#' this range, as in the ASD. The observed wavelength is used when there is
#' one, the Ritz wavelength otherwise. Energies in brackets or with other
#' qualifiers are returned as they can be read as numbers; check the ASD
#' for such levels. The accuracy grade of the transition probability
#' (`accuracy`: AAA, AA, A+, A, B+, B, C+, C, D+, D, E) matters for plasma
#' diagnostics: prefer lines of grade B or better.
#'
#' The results of a query are cached for the R session. Cite the database
#' when you use these data:
#'  - Kramida, A., Ralchenko, Yu., Reader, J., and NIST ASD Team. NIST
#'    Atomic Spectra Database, \url{https://physics.nist.gov/asd}. National
#'    Institute of Standards and Technology, Gaithersburg, MD.
#'
#' @param species A character string naming the emitter in spectroscopic
#'   notation, such as `"Ca I"` or `"Ca II"`.
#' @param wavelength A numeric vector of length 2: the wavelength range, in
#'   nm.
#' @param with_aki A logical: keep only the lines with a transition
#'   probability (`TRUE`, default).
#' @param timeout The download timeout, in seconds. Default is 120.
#'
#' @return A tibble with one row per line and columns `species`,
#'   `wavelength` (nm), `Aki` (s\eqn{^{-1}}), `fik` (oscillator strength),
#'   `accuracy`, `Ei` and `Ek` (lower and upper level energies, eV), `gi`
#'   and `gk` (statistical weights), `lower` and `upper` (configuration,
#'   term and J of the levels) and `intensity` (the relative intensity
#'   listed by the ASD, as text).
#'
#' @seealso [nist_ionization_energy()], [boltzmann_plot()],
#'   [saha_boltzmann_plot()], [starkb_lines()]
#' @export nist_lines
#'
#' @examples
#' \donttest{
#' # needs an internet connection
#' ca <- try(nist_lines("Ca II", wavelength = c(310, 400)))
#' if (!inherits(ca, "try-error")) ca
#' }
nist_lines <- function(species, wavelength, with_aki = TRUE, timeout = 120) {
  sp <- parse_species(species)
  if (missing(wavelength) || !is.numeric(wavelength) || length(wavelength) != 2 ||
      anyNA(wavelength) || any(wavelength <= 0)) {
    stop("'wavelength' must be a numeric vector of length 2 (nm).")
  }
  check_flag(with_aki, "with_aki")
  check_number(timeout, "timeout", lower = 0, lower_open = TRUE)
  wavelength <- sort(wavelength)
  key <- paste("lines", sp$label, wavelength[1], wavelength[2])
  lines <- nist_cache[[key]]
  if (is.null(lines)) {
    query <- list(
      spectra = sp$label, low_w = wavelength[1], upp_w = wavelength[2], unit = 1,
      submit = "Retrieve Data", format = 2, line_out = 0, en_unit = 1, output_type = 0,
      bibrefs = 1, show_obs_wl = 1, show_calc_wl = 1, order_out = 0, max_low_enrg = "",
      show_av = 2, max_upp_enrg = "", tsb_value = 0, min_str = "", A_out = 0, f_out = "on",
      intens_out = "on", max_str = "", allowed_out = 1, forbid_out = 1, min_accur = "",
      min_intens = "", conf_out = "on", term_out = "on", enrg_out = "on", J_out = "on",
      g_out = "on", page_size = 15, remove_js = "on", show_wn = 1
    )
    text <- nist_download("https://physics.nist.gov/cgi-bin/ASD/lines1.pl", query, sp$label, timeout)
    lines <- parse_nist_lines(text, sp$label)
    assign(key, lines, envir = nist_cache)
  }
  if (with_aki) {
    lines <- lines[!is.na(lines$Aki), ]
  }
  lines
}

#' @title Ionization Energies from the NIST Atomic Spectra Database
#'
#' @description
#' Retrieves ionization energies from the NIST Atomic Spectra Database, for
#' example the ionization energy of the neutral atom needed by
#' [saha_boltzmann_plot()].
#'
#' @param species A character vector of emitters in spectroscopic notation:
#'   `"Ca I"` gives the energy needed to ionize neutral calcium, `"Ca II"`
#'   that needed to ionize Ca\eqn{^{+}}.
#' @inheritParams nist_lines
#'
#' @return A named numeric vector of ionization energies, in eV.
#'
#' @seealso [nist_lines()], [saha_boltzmann_plot()]
#' @export nist_ionization_energy
#'
#' @examples
#' \donttest{
#' # needs an internet connection
#' try(nist_ionization_energy(c("Ca I", "Mg I")))
#' }
nist_ionization_energy <- function(species, timeout = 120) {
  if (!is.character(species) || length(species) == 0) {
    stop("'species' must be a character vector, such as \"Ca I\".")
  }
  check_number(timeout, "timeout", lower = 0, lower_open = TRUE)
  parsed <- lapply(species, parse_species)
  out <- vapply(parsed, function(sp) {
    key <- paste("ie", sp$element)
    table <- nist_cache[[key]]
    if (is.null(table)) {
      query <- list(spectra = sp$element, units = 1, format = 2, order = 0,
                    sp_name_out = "on", ion_charge_out = "on", e_out = 0,
                    submit = "Retrieve Data")
      text <- nist_download("https://physics.nist.gov/cgi-bin/ASD/ie.pl", query, sp$element, timeout)
      table <- parse_nist_ie(text)
      assign(key, table, envir = nist_cache)
    }
    value <- table$energy[table$species == sp$label]
    if (length(value) != 1 || is.na(value)) {
      stop("No ionization energy for ", sp$label, " in the NIST database.", call. = FALSE)
    }
    value
  }, numeric(1))
  stats::setNames(out, vapply(parsed, `[[`, character(1), "label"))
}

# ---- internals ---------------------------------------------------------------

nist_cache <- new.env(parent = emptyenv())

nist_download <- function(url, query, label, timeout) {
  full <- paste0(url, "?", paste(
    names(query), vapply(query, function(v) utils::URLencode(as.character(v), reserved = TRUE), character(1)),
    sep = "=", collapse = "&"
  ))
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  old <- options(timeout = max(timeout, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  status <- tryCatch(
    utils::download.file(full, file, quiet = TRUE, mode = "wb"),
    error = function(e) e, warning = function(w) w
  )
  if (inherits(status, "condition") || !file.exists(file) || file.size(file) == 0) {
    msg <- if (inherits(status, "condition")) conditionMessage(status) else "empty response"
    stop("Could not download NIST data for ", label, " (", msg, "). ",
         "Check the internet connection, or try again later.", call. = FALSE)
  }
  text <- readLines(file, warn = FALSE, encoding = "UTF-8")
  if (length(text) == 0 || grepl("<html|<!DOCTYPE", text[1], ignore.case = TRUE) ||
      any(grepl("<title>NIST ASD : Input Error", text, fixed = TRUE))) {
    stop("The NIST database returned no data for ", label,
         ": check the species name and the wavelength range.", call. = FALSE)
  }
  text
}

# Reads NIST CSV text, whose fields are written as ="value" for spreadsheets.
read_nist_csv <- function(text) {
  text <- text[nzchar(trimws(text))]
  df <- utils::read.csv(text = text, check.names = FALSE, colClasses = "character",
                        strip.white = TRUE)
  df <- df[, nzchar(names(df)), drop = FALSE]  # trailing comma
  df[] <- lapply(df, function(v) sub('^="(.*)"$', "\\1", v))
  df
}

# Numbers from ASD fields, which can carry brackets, parentheses or "?".
nist_number <- function(v) {
  suppressWarnings(as.numeric(gsub("[][()?]", "", v)))
}

parse_nist_lines <- function(text, label) {
  df <- read_nist_csv(text)
  column <- function(pattern) {
    hit <- grep(pattern, names(df))
    if (length(hit) == 0) rep(NA_character_, nrow(df)) else df[[hit[1]]]
  }
  observed <- nist_number(column("^obs_wl"))
  ritz <- nist_number(column("^ritz_wl"))
  level <- function(conf, term, j) {
    out <- trimws(paste(column(conf), column(term), column(j)))
    ifelse(out == "NA NA NA", "", out)
  }
  tibble::tibble(
    species = label,
    wavelength = ifelse(is.na(observed), ritz, observed),
    Aki = nist_number(column("^Aki")),
    fik = nist_number(column("^fik")),
    accuracy = column("^Acc"),
    Ei = nist_number(column("^Ei")),
    Ek = nist_number(column("^Ek")),
    gi = nist_number(column("^g_i")),
    gk = nist_number(column("^g_k")),
    lower = level("^conf_i", "^term_i", "^J_i"),
    upper = level("^conf_k", "^term_k", "^J_k"),
    intensity = column("^intens")
  )
}

parse_nist_ie <- function(text) {
  df <- read_nist_csv(text)
  name <- grep("^Sp", names(df))[1]
  energy <- grep("^Ionization Energy", names(df))[1]
  if (is.na(name) || is.na(energy)) {
    stop("Unexpected format of the NIST ionization energy table.", call. = FALSE)
  }
  data.frame(species = df[[name]], energy = nist_number(df[[energy]]), stringsAsFactors = FALSE)
}
