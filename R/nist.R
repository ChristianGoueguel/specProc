#' @title Atomic Line Data from the NIST Atomic Spectra Database
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Retrieves the lines of an atom or ion in a wavelength range from the NIST
#' Atomic Spectra Database (ASD): wavelengths, transition probabilities,
#' level energies and statistical weights, as needed by [boltzmann()]
#' and [saha_boltzmann()]. The data are downloaded on demand; they are
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
#' @seealso [nist_ionization_energy()], [boltzmann()],
#'   [saha_boltzmann()], [starkb_lines()]
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
#' [saha_boltzmann()].
#'
#' @param species A character vector of emitters in spectroscopic notation:
#'   `"Ca I"` gives the energy needed to ionize neutral calcium, `"Ca II"`
#'   that needed to ionize Ca\eqn{^{+}}.
#' @inheritParams nist_lines
#'
#' @return A named numeric vector of ionization energies, in eV.
#'
#' @seealso [nist_lines()], [saha_boltzmann()]
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

#' @title Energy Levels and Partition Functions from the NIST Atomic Spectra Database
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `nist_levels()` retrieves the energy levels of an atom or ion from the
#' NIST Atomic Spectra Database, and `partition_function()` computes the
#' internal partition function of a species from its levels, as needed by
#' [cf_libs()].
#'
#' @details
#' All the levels listed by the ASD are returned, including the
#' autoionizing levels above the first ionization limit, as in the partition
#' functions computed by the ASD; levels without a statistical weight are
#' dropped. The partition function
#' \deqn{U(T) = \sum_i g_i \exp\left(-\frac{E_i}{k_B T}\right)}
#' is summed over the levels given, up to `max_energy`. In a plasma, the
#' ionization energy is lowered by the surrounding charges, and the levels
#' above the lowered limit do not exist: truncating the sum there matters
#' for atoms with many high Rydberg levels close to the limit, such as the
#' alkali metals (see [cf_libs()], which does it). Without truncation, the
#' result equals the partition function computed by the ASD.
#'
#' The results of a query are cached for the R session. Cite the database
#' when you use these data (see [nist_lines()]).
#'
#' @param species A character vector of species in spectroscopic notation,
#'   such as `"Ca I"`.
#' @param timeout The download timeout, in seconds. Default is 120.
#'
#' @return `nist_levels()`: a tibble with one row per level and columns
#'   `species`, `configuration`, `term`, `J`, `g` (statistical weight) and
#'   `energy` (eV).
#'
#' @seealso [cf_libs()], [nist_lines()], [nist_ionization_energy()]
#' @export nist_levels
#'
#' @examples
#' \donttest{
#' # needs an internet connection
#' ca <- try(nist_levels("Ca I"))
#' if (!inherits(ca, "try-error")) partition_function(ca, temperature = c(8000, 10000))
#' }
nist_levels <- function(species, timeout = 120) {
  if (!is.character(species) || length(species) == 0) {
    stop("'species' must be a character vector, such as \"Ca I\".")
  }
  check_number(timeout, "timeout", lower = 0, lower_open = TRUE)
  tables <- lapply(species, function(s) {
    sp <- parse_species(s)
    key <- paste("levels", sp$label)
    table <- nist_cache[[key]]
    if (is.null(table)) {
      query <- list(de = 0, spectrum = sp$label, submit = "Retrieve Data", units = 1, format = 2,
                    output = 0, page_size = 15, multiplet_ordered = 0, conf_out = "on",
                    term_out = "on", level_out = "on", unc_out = 1, j_out = "on", g_out = "on",
                    lande_out = "on", perc_out = "on", biblio = "on", splitting = 1, temp = "")
      text <- nist_download("https://physics.nist.gov/cgi-bin/ASD/energy1.pl", query, sp$label,
                            timeout)
      table <- parse_nist_levels(text, sp$label)
      assign(key, table, envir = nist_cache)
    }
    table
  })
  do.call(rbind, tables)
}

#' @rdname nist_levels
#' @param levels A data frame of levels with columns `species`, `g` and
#'   `energy` (eV), such as returned by `nist_levels()`.
#' @param temperature The temperature(s), in K.
#' @param max_energy The highest level energy included, in eV: one value, or
#'   a vector named by species. Default is `Inf` (all levels).
#' @return `partition_function()`: a tibble with the `species`, the
#'   `temperature` and the `partition` function, for each species and
#'   temperature.
#' @export partition_function
partition_function <- function(levels, temperature, max_energy = Inf) {
  if (!is.data.frame(levels) || !all(c("species", "g", "energy") %in% names(levels))) {
    stop("'levels' must be a data frame with columns species, g and energy (eV).", call. = FALSE)
  }
  if (!is.numeric(temperature) || length(temperature) == 0 || anyNA(temperature) ||
      any(temperature <= 0)) {
    stop("'temperature' must contain positive values (K).", call. = FALSE)
  }
  ok <- is.finite(levels$g) & is.finite(levels$energy)
  species <- unique(levels$species)
  if (!is.numeric(max_energy) || anyNA(max_energy) || length(max_energy) == 0) {
    stop("'max_energy' must be numeric (eV).", call. = FALSE)
  }
  cut <- if (is.null(names(max_energy))) {
    stats::setNames(rep_len(max_energy, length(species)), species)
  } else {
    stats::setNames(ifelse(species %in% names(max_energy), max_energy[species], Inf), species)
  }
  grid <- expand.grid(temperature = temperature, species = species, stringsAsFactors = FALSE)
  grid$partition <- mapply(function(sp, t) {
    keep <- ok & levels$species == sp & levels$energy <= cut[[sp]]
    sum(levels$g[keep] * exp(-levels$energy[keep] / (k_boltzmann_ev * t)))
  }, grid$species, grid$temperature)
  tibble::tibble(species = grid$species, temperature = grid$temperature,
                 partition = grid$partition)
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

parse_nist_levels <- function(text, label) {
  text <- text[!grepl("^\\s*Partition function", text)]
  df <- read_nist_csv(text)
  column <- function(pattern) {
    hit <- grep(pattern, names(df))
    if (length(hit) == 0) rep(NA_character_, nrow(df)) else df[[hit[1]]]
  }
  term <- column("^Term")
  bound <- which(term != "Limit")   # the ionization limits are listed as rows too
  out <- tibble::tibble(
    species = label,
    configuration = column("^Configuration")[bound],
    term = term[bound],
    J = column("^J")[bound],
    g = nist_number(column("^g")[bound]),
    energy = nist_number(column("^Level")[bound])
  )
  out <- out[is.finite(out$g) & is.finite(out$energy), , drop = FALSE]
  if (nrow(out) == 0) {
    stop("No energy levels for ", label, " in the NIST database.", call. = FALSE)
  }
  out
}
