#' @title Stark Widths from VALD3 and Kurucz Line Lists
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Reads the quadratic Stark damping constants of a line list saved from the
#' Vienna Atomic Line Database (VALD3) or from the line lists of R. L.
#' Kurucz, as a table of Stark widths in the format of [starkb_lines()], for
#' [stark_width()] and [electron_density()]. These line lists give a width
#' for many lines that STARK-B does not have, but a cruder one.
#'
#' @details
#' **Files.** The data of VALD3 are free, but need a registration: request
#' an "Extract All", "Extract Element" or "Extract Stellar" list from the
#' VALD website or its email service, in the short or the long format, with
#' any wavelength unit (Å, nm or cm\eqn{^{-1}}), medium and energy unit, and
#' give the saved file to `read_vald()`. The long format also gives the
#' designations of the levels (`lower` and `upper`). The line lists of
#' Kurucz are free to download from `kurucz.harvard.edu`: the whole list
#' (`gfall*.dat`, in air above 200 nm, or `gfallvac*.dat`, in vacuum) or
#' the list of one ion (such as `atoms/2001/gf2001.pos` for Ca II), in the
#' 160-column format of these files. The wavenumber files (`gfallwn*.dat`)
#' are not read. The wavelengths are kept in the medium of the file: in
#' air, they match those of [nist_lines()] above 200 nm.
#'
#' **Widths.** Both line lists give, for each line, the decimal logarithm
#' of the quadratic Stark damping constant \eqn{\gamma} at 10 000 K per
#' electron: the full width at half maximum of the Lorentzian profile in
#' angular frequency (rad s\eqn{^{-1}}), divided by the electron density
#' (cm\eqn{^{-3}}). The full width at half maximum in wavelength is
#' \deqn{w = \frac{\lambda^2}{2 \pi c} \gamma N_e}
#' It is tabulated at the density \eqn{10^{17}} cm\eqn{^{-3}} (the width is
#' proportional to it) and at each of `temperature`, scaled as
#' \eqn{(T / 10^4\,\mathrm{K})^{1/6}}, the temperature dependence of the
#' quadratic Stark width in the impact approximation (Gray, 2005). The
#' shift is not given: it is set to zero. Lines without a Stark constant
#' (zero in the file) are left out, and a line listed several times (as
#' its isotopic or hyperfine components) is kept once.
#'
#' **Accuracy.** Most of these constants come from approximate formulas
#' rather than from calculations such as those of STARK-B, and the
#' temperature law is approximate too: prefer STARK-B ([starkb_lines()]) or
#' measured widths ([stark_table()]) when they exist, and use these for the
#' other lines. A table of several species needs `species` in
#' [stark_width()] and [electron_density()].
#'
#' When you use these data, cite the line list and the references of the
#' constants, given in the `source` column (the reference codes of VALD are
#' described in the BibTeX file that comes with each extract).
#'
#' @param file The path to the line list: an extract saved from VALD3 for
#'   `read_vald()`, a Kurucz line list for `read_kurucz()`.
#' @param species An optional character vector of the species to keep, in
#'   spectroscopic notation, such as `c("Ca II", "Mg I")`. By default all
#'   the species of the file are kept.
#' @param wavelength An optional numeric vector of length 2: the wavelength
#'   range to keep, in nm.
#' @param temperature The temperatures (K) at which the widths are
#'   tabulated. Default is 2500 to 40 000 K; [stark_width()] interpolates
#'   between them.
#'
#' @return A tibble in the format of [starkb_lines()], with one row per
#'   line and temperature: `species`, `wavelength` (nm), `upper` and
#'   `lower` (the level designations, empty when the file does not give
#'   them), `perturber` (`"electron"`), `temperature`, `density`
#'   (\eqn{10^{17}} cm\eqn{^{-3}}), `width` (nm), `shift` (0) and `source`
#'   (the line list, the logarithm of the constant and its references).
#'
#' @references
#'  - Ryabchikova, T., Piskunov, N., Kurucz, R.L., Stempels, H.C., Heiter,
#'    U., Pakhomov, Yu., Barklem, P.S. (2015). A major upgrade of the VALD
#'    database. Physica Scripta, 90(5):054005.
#'    \doi{10.1088/0031-8949/90/5/054005}
#'  - Kurucz, R.L. Atomic line lists, `kurucz.harvard.edu/linelists.html`.
#'  - Gray, D.F. (2005). The Observation and Analysis of Stellar
#'    Photospheres, 3rd edition. Cambridge University Press.
#'
#' @seealso [starkb_lines()], [stark_table()], [stark_width()],
#'   [electron_density()]
#' @name read_vald
#' @export read_vald
#'
#' @examples
#' # two lines of a VALD3 "Extract Element" list, short format (made-up
#' # references)
#' file <- tempfile(fileext = ".txt")
#' writeLines(c(
#'   "                                            Damping parameters    Lande",
#'   "Elm Ion       WL_air(A) Excit(eV) log(gf)   Rad.  Stark    Waals  factor References",
#'   "'Ca 2',       3933.6630,   0.0000,  0.135, 8.170,-6.270,  -7.760, 1.170,'   1 K10   Ca+  '",
#'   "'Ca 2',       3968.4690,   0.0000, -0.180, 8.160,-6.270,  -7.760, 1.330,'   1 K10   Ca+  '"
#' ), file)
#' ca <- read_vald(file)
#' ca
#' stark_width(ca, wavelength = 393.366, temperature = 9000, density = 5e16)
#'
#' # the same line in the 160-column format of the Kurucz line lists
#' kurucz <- tempfile(fileext = ".pos")
#' writeLines(paste0(
#'   "   393.3663  0.111 20.01       0.000  0.5 4s 2S        25414.400  1.5 4p 2P",
#'   "       8.17 -6.27 -7.76K10 "
#' ), kurucz)
#' read_kurucz(kurucz, temperature = c(5000, 20000))
read_vald <- function(file, species = NULL, wavelength = NULL,
                      temperature = c(2500, 5000, 10000, 20000, 40000)) {
  check_line_list(file, temperature)
  keep <- line_list_species(species)
  window <- wavelength_window(wavelength)
  text <- readLines(file, warn = FALSE)
  header <- grep("(WL|WN)_?(air|vac)?\\([^)]*\\)", text)
  header <- header[grepl("Stark", text[header], fixed = TRUE)]
  if (length(header) == 0) {
    stop("'file' is not a VALD3 extract: no column header with the wavelength and 'Stark'.",
         call. = FALSE)
  }
  unit <- regmatches(text[header[1]], regexpr("(WL|WN)_?(air|vac)?\\([^)]*\\)", text[header[1]]))
  rows <- grep("^'[A-Z][A-Za-z0-9]* +[0-9]+',", text)
  if (length(rows) == 0) {
    return(no_stark_lines(file))
  }
  fields <- utils::read.csv(text = text[rows], header = FALSE, quote = "'", strip.white = TRUE,
                            stringsAsFactors = FALSE, colClasses = "character")
  numbers <- suppressWarnings(lapply(fields[-1], as.numeric))
  # the numbers come first, then the references (short formats) or an empty
  # field after a trailing comma
  is_number <- vapply(seq_along(numbers), function(j) !all(is.na(numbers[[j]])), logical(1))
  first_text <- which(!is_number)[1]
  n_numbers <- if (is.na(first_text)) length(numbers) else first_text - 1L
  # the position of the Stark constant among the numbers: short Extract All
  # or Element (7 numbers), short Extract Stellar (9), long formats (12, or
  # 13 with the central depth)
  stark_at <- c(`7` = 5L, `9` = 6L, `12` = 11L, `13` = 11L)[as.character(n_numbers)]
  if (is.na(stark_at)) {
    stop("'file' has an unknown VALD3 format (", n_numbers, " numbers per line).", call. = FALSE)
  }
  long <- n_numbers >= 12
  ion <- strsplit(fields[[1]], " +")
  element <- vapply(ion, `[`, character(1), 1)
  charge <- as.integer(vapply(ion, `[`, character(1), 2)) - 1L
  references <- if (long) {
    text[pmin(rows + 3L, length(text))]
  } else if (ncol(fields) >= n_numbers + 2) {
    fields[[n_numbers + 2]]
  } else {
    rep("", length(rows))
  }
  stark_from_damping(
    element = element, charge = charge,
    wavelength = vald_wavelength(numbers[[1]], unit),
    log_gamma = numbers[[stark_at]],
    lower = if (long) vald_level(text[rows + 1L]) else "",
    upper = if (long) vald_level(text[rows + 2L]) else "",
    source = paste0("VALD3", reference_codes(references)),
    keep = keep, window = window, temperature = temperature, file = file
  )
}

#' @rdname read_vald
#' @export read_kurucz
read_kurucz <- function(file, species = NULL, wavelength = NULL,
                        temperature = c(2500, 5000, 10000, 20000, 40000)) {
  check_line_list(file, temperature)
  keep <- line_list_species(species)
  window <- wavelength_window(wavelength)
  text <- readLines(file, warn = FALSE)
  text <- text[nchar(text) >= 92]
  code <- suppressWarnings(as.numeric(substr(text, 19, 24)))
  wl <- suppressWarnings(as.numeric(substr(text, 1, 11)))
  if (length(text) == 0 || all(is.na(code) | is.na(wl))) {
    stop("'file' is not a Kurucz line list in the 160-column format.", call. = FALSE)
  }
  # element number and charge: 20.01 is Ca II
  z <- floor(code + 1e-6)
  charge <- as.integer(round(100 * (code - z)))
  symbols <- periodic_table()$symbol
  element <- symbols[match(z, seq_along(symbols))]
  # the lower level has the lower energy (negative energies are predicted
  # levels)
  e1 <- abs(suppressWarnings(as.numeric(substr(text, 25, 36))))
  e2 <- abs(suppressWarnings(as.numeric(substr(text, 53, 64))))
  label1 <- squish(substr(text, 43, 52))
  label2 <- squish(substr(text, 71, 80))
  first_lower <- is.na(e2) | (!is.na(e1) & e1 <= e2)
  stark_from_damping(
    element = element, charge = charge, wavelength = wl,
    log_gamma = suppressWarnings(as.numeric(substr(text, 87, 92))),
    lower = ifelse(first_lower, label1, label2),
    upper = ifelse(first_lower, label2, label1),
    source = paste0("Kurucz", ifelse(nzchar(squish(substr(text, 99, 102))),
                                     paste0(" (", squish(substr(text, 99, 102)), ")"), "")),
    keep = keep, window = window, temperature = temperature, file = file
  )
}

# ---- internals ---------------------------------------------------------------

speed_of_light <- 299792458

check_line_list <- function(file, temperature) {
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    stop("'file' must be the path to an existing line list.", call. = FALSE)
  }
  if (!is.numeric(temperature) || length(temperature) == 0 || anyNA(temperature) ||
      any(temperature <= 0)) {
    stop("'temperature' must contain positive values (K).", call. = FALSE)
  }
  invisible(TRUE)
}

# The labels of the species to keep, or NULL for all.
line_list_species <- function(species) {
  if (is.null(species)) {
    return(NULL)
  }
  if (!is.character(species) || length(species) == 0) {
    stop("'species' must be a character vector, such as c(\"Ca II\", \"Mg I\").", call. = FALSE)
  }
  vapply(species, function(s) parse_species(s)$label, character(1), USE.NAMES = FALSE)
}

squish <- function(x) gsub("\\s+", " ", trimws(x))

# Wavelengths of a VALD3 extract in nm, from the unit of its header, such as
# "WL_air(A)", "WL_vac(nm)" or a wavenumber in cm^-1.
vald_wavelength <- function(value, unit) {
  if (grepl("cm", unit, fixed = TRUE)) {
    1e7 / value
  } else if (grepl("(nm)", unit, fixed = TRUE)) {
    value
  } else {
    value / 10
  }
}

# The designation of a level in the long format of VALD3: the line without
# its quotes and coupling scheme ("LS", "JK", ...).
vald_level <- function(lines) {
  x <- squish(gsub("'", "", lines, fixed = TRUE))
  squish(sub("^[A-Z][A-Z0-9]{0,3} ", "", x))
}

# The distinct reference codes of VALD3 reference strings, as " (K10, RU)".
reference_codes <- function(references) {
  vapply(references, function(r) {
    codes <- regmatches(r, gregexpr(" [0-9]+ +[A-Za-z][^ ']*", r))[[1]]
    codes <- unique(sub("^[a-z]+:", "", sub("^ [0-9]+ +", "", codes)))
    if (length(codes) == 0) "" else paste0(" (", paste(codes, collapse = ", "), ")")
  }, character(1), USE.NAMES = FALSE)
}

# The table of Stark widths of the lines with a Stark constant (log10 of the
# FWHM in angular frequency per electron at 10^4 K), at 10^17 cm^-3 and at
# each temperature.
stark_from_damping <- function(element, charge, wavelength, log_gamma, lower, upper, source,
                               keep, window, temperature, file) {
  n <- length(wavelength)
  lines <- data.frame(element = element, charge = charge, wavelength = wavelength,
                      log_gamma = log_gamma, lower = rep_len(lower, n),
                      upper = rep_len(upper, n), source = source, stringsAsFactors = FALSE)
  ok <- !is.na(lines$element) & !is.na(lines$charge) & lines$charge >= 0 &
    lines$charge < length(roman_numerals) & !is.na(lines$wavelength) & lines$wavelength > 0 &
    !is.na(lines$log_gamma) & lines$log_gamma != 0
  lines <- lines[ok, , drop = FALSE]
  lines$species <- species_label(lines$element, lines$charge)
  if (!is.null(keep)) lines <- lines[lines$species %in% keep, , drop = FALSE]
  lines <- lines[in_window(lines$wavelength, window), , drop = FALSE]
  lines <- lines[!duplicated(lines[c("species", "wavelength", "lower", "upper")]), , drop = FALSE]
  if (nrow(lines) == 0) {
    return(no_stark_lines(file))
  }
  # FWHM in nm at 10^17 cm^-3 and 10^4 K
  width <- (lines$wavelength * 1e-9)^2 / (2 * pi * speed_of_light) * 10^lines$log_gamma * 1e17 * 1e9
  temperature <- sort(unique(temperature))
  row <- rep(seq_len(nrow(lines)), each = length(temperature))
  t_row <- rep(temperature, nrow(lines))
  out <- tibble::tibble(
    species = lines$species[row],
    wavelength = lines$wavelength[row],
    upper = lines$upper[row],
    lower = lines$lower[row],
    perturber = "electron",
    temperature = t_row,
    density = 1e17,
    width = width[row] * (t_row / 1e4)^(1 / 6),
    shift = 0,
    source = paste0(lines$source[row], ", log gamma_Stark ",
                    formatC(lines$log_gamma[row], format = "f", digits = 2), " at 10000 K")
  )
  out[order(out$wavelength, out$species, out$temperature), ]
}

no_stark_lines <- function(file) {
  warning("No line with a Stark constant in '", basename(file),
          "' (for the species and wavelength range given).", call. = FALSE)
  empty_stark_table()
}
