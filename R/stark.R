#' @title Stark Broadening Parameters from the STARK-B Database
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Retrieves the Stark broadening parameters (full widths at half maximum and
#' shifts) of the lines of an atom or ion from STARK-B, the database of Stark
#' broadening parameters of isolated lines of atoms and ions in the impact
#' approximation (Sahal-Bréchot, Dimitrijević and Moreau, Observatoire de
#' Paris and Astronomical Observatory of Belgrade). The data are downloaded
#' on demand from the database's Virtual Atomic and Molecular Data Centre
#' (VAMDC) service; they are not distributed with specProc.
#'
#' @details
#' For each transition, STARK-B tabulates the Stark width \eqn{w} (full
#' width at half maximum) and shift \eqn{d} for collisions with electrons and
#' with ions (protons, singly ionized helium, ...), at several temperatures
#' and perturber densities. In the impact approximation, the width and shift
#' are proportional to the perturber density. Many entries are multiplets,
#' given at the mean wavelength of the multiplet; the `upper` and `lower`
#' configurations and terms identify them.
#'
#' The results of a query are cached for the R session, so repeated calls
#' with the same `species` do not download the data again.
#'
#' When you use these data, cite the STARK-B database and the original
#' publications, listed in the `source` column:
#'  - Sahal-Bréchot, S., Dimitrijević, M.S., Moreau, N. STARK-B database,
#'    `stark-b.obspm.fr`. Observatoire de Paris and Astronomical Observatory
#'    of Belgrade (see the References).
#'
#' @param species A character string naming the emitter in spectroscopic
#'   notation, such as `"Ca II"` (singly ionized calcium) or `"Na I"`
#'   (neutral sodium).
#' @param wavelength An optional numeric vector of length 2: the wavelength
#'   range to keep, in nm.
#' @param perturber An optional character vector of perturbers to keep, such
#'   as `"electron"`, `"H II"` (protons) or `"He II"`. By default all are
#'   kept.
#' @param timeout The download timeout, in seconds. Default is 120.
#'
#' @return A tibble with one row per transition, perturber, temperature and
#'   density, and columns:
#'  - `species`, `wavelength` (nm, as given by STARK-B), `upper`, `lower`
#'    (configuration and term of the upper and lower levels);
#'  - `perturber`, `temperature` (K), `density` (perturber density,
#'    cm\eqn{^{-3}});
#'  - `width` (Stark full width at half maximum, nm) and `shift` (nm;
#'    positive towards the red);
#'  - `source`: the publications the data come from.
#'
#' @references
#'  - Sahal-Bréchot, S., Dimitrijević, M.S., Moreau, N., Ben Nessib, N.
#'    (2015). The STARK-B database VAMDC node: a repository for spectral
#'    line broadening and shifts due to collisions with charged particles.
#'    Physica Scripta, 90(5):054008. \doi{10.1088/0031-8949/90/5/054008}
#'
#' @seealso [stark_width()] to interpolate the width of a line at a given
#'   temperature and density, [electron_density()], [read_starkb()] for
#'   saved STARK-B files, and [stark_table()] for data from other sources.
#'
#' @export starkb_lines
#'
#' @examples
#' \donttest{
#' # needs an internet connection
#' ca <- try(starkb_lines("Ca II", wavelength = c(390, 400), perturber = "electron"))
#' if (!inherits(ca, "try-error")) head(ca)
#' }
starkb_lines <- function(species, wavelength = NULL, perturber = NULL, timeout = 120) {
  sp <- parse_species(species)
  check_wavelength_range(wavelength)
  check_number(timeout, "timeout", lower = 0, lower_open = TRUE)
  key <- paste(sp$element, sp$charge)
  lines <- starkb_cache[[key]]
  if (is.null(lines)) {
    rlang::check_installed("xml2", reason = "to read STARK-B data.")
    file <- starkb_download(sp, timeout)
    on.exit(unlink(file), add = TRUE)
    lines <- parse_xsams(file)
    assign(key, lines, envir = starkb_cache)
  }
  filter_stark(lines, wavelength, perturber)
}

#' @title Read Saved STARK-B Data
#'
#' @description
#' Reads Stark broadening parameters from an XSAMS file of the STARK-B
#' database saved earlier: for example, the result of a query to the
#' STARK-B VAMDC service downloaded in a web browser or through the VAMDC
#' portal. This gives the same table as [starkb_lines()] without an internet
#' connection, and makes an analysis reproducible with archived data.
#'
#' @details
#' A query for one species can be downloaded from
#' `http://stark-b.obspm.fr/12.07/vamdc/tap/sync?LANG=VSS2&REQUEST=doQuery&FORMAT=XSAMS&QUERY=select * where (atomsymbol = 'Ca' and ioncharge = 1)`
#' (with the element and the ion charge, 0 for neutral atoms, adapted).
#' Cite the STARK-B database and the original publications, listed in the
#' `source` column, as for [starkb_lines()].
#'
#' @param file The path to an XSAMS (XML) file from STARK-B.
#' @inheritParams starkb_lines
#'
#' @return A tibble as returned by [starkb_lines()].
#'
#' @seealso [starkb_lines()], [stark_table()] for data from other sources.
#' @export read_starkb
#'
#' @examplesIf rlang::is_installed("xml2")
#' # a minimal file in the STARK-B format, with made-up values
#' file <- tempfile(fileext = ".xml")
#' writeLines(c(
#'   '<XSAMSData xmlns="http://vamdc.org/xml/xsams/1.0">',
#'   '<Sources><Source sourceID="B1"><Title>Example</Title><Year>2000</Year></Source></Sources>',
#'   '<Environments><Environment envID="E1"><Temperature><Value units="K">10000</Value>',
#'   '</Temperature><TotalNumberDensity><Value units="1/cm3">1e17</Value></TotalNumberDensity>',
#'   '<Composition><Species name="electron" speciesRef="XP1"/></Composition></Environment>',
#'   '</Environments><Species><Atoms><Atom><ChemicalElement><ElementSymbol>Ca</ElementSymbol>',
#'   '</ChemicalElement><Isotope><Ion speciesID="X1"><IonCharge>1</IonCharge></Ion></Isotope>',
#'   '</Atom></Atoms></Species><Processes><Radiative><RadiativeTransition id="P1">',
#'   '<SourceRef>B1</SourceRef><EnergyWavelength><Wavelength><Value units="A">3950</Value>',
#'   '</Wavelength></EnergyWavelength><SpeciesRef>X1</SpeciesRef>',
#'   '<Broadening name="pressure" envRef="E1"><Lineshape name="Lorentzian">',
#'   '<LineshapeParameter name="gammaL"><Value units="A">0.2</Value></LineshapeParameter>',
#'   '</Lineshape></Broadening></RadiativeTransition></Radiative></Processes></XSAMSData>'
#' ), file)
#' read_starkb(file)
read_starkb <- function(file, wavelength = NULL, perturber = NULL) {
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    stop("'file' must be the path to an existing XSAMS file.")
  }
  check_wavelength_range(wavelength)
  rlang::check_installed("xml2", reason = "to read STARK-B data.")
  lines <- tryCatch(parse_xsams(file), error = function(e) {
    stop("Could not read '", file, "' as STARK-B XSAMS data: ", conditionMessage(e), call. = FALSE)
  })
  filter_stark(lines, wavelength, perturber)
}

#' @title Build a Table of Stark Broadening Parameters
#'
#' @description
#' Builds a table of Stark widths and shifts from user-supplied values, in
#' the format of [starkb_lines()], so that [stark_width()] and
#' [electron_density()] can use Stark parameters from any source:
#' values copied from the STARK-B website, from a publication, or from the
#' fitted temperature laws that STARK-B provides.
#'
#' @details
#' Give either the tabulated `width` at each `temperature`, or the
#' coefficients of a fitted temperature law. STARK-B fits the widths and
#' shifts at a given perturber density as
#' \deqn{\log w = a_0 + a_1 \log T + a_2 (\log T)^2}
#' \deqn{d / w = b_0 + b_1 \log T + b_2 (\log T)^2}
#' with decimal logarithms and \eqn{w} in the units of its tables (Å); the
#' fits are only valid within the tabulated temperature range
#' (Sahal-Bréchot, Dimitrijević and Ben Nessib, 2011). With
#' `coefficients`, the table is evaluated at `temperature`, which should
#' span that range. Check the base of the logarithm and the units of any
#' other source.
#'
#' `units` is required, because Stark widths are reported in both Å and nm,
#' and a wrong guess changes the electron density tenfold. It applies to
#' `wavelength`, `width` and `shift` (and to the fitted widths); the table
#' is returned in nm.
#'
#' Arguments of length 1 are recycled. Tables for several lines or
#' perturbers can be combined with `rbind()` or [dplyr::bind_rows()].
#'
#' @param wavelength The wavelength of the line (or multiplet), in `units`.
#' @param temperature The temperatures, in K.
#' @param width The Stark full widths at half maximum at each temperature, in
#'   `units`. Leave `NULL` when `coefficients` are given.
#' @param units The units of `wavelength`, `width` and `shift`: `"nm"` or
#'   `"A"` (Ångström). Required.
#' @param density The perturber density of the widths, in cm\eqn{^{-3}}.
#'   Default is \eqn{10^{17}}.
#' @param shift The Stark shifts, in `units`. Default is 0 (unknown).
#' @param perturber The perturber. Default is `"electron"`.
#' @param species,upper,lower Optional labels of the emitter and of the upper
#'   and lower levels.
#' @param coefficients Optional coefficients \eqn{(a_0, a_1, a_2)} of the
#'   fitted width law.
#' @param shift_coefficients Optional coefficients \eqn{(b_0, b_1, b_2)} of
#'   the fitted law for the ratio \eqn{d/w}; used with `coefficients`.
#' @param log_base The base of the logarithms of the fitted laws. Default is
#'   10.
#' @param source A description of where the data come from.
#'
#' @return A tibble with the columns of [starkb_lines()].
#'
#' @references
#'  - Sahal-Bréchot, S., Dimitrijević, M.S., Ben Nessib, N. (2011). Widths
#'    and shifts of isolated lines of neutral and ionized atoms perturbed by
#'    collisions with electrons and ions: an outline of the semiclassical
#'    perturbation (SCP) method and of the approximations used for the
#'    calculations. Baltic Astronomy, 20:523-530.
#'
#' @seealso [stark_width()], [electron_density()], [read_starkb()]
#' @export stark_table
#'
#' @examples
#' # made-up widths of a hypothetical line, in Angstrom at 1e17 cm-3
#' x <- stark_table(
#'   wavelength = 5000, temperature = c(5000, 10000, 20000),
#'   width = c(0.20, 0.16, 0.13), units = "A", species = "X II"
#' )
#' x
#' electron_density(0.012, stark = x, wavelength = 500, temperature = 8000)
#'
#' # the same line from the coefficients of a fitted temperature law
#' # log10(w) = a0 + a1 log10(T) + a2 log10(T)^2 (here fitted to the table above)
#' lt <- log10(c(5000, 10000, 20000))
#' a <- unname(coef(lm(log10(c(0.20, 0.16, 0.13)) ~ lt + I(lt^2))))
#' stark_table(wavelength = 5000, temperature = c(5000, 10000, 20000),
#'             coefficients = a, units = "A")
stark_table <- function(wavelength, temperature, width = NULL, units, density = 1e17,
                        shift = 0, perturber = "electron", species = NA_character_,
                        upper = "", lower = "", coefficients = NULL,
                        shift_coefficients = NULL, log_base = 10, source = "user-supplied") {
  if (missing(units)) {
    stop("'units' is required: \"nm\" or \"A\" (the units of 'wavelength', 'width' and 'shift').",
         call. = FALSE)
  }
  units <- match.arg(units, c("nm", "A"))
  to_nm <- if (units == "A") 0.1 else 1
  positive <- function(v, arg) {
    if (!is.numeric(v) || length(v) == 0 || anyNA(v) || any(!is.finite(v)) || any(v <= 0)) {
      stop("'", arg, "' must contain positive numbers.", call. = FALSE)
    }
  }
  positive(wavelength, "wavelength")
  positive(temperature, "temperature")
  positive(density, "density")

  if (is.null(coefficients)) {
    if (is.null(width)) {
      stop("Give either 'width' or 'coefficients'.", call. = FALSE)
    }
    positive(width, "width")
  } else {
    if (!is.null(width)) {
      stop("Give either 'width' or 'coefficients', not both.", call. = FALSE)
    }
    fitted_law <- function(b, arg) {
      if (!is.numeric(b) || length(b) != 3 || anyNA(b)) {
        stop("'", arg, "' must be a numeric vector of length 3.", call. = FALSE)
      }
      lt <- log(temperature, base = log_base)
      b[1] + b[2] * lt + b[3] * lt^2
    }
    check_number(log_base, "log_base", lower = 1, lower_open = TRUE)
    width <- log_base^fitted_law(coefficients, "coefficients")
    if (!is.null(shift_coefficients)) {
      shift <- fitted_law(shift_coefficients, "shift_coefficients") * width
    }
  }
  if (!is.numeric(shift) || anyNA(shift)) {
    stop("'shift' must contain numbers.", call. = FALSE)
  }

  columns <- list(wavelength = wavelength, temperature = temperature, width = width,
                  density = density, shift = shift, perturber = perturber, species = species,
                  upper = upper, lower = lower, source = source)
  n <- max(lengths(columns))
  bad <- names(columns)[!lengths(columns) %in% c(1, n)]
  if (length(bad) > 0) {
    stop("Arguments must have length 1 or ", n, ": ", paste(bad, collapse = ", "), ".",
         call. = FALSE)
  }
  columns <- lapply(columns, rep_len, length.out = n)
  tibble::tibble(
    species = as.character(columns$species),
    wavelength = columns$wavelength * to_nm,
    upper = as.character(columns$upper),
    lower = as.character(columns$lower),
    perturber = as.character(columns$perturber),
    temperature = columns$temperature,
    density = columns$density,
    width = columns$width * to_nm,
    shift = columns$shift * to_nm,
    source = as.character(columns$source)
  )
}

#' @title Stark Width of a Line at Given Plasma Conditions
#'
#' @description
#' Computes the Stark width (and shift) of a line at a given temperature and
#' electron density, from the Stark broadening parameters returned by
#' [starkb_lines()].
#'
#' @details
#' The transition whose wavelength is closest to `wavelength` is used; its
#' wavelength must lie within `tolerance` nm. STARK-B often tabulates a
#' multiplet at its mean wavelength \eqn{\lambda_m}; the width and shift of a
#' line of the multiplet at \eqn{\lambda} are then
#' \eqn{w_\lambda = w_m \lambda^2 / \lambda_m^2} (and likewise for the shift),
#' as the database recommends. This scaling is applied to every match. Within the impact
#' approximation, the width and shift are proportional to the perturber
#' density, so the tabulated values at the density closest to `density` are
#' scaled linearly to `density`. Between the tabulated temperatures, the
#' width is interpolated linearly in \eqn{\log w} against \eqn{\log T}, and
#' the shift linearly against \eqn{\log T}; temperatures outside the
#' tabulated range give an error.
#'
#' @param stark A tibble returned by [starkb_lines()].
#' @param wavelength The wavelength of the line, in nm.
#' @param temperature The temperature, in K (a vector is allowed).
#' @param density The perturber (electron) density, in cm\eqn{^{-3}}.
#'   Default is \eqn{10^{17}}.
#' @param perturber The perturber. Default is `"electron"`.
#' @param tolerance The largest allowed difference between `wavelength` and
#'   the tabulated wavelength, in nm. Default is 0.5.
#'
#' @return A tibble with one row per temperature and columns `wavelength`
#'   (the requested wavelength, nm), `tabulated_wavelength`, `upper`,
#'   `lower`, `perturber`, `temperature`, `density`, `width` (FWHM, nm) and
#'   `shift` (nm).
#'
#' @seealso [starkb_lines()], [electron_density()]
#' @export stark_width
#'
#' @examples
#' # A table in the format of starkb_lines(), for illustration
#' stark <- data.frame(
#'   species = "X II", wavelength = 500, upper = "u", lower = "l",
#'   perturber = "electron", temperature = c(5000, 10000, 20000),
#'   density = 1e17, width = c(0.020, 0.014, 0.010), shift = 0, source = ""
#' )
#' stark_width(stark, wavelength = 500, temperature = 8000, density = 5e16)
stark_width <- function(stark, wavelength, temperature, density = 1e17,
                        perturber = "electron", tolerance = 0.5) {
  check_stark_table(stark)
  check_number(wavelength, "wavelength", lower = 0, lower_open = TRUE)
  check_number(density, "density", lower = 0, lower_open = TRUE)
  check_number(tolerance, "tolerance", lower = 0)
  if (!is.numeric(temperature) || anyNA(temperature) || any(temperature <= 0)) {
    stop("'temperature' must contain positive values (K).")
  }
  rows <- stark[stark$perturber == perturber, ]
  if (nrow(rows) == 0) {
    stop("No data for the perturber '", perturber, "'.")
  }
  gap <- abs(rows$wavelength - wavelength)
  if (min(gap) > tolerance) {
    stop("No tabulated line within ", tolerance, " nm of ", wavelength, " nm; the closest is at ",
         format(rows$wavelength[which.min(gap)]), " nm.")
  }
  best <- which.min(gap)
  line <- rows[same_value(rows$wavelength, rows$wavelength[best]) &
                 same_value(rows$upper, rows$upper[best]) &
                 same_value(rows$lower, rows$lower[best]), ]
  # tabulated density closest to the requested one (log scale)
  densities <- unique(line$density)
  ref <- densities[which.min(abs(log(densities) - log(density)))]
  line <- line[line$density == ref, ]
  line <- line[order(line$temperature), ]
  if (any(temperature < min(line$temperature) | temperature > max(line$temperature))) {
    stop("'temperature' must lie within the tabulated range, ",
         min(line$temperature), " to ", max(line$temperature), " K.")
  }
  scale <- (density / ref) * (wavelength / line$wavelength[1])^2
  logt <- log(line$temperature)
  width <- if (nrow(line) == 1) rep(line$width, length(temperature)) else
    exp(stats::approx(logt, log(line$width), xout = log(temperature))$y)
  shift <- if (nrow(line) == 1) rep(line$shift, length(temperature)) else
    stats::approx(logt, line$shift, xout = log(temperature))$y
  tibble::tibble(
    wavelength = wavelength, tabulated_wavelength = line$wavelength[1],
    upper = line$upper[1], lower = line$lower[1],
    perturber = perturber, temperature = temperature, density = density,
    width = width * scale, shift = shift * scale
  )
}

#' @title Electron Density from Stark Broadening
#'
#' @description
#' Estimates the electron density of a plasma from the Stark width of an
#' emission line: from the Stark broadening parameters of the line
#' (STARK-B, or a reference width), or from the width of the hydrogen
#' H\eqn{\alpha} line at 656.28 nm.
#'
#' @details
#' `width` is the Stark full width at half maximum, in nm, with the
#' instrumental and Doppler broadening removed. The line must be optically
#' thin: self-absorption broadens lines and overestimates the density (see
#' [self_absorption()]).
#'
#' - For isolated lines (`method = "stark"`), the Stark profile is
#'   Lorentzian: use the Lorentzian width `wL` of a Voigt fit with
#'   [peak_fit()] or [multipeak_fit()], whose Gaussian part absorbs the
#'   instrumental and Doppler broadening.
#' - The Stark profile of H\eqn{\alpha} (`method = "halpha"`) is not
#'   Lorentzian: use the FWHM of the whole line, for example [voigt_fwhm()]
#'   of the fitted widths, corrected for the instrumental width when it is
#'   not negligible (for Gaussian broadening, approximately
#'   \eqn{\sqrt{w^2 - w_{inst}^2}}).
#'
#' - `method = "stark"`: in the impact approximation the Stark width is
#'   proportional to the electron density,
#'   \deqn{N_e = N_{ref} \frac{w}{w_{ref}(T)}}
#'   where \eqn{w_{ref}(T)} is the electron-impact width at density
#'   \eqn{N_{ref}}. It is taken from `stark` (a table returned by
#'   [starkb_lines()], interpolated at `temperature` with [stark_width()]),
#'   or given directly as `reference_width` (nm) at `reference_density`.
#'   Tables from other sources can be built with [stark_table()] or read
#'   from saved STARK-B files with [read_starkb()].
#'   Ion broadening is neglected, which is usually justified for lines of
#'   ions and at LIBS densities.
#' - `method = "halpha"`: the computer-simulation fit of Gigosos,
#'   González and Cardeñoso (2003) for the H\eqn{\alpha} line,
#'   \deqn{w = 1.098\,(N_e / 10^{17})^{0.67823}\ \mathrm{nm},}
#'   which includes the ion dynamics and depends only weakly on temperature.
#'
#' @param width The measured Stark full width at half maximum, in nm (see
#'   Details). A vector is allowed.
#' @param method `"stark"` (default) or `"halpha"`.
#' @param stark For `method = "stark"`: a tibble returned by
#'   [starkb_lines()]. Alternatively, give `reference_width`.
#' @param wavelength For `method = "stark"` with `stark`: the wavelength of
#'   the line, in nm.
#' @param temperature For `method = "stark"` with `stark`: the plasma
#'   temperature, in K.
#' @param reference_width For `method = "stark"`: the electron-impact Stark
#'   width (nm) at `reference_density`, when `stark` is not given.
#' @param reference_density The density of `reference_width`, in
#'   cm\eqn{^{-3}}. Default is \eqn{10^{17}}.
#' @param tolerance For `method = "stark"` with `stark`: the largest
#'   difference between `wavelength` and the tabulated wavelength, in nm
#'   (see [stark_width()]). Default is 0.5.
#'
#' @return A numeric vector of electron densities, in cm\eqn{^{-3}}.
#'
#' @references
#'  - Gigosos, M.A., González, M.Á., Cardeñoso, V. (2003). Computer
#'    simulated Balmer-alpha, -beta and -gamma Stark line profiles for
#'    non-equilibrium plasmas diagnostics. Spectrochimica Acta Part B,
#'    58(8):1489-1504.
#'  - Konjević, N. (1999). Plasma broadening and shifting of non-hydrogenic
#'    spectral lines: present status and applications. Physics Reports,
#'    316(6):339-401.
#'
#' @seealso [starkb_lines()], [stark_width()], [peak_fit()], [voigt_fwhm()]
#' @export electron_density
#'
#' @examples
#' # From the H-alpha line: a Stark FWHM of 0.5 nm
#' electron_density(0.5, method = "halpha")
#'
#' # From a line with known Stark width 0.012 nm at 1e17 cm-3
#' electron_density(c(0.006, 0.012, 0.024), reference_width = 0.012)
electron_density <- function(width, method = "stark", stark = NULL, wavelength = NULL,
                             temperature = NULL, reference_width = NULL,
                             reference_density = 1e17, tolerance = 0.5) {
  method <- match.arg(method, c("stark", "halpha"))
  if (!is.numeric(width) || anyNA(width) || any(width <= 0)) {
    stop("'width' must contain positive values (nm).")
  }
  if (method == "halpha") {
    return(1e17 * (width / 1.098)^(1 / 0.67823))
  }
  if (is.null(reference_width)) {
    if (is.null(stark) || is.null(wavelength) || is.null(temperature)) {
      stop("Give either 'reference_width', or 'stark', 'wavelength' and 'temperature'.")
    }
    check_number(temperature, "temperature", lower = 0, lower_open = TRUE)
    reference_width <- stark_width(stark, wavelength, temperature, density = reference_density,
                                   tolerance = tolerance)$width
  } else {
    check_number(reference_width, "reference_width", lower = 0, lower_open = TRUE)
  }
  check_number(reference_density, "reference_density", lower = 0, lower_open = TRUE)
  reference_density * width / reference_width
}

# ---- internals ---------------------------------------------------------------

starkb_cache <- new.env(parent = emptyenv())

starkb_endpoint <- "http://stark-b.obspm.fr/12.07/vamdc/tap/sync"

roman_numerals <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X",
                    "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", "XX")

# "Ca II" -> element Ca, charge 1
parse_species <- function(species) {
  if (!is.character(species) || length(species) != 1) {
    stop("'species' must be a single character string, such as \"Ca II\".")
  }
  parts <- strsplit(trimws(species), "\\s+")[[1]]
  if (length(parts) != 2 || !grepl("^[A-Z][a-z]?$", parts[1]) || !parts[2] %in% roman_numerals) {
    stop("'species' must be an element symbol and an ionization stage in Roman numerals, ",
         "such as \"Ca II\".")
  }
  list(element = parts[1], charge = match(parts[2], roman_numerals) - 1L,
       label = paste(parts[1], parts[2]))
}

species_label <- function(element, charge) {
  paste(element, roman_numerals[charge + 1])
}

starkb_download <- function(sp, timeout) {
  query <- sprintf("select * where (atomsymbol = '%s' and ioncharge = %d)", sp$element, sp$charge)
  url <- paste0(starkb_endpoint, "?LANG=VSS2&REQUEST=doQuery&FORMAT=XSAMS&QUERY=",
                utils::URLencode(query, reserved = TRUE))
  file <- tempfile(fileext = ".xml")
  old <- options(timeout = max(timeout, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  status <- tryCatch(
    utils::download.file(url, file, quiet = TRUE, mode = "wb"),
    error = function(e) e, warning = function(w) w
  )
  if (inherits(status, "condition") || !file.exists(file) || file.size(file) == 0) {
    msg <- if (inherits(status, "condition")) conditionMessage(status) else "empty response"
    stop("Could not download STARK-B data for ", sp$label, " (", msg, "). ",
         "Check the internet connection, or try again later.", call. = FALSE)
  }
  file
}

# Reads a STARK-B XSAMS document into a tibble.
parse_xsams <- function(file) {
  doc <- xml2::read_xml(file)
  xml2::xml_ns_strip(doc)
  text_of <- function(node, xpath) {
    vapply(node, function(n) {
      v <- xml2::xml_find_first(n, xpath)
      if (inherits(v, "xml_missing")) NA_character_ else xml2::xml_text(v)
    }, character(1))
  }

  # species: id -> element and charge
  ions <- xml2::xml_find_all(doc, "//Species//Ion")
  species <- data.frame(
    id = xml2::xml_attr(ions, "speciesID"),
    element = text_of(ions, "ancestor::Atom/ChemicalElement/ElementSymbol"),
    charge = as.integer(text_of(ions, "IonCharge")),
    stringsAsFactors = FALSE
  )

  # states: id -> "configuration term"
  states <- xml2::xml_find_all(doc, "//AtomicState")
  state_label <- trimws(paste(
    ifelse(is.na(conf <- text_of(states, ".//ConfigurationLabel")), "", conf),
    ifelse(is.na(term <- text_of(states, ".//TermLabel")), "", term)
  ))
  names(state_label) <- xml2::xml_attr(states, "stateID")

  # environments: id -> temperature, density, perturber
  envs <- xml2::xml_find_all(doc, "//Environments/Environment")
  env_species <- xml2::xml_find_first(envs, ".//Composition/Species")
  perturber_ref <- xml2::xml_attr(env_species, "speciesRef")
  perturber <- ifelse(
    xml2::xml_attr(env_species, "name") == "electron", "electron",
    ifelse(perturber_ref %in% species$id,
           species_label(species$element[match(perturber_ref, species$id)],
                         species$charge[match(perturber_ref, species$id)]),
           xml2::xml_attr(env_species, "name"))
  )
  environment <- data.frame(
    id = xml2::xml_attr(envs, "envID"),
    temperature = as.numeric(text_of(envs, "Temperature/Value")),
    density = as.numeric(text_of(envs, "TotalNumberDensity/Value")),
    perturber = perturber,
    stringsAsFactors = FALSE
  )

  # sources: id -> "Title (Year)"
  sources <- xml2::xml_find_all(doc, "//Sources/Source")
  source_label <- paste0(text_of(sources, "Title"), " (", text_of(sources, "Year"), ")")
  names(source_label) <- xml2::xml_attr(sources, "sourceID")

  transitions <- xml2::xml_find_all(doc, "//Radiative/RadiativeTransition")
  if (length(transitions) == 0) {
    return(empty_stark_table())
  }
  rows <- lapply(transitions, function(tr) {
    wl <- xml2::xml_find_first(tr, "EnergyWavelength/Wavelength/Value")
    to_nm <- switch(xml2::xml_attr(wl, "units"), A = 0.1, nm = 1, 1)
    broadening <- xml2::xml_find_all(tr, "Broadening[@name='pressure']")
    if (length(broadening) == 0) return(NULL)
    width_value <- xml2::xml_find_first(broadening, ".//LineshapeParameter/Value")
    width_to_nm <- ifelse(xml2::xml_attr(width_value, "units") == "A", 0.1, 1)
    shifts <- xml2::xml_find_all(tr, "Shifting")
    shift_value <- xml2::xml_find_first(shifts, ".//ShiftingParameter/Value")
    shift <- as.numeric(xml2::xml_text(shift_value)) *
      ifelse(xml2::xml_attr(shift_value, "units") == "A", 0.1, 1)
    names(shift) <- xml2::xml_attr(shifts, "envRef")
    env <- xml2::xml_attr(broadening, "envRef")
    species_ref <- xml2::xml_text(xml2::xml_find_first(tr, "SpeciesRef"))
    data.frame(
      species_ref = species_ref,
      wavelength = as.numeric(xml2::xml_text(wl)) * to_nm,
      upper = level_label(state_label, xml2::xml_text(xml2::xml_find_first(tr, "UpperStateRef"))),
      lower = level_label(state_label, xml2::xml_text(xml2::xml_find_first(tr, "LowerStateRef"))),
      env = env,
      width = as.numeric(xml2::xml_text(width_value)) * width_to_nm,
      shift = unname(shift[env]),
      source = paste(unique(source_label[xml2::xml_text(xml2::xml_find_all(tr, "SourceRef"))]),
                     collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  lines <- do.call(rbind, rows)
  env_row <- match(lines$env, environment$id)
  sp_row <- match(lines$species_ref, species$id)
  out <- tibble::tibble(
    species = species_label(species$element[sp_row], species$charge[sp_row]),
    wavelength = lines$wavelength,
    upper = lines$upper,
    lower = lines$lower,
    perturber = environment$perturber[env_row],
    temperature = environment$temperature[env_row],
    density = environment$density[env_row],
    width = lines$width,
    shift = lines$shift,
    source = lines$source
  )
  out[order(out$wavelength, out$perturber, out$density, out$temperature), ]
}

# Label of a level, "" when the file does not describe it
level_label <- function(labels, ref) {
  out <- unname(labels[ref])
  ifelse(is.na(out), "", out)
}

# Element-wise equality that treats two missing values as equal
same_value <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

empty_stark_table <- function() {
  tibble::tibble(species = character(), wavelength = numeric(), upper = character(),
                 lower = character(), perturber = character(), temperature = numeric(),
                 density = numeric(), width = numeric(), shift = numeric(), source = character())
}

check_wavelength_range <- function(wavelength) {
  if (!is.null(wavelength) && (!is.numeric(wavelength) || length(wavelength) != 2 || anyNA(wavelength))) {
    stop("'wavelength' must be a numeric vector of length 2 (nm).", call. = FALSE)
  }
  invisible(wavelength)
}

filter_stark <- function(lines, wavelength, perturber) {
  if (!is.null(wavelength)) {
    lines <- lines[lines$wavelength >= min(wavelength) & lines$wavelength <= max(wavelength), ]
  }
  if (!is.null(perturber)) {
    lines <- lines[lines$perturber %in% perturber, ]
  }
  lines
}

check_stark_table <- function(stark) {
  needed <- c("wavelength", "upper", "lower", "perturber", "temperature", "density", "width", "shift")
  if (!is.data.frame(stark) || !all(needed %in% names(stark))) {
    stop("'stark' must be a table returned by starkb_lines(), read_starkb() or stark_table(), ",
         "with columns ",
         paste(needed, collapse = ", "), ".", call. = FALSE)
  }
  invisible(stark)
}
