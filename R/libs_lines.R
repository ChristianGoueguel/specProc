#' @title Candidate Emission Lines for LIBS Spectra
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Lists the lines of the selected atoms and ions that are expected to be the
#' strongest in a plasma at a given temperature, from the NIST Atomic
#' Spectra Database, to help identify the emission lines of a spectrum.
#'
#' @details
#' For each species, the lines with a transition probability are retrieved
#' with [nist_lines()] (and cached for the R session). Their relative
#' intensities in a plasma in local thermodynamic equilibrium at
#' temperature \eqn{T} are
#' \deqn{I \propto \frac{g_k A_{ki}}{\lambda} \exp\left(-\frac{E_k}{k_B T}\right)}
#' normalized to the strongest line of the species in the range. The
#' intensities are relative within each species: comparing species would
#' need their concentrations and, between ionization stages, the Saha
#' equation. Self-absorption, which weakens resonance lines, is ignored.
#'
#' The `top` strongest lines of each species with a relative intensity of
#' at least `min_relative` are kept. [plot_lines()] overlays them on a
#' spectrum, and [line_finder()] does so interactively.
#'
#' @param species A character vector of emitters in spectroscopic notation,
#'   such as `c("Ca I", "Ca II", "Mg I")`.
#' @param wavelength A numeric vector of length 2: the wavelength range, in
#'   nm. Default is 200 to 900 nm.
#' @param temperature The plasma temperature, in K. Default is 10000.
#' @param top The maximum number of lines kept per species. Default is 20.
#' @param min_relative The minimum relative intensity of the lines kept,
#'   between 0 and 1. Default is 0.
#' @param timeout The download timeout, in seconds. Default is 120.
#'
#' @return A tibble with one row per line, sorted by wavelength, and
#'   columns `species`, `element`, `stage` (1 for neutral atoms, 2 for
#'   singly charged ions, ...), `wavelength` (nm), `relative_intensity`,
#'   `Aki`, `Ek`, `gk`, `accuracy`, `lower` and `upper`. Species without
#'   lines in the range contribute no rows.
#'
#' @seealso [plot_lines()], [line_finder()], [nist_lines()]
#' @export libs_lines
#'
#' @examples
#' \donttest{
#' # needs an internet connection
#' lines <- try(libs_lines(c("Ca I", "Ca II"), wavelength = c(390, 450), top = 5))
#' if (!inherits(lines, "try-error")) lines
#' }
libs_lines <- function(species, wavelength = c(200, 900), temperature = 10000, top = 20,
                       min_relative = 0, timeout = 120) {
  if (!is.character(species) || length(species) == 0) {
    stop("'species' must be a character vector, such as c(\"Ca I\", \"Ca II\").")
  }
  if (!is.numeric(wavelength) || length(wavelength) != 2 || anyNA(wavelength) || any(wavelength <= 0)) {
    stop("'wavelength' must be a numeric vector of length 2 (nm).")
  }
  check_line_settings(temperature, top, min_relative)
  raw <- lapply(unique(species), fetch_species_lines, wavelength = sort(wavelength), timeout = timeout)
  rank_lines(do.call(rbind, raw), temperature = temperature, top = top,
             min_relative = min_relative, wavelength = wavelength)
}

#' @title Overlay Emission Lines on a Spectrum
#'
#' @description
#' Plots a spectrum with vertical markers at the wavelengths of candidate
#' emission lines, colored by ionization stage, to identify the lines of the
#' spectrum.
#'
#' @details
#' With `interactive = TRUE`, the plot is made with plotly: the spectrum is
#' drawn with WebGL, the lines of each ionization stage form one trace that
#' can be shown or hidden from the legend, and hovering over a line shows its
#' species, wavelength, transition probability, upper-level energy and
#' accuracy grade. Otherwise, a ggplot is returned.
#'
#' With `scale_markers = TRUE`, the height of each marker is its relative
#' intensity (see [libs_lines()]) times the maximum of the spectrum;
#' otherwise all markers span the full height.
#'
#' @param spectrum A spectrum: a numeric vector named by wavelength, or a
#'   one-row matrix or data frame whose column names are the wavelengths.
#'   Average several spectra first.
#' @param lines A tibble returned by [libs_lines()].
#' @param shift A wavelength shift, in nm, added to the line wavelengths to
#'   correct the wavelength calibration of the spectrometer. Default is 0.
#' @param scale_markers A logical: scale the markers by the relative
#'   intensity of the lines (`TRUE`, default) or draw them full height.
#' @param interactive A logical: plotly (`TRUE`, default) or ggplot2.
#' @param title An optional plot title.
#'
#' @return A plotly or ggplot object.
#'
#' @seealso [libs_lines()], [line_finder()]
#' @export plot_lines
#'
#' @examples
#' data(forageLIBS)
#' spectrum <- colMeans(forageLIBS[-(1:14)])
#' spectrum <- spectrum[as.numeric(names(spectrum)) > 403 & as.numeric(names(spectrum)) < 406]
#' # the K I doublet, with the atomic data of the NIST database
#' lines <- tibble::tibble(
#'   species = "K I", element = "K", stage = 1L, wavelength = c(404.414, 404.721),
#'   relative_intensity = c(1, 0.5), Aki = c(1.15e6, 1.07e6), Ek = c(3.065, 3.063),
#'   gk = c(4, 2), accuracy = "A", lower = "4s 2S", upper = c("5p 2P* 3/2", "5p 2P* 1/2")
#' )
#' plot_lines(spectrum, lines, interactive = FALSE)
#'
plot_lines <- function(spectrum, lines, shift = 0, scale_markers = TRUE, interactive = TRUE,
                       title = NULL) {
  spec <- as_spectrum(spectrum)
  check_lines_table(lines)
  check_number(shift, "shift")
  check_flag(scale_markers, "scale_markers")
  check_flag(interactive, "interactive")
  segments <- line_segments(lines, spec, shift, scale_markers)
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive plots.")
    p <- plotly::plot_ly()
    p <- plotly::add_trace(p, x = spec$wavelength, y = spec$intensity, type = "scattergl",
                           mode = "lines", name = "spectrum",
                           line = list(color = "#595959", width = 1), hoverinfo = "x+y")
    for (trace in stage_traces(segments)) {
      p <- do.call(plotly::add_trace, c(list(p), trace))
    }
    plotly::layout(p, title = plotly_title(title),
                   xaxis = list(title = "Wavelength (nm)"), yaxis = list(title = "Intensity"),
                   legend = list(orientation = "h", y = -0.15), hovermode = "closest")
  } else {
    # markers on top of the spectrum, semi-transparent
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = spec, ggplot2::aes(.data$wavelength, .data$intensity),
                         colour = "grey35", linewidth = 0.3) +
      ggplot2::geom_segment(
        data = segments,
        ggplot2::aes(x = .data$x, xend = .data$x, y = 0, yend = .data$height, colour = .data$stage_label),
        linewidth = 0.5, alpha = 0.7
      ) +
      ggplot2::scale_colour_manual(values = stage_colours, name = NULL, drop = TRUE) +
      ggplot2::labs(x = "Wavelength (nm)", y = "Intensity", title = title) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
    finish_title(p)
  }
}

# ---- internals ---------------------------------------------------------------

stage_colours <- c(I = "#1b9e77", II = "#d95f02", III = "#7570b3", IV = "#e7298a", V = "#66a61e")

check_line_settings <- function(temperature, top, min_relative) {
  check_number(temperature, "temperature", lower = 0, lower_open = TRUE)
  check_count(top, "top")
  check_number(min_relative, "min_relative", lower = 0, upper = 1)
  invisible(TRUE)
}

# Lines of one species with a transition probability; no rows when NIST has
# none in the range (other errors, such as network failures, propagate).
fetch_species_lines <- function(species, wavelength, timeout = 120) {
  tryCatch(
    nist_lines(species, wavelength, with_aki = TRUE, timeout = timeout),
    error = function(e) {
      if (grepl("returned no data", conditionMessage(e), fixed = TRUE)) {
        nist_empty_lines(parse_species(species)$label)
      } else {
        stop(e)
      }
    }
  )
}

nist_empty_lines <- function(label) {
  tibble::tibble(species = character(), wavelength = numeric(), Aki = numeric(),
                 fik = numeric(), accuracy = character(), Ei = numeric(), Ek = numeric(),
                 gi = numeric(), gk = numeric(), lower = character(), upper = character(),
                 intensity = character())
}

# Relative LTE intensities, and the strongest lines of each species.
rank_lines <- function(raw, temperature, top, min_relative, wavelength = NULL) {
  if (is.null(raw) || nrow(raw) == 0) {
    return(empty_line_list())
  }
  raw <- raw[!is.na(raw$Aki) & !is.na(raw$Ek) & !is.na(raw$gk) & !is.na(raw$wavelength), ]
  if (!is.null(wavelength)) {
    raw <- raw[raw$wavelength >= min(wavelength) & raw$wavelength <= max(wavelength), ]
  }
  if (nrow(raw) == 0) {
    return(empty_line_list())
  }
  strength <- raw$gk * raw$Aki / raw$wavelength * exp(-raw$Ek / (k_boltzmann_ev * temperature))
  relative <- strength / stats::ave(strength, raw$species, FUN = max)
  keep <- unlist(lapply(split(seq_len(nrow(raw)), raw$species), function(i) {
    i <- i[relative[i] >= min_relative]
    i[order(relative[i], decreasing = TRUE)][seq_len(min(top, length(i)))]
  }), use.names = FALSE)
  keep <- keep[!is.na(keep)]
  parsed <- lapply(raw$species[keep], parse_species)
  out <- tibble::tibble(
    species = raw$species[keep],
    element = vapply(parsed, `[[`, character(1), "element"),
    stage = vapply(parsed, function(p) p$charge + 1L, integer(1)),
    wavelength = raw$wavelength[keep],
    relative_intensity = relative[keep],
    Aki = raw$Aki[keep],
    Ek = raw$Ek[keep],
    gk = raw$gk[keep],
    accuracy = raw$accuracy[keep],
    lower = raw$lower[keep],
    upper = raw$upper[keep]
  )
  out[order(out$wavelength), ]
}

empty_line_list <- function() {
  tibble::tibble(species = character(), element = character(), stage = integer(),
                 wavelength = numeric(), relative_intensity = numeric(), Aki = numeric(),
                 Ek = numeric(), gk = numeric(), accuracy = character(), lower = character(),
                 upper = character())
}

check_lines_table <- function(lines) {
  needed <- c("species", "stage", "wavelength", "relative_intensity")
  if (!is.data.frame(lines) || !all(needed %in% names(lines))) {
    stop("'lines' must be a table returned by libs_lines().", call. = FALSE)
  }
  invisible(lines)
}

# A spectrum as a data frame of wavelength and intensity.
as_spectrum <- function(spectrum) {
  if (is.data.frame(spectrum) || is.matrix(spectrum)) {
    if (nrow(spectrum) != 1) {
      stop("'spectrum' must be a single spectrum (one row); average several spectra first.",
           call. = FALSE)
    }
    values <- as_numeric_matrix(spectrum, "spectrum")[1, ]
  } else if (is.numeric(spectrum)) {
    values <- spectrum
  } else {
    stop("'spectrum' must be a named numeric vector or a one-row matrix or data frame.", call. = FALSE)
  }
  wavelength <- parse_wavelength(names(values) %||% character(0))
  if (length(wavelength) != length(values) || anyNA(wavelength)) {
    stop("The names (or column names) of 'spectrum' must be the wavelengths.", call. = FALSE)
  }
  data.frame(wavelength = wavelength, intensity = unname(values))
}

# Marker segments: one row per line, with its position, height and hover text.
line_segments <- function(lines, spec, shift, scale_markers) {
  top_value <- max(spec$intensity, na.rm = TRUE)
  if (!is.finite(top_value) || top_value <= 0) top_value <- 1
  if (nrow(lines) == 0) {
    return(data.frame(x = numeric(), height = numeric(), stage_label = character(), text = character()))
  }
  stage_label <- factor(roman_numerals[lines$stage], levels = roman_numerals)
  data.frame(
    x = lines$wavelength + shift,
    height = if (scale_markers) top_value * lines$relative_intensity else rep(top_value, nrow(lines)),
    stage_label = droplevels(stage_label),
    text = sprintf(
      "%s %.3f nm<br>relative intensity %.2f<br>A = %.2e s-1, Ek = %.2f eV, gk = %s<br>accuracy %s<br>%s - %s",
      lines$species, lines$wavelength, lines$relative_intensity,
      lines$Aki %||% NA, lines$Ek %||% NA, lines$gk %||% NA, lines$accuracy %||% "",
      lines$lower %||% "", lines$upper %||% ""
    ),
    stringsAsFactors = FALSE
  )
}

# One plotly trace per ionization stage: vertical segments separated by NA.
stage_traces <- function(segments) {
  if (nrow(segments) == 0) return(list())
  lapply(levels(droplevels(segments$stage_label)), function(stage) {
    s <- segments[segments$stage_label == stage, ]
    n <- nrow(s)
    list(
      x = as.vector(rbind(s$x, s$x, NA)),
      y = as.vector(rbind(0, s$height, NA)),
      text = as.vector(rbind(s$text, s$text, NA)),
      type = "scatter", mode = "lines", name = paste("stage", stage),
      line = list(color = unname(stage_colours[stage]), width = 1.5),
      hoverinfo = "text", connectgaps = FALSE
    )
  })
}
