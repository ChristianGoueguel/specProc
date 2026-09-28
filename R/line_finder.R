#' @title Interactive Identification of Emission Lines
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Launches a Shiny app to identify the emission lines of LIBS spectra.
#' Elements are selected on a periodic table, and the strongest lines of
#' their selected ionization stages, from the NIST Atomic Spectra Database,
#' are overlaid on a spectrum in an interactive plotly graph, with one color
#' per ionization stage.
#'
#' @details
#' The app offers:
#'  - a clickable periodic table; elements without lines in the wavelength
#'    range of the spectra are grayed out once queried;
#'  - the ionization stages to show (I, II, III);
#'  - the plasma temperature and the number of lines per species, which set
#'    the lines kept and the height of their markers (relative intensities in
#'    local thermodynamic equilibrium, see [libs_lines()]);
#'  - the wavelength range, a wavelength shift to correct the calibration of
#'    the spectrometer, and the spectrum to show: the mean of all spectra, the
#'    mean of a group (from the first character or factor column of
#'    `spectra`), or a single spectrum;
#'  - a table of the displayed lines, downloadable as CSV, and buttons to save
#'    and reload the lines fetched so far, so that a session can continue
#'    offline.
#'
#' Each species is downloaded from NIST once per R session (see
#' [nist_lines()]); later changes of the settings are computed locally. The
#' zoom of the plot is kept when the lines change.
#'
#' The app needs the shiny, plotly and bslib packages, and an internet
#' connection for the species not yet fetched.
#'
#' @param spectra Spectra, one per row: a numeric matrix, or a data frame
#'   whose wavelength columns are named by their wavelengths (other columns
#'   are used as labels).
#' @param launch.browser Passed to [shiny::runApp()]. Default is `TRUE`
#'   when R is interactive.
#'
#' @return Called for its side effect: runs the app. Use [libs_lines()] and
#'   [plot_lines()] for the same results in scripts.
#'
#' @seealso [libs_lines()], [plot_lines()], [nist_lines()]
#' @export line_finder
#'
#' @examples
#' if (interactive() && rlang::is_installed(c("shiny", "plotly", "bslib"))) {
#'   data(specLIBS)
#'   line_finder(specLIBS)
#' }
line_finder <- function(spectra, launch.browser = interactive()) {
  rlang::check_installed(c("shiny", "plotly", "bslib"), reason = "to run the line finder app.")
  shiny::runApp(line_finder_app(spectra), launch.browser = launch.browser)
}

# The app object; `fetch(species, wavelength)` returns the NIST lines of a
# species (replaced by a stub in tests).
line_finder_app <- function(spectra, fetch = fetch_species_lines) {
  data <- finder_spectra(spectra)
  shiny::shinyApp(ui = finder_ui(data), server = finder_server(data, fetch))
}

# ---- data --------------------------------------------------------------------

finder_spectra <- function(spectra) {
  if (is.matrix(spectra)) spectra <- as.data.frame(spectra, check.names = FALSE)
  if (!is.data.frame(spectra) || nrow(spectra) == 0) {
    stop("'spectra' must be a matrix or data frame with one spectrum per row.", call. = FALSE)
  }
  wl <- suppressWarnings(as.numeric(names(spectra)))
  is_channel <- !is.na(wl) & vapply(spectra, is.numeric, logical(1))
  if (sum(is_channel) < 2) {
    stop("'spectra' must have columns named by their wavelengths.", call. = FALSE)
  }
  x <- as.matrix(spectra[is_channel])
  storage.mode(x) <- "double"
  labels <- names(spectra)[!is_channel &
                             vapply(spectra, function(v) is.character(v) || is.factor(v), logical(1))]
  group <- if (length(labels) > 0) as.character(spectra[[labels[1]]]) else NULL
  list(x = x, wavelength = wl[is_channel], group = group,
       group_name = if (length(labels) > 0) labels[1] else NULL)
}

finder_spectrum <- function(data, choice) {
  x <- data$x
  values <- if (is.null(choice) || choice == "mean") {
    colMeans(x, na.rm = TRUE)
  } else if (startsWith(choice, "group:")) {
    colMeans(x[data$group == sub("^group:", "", choice), , drop = FALSE], na.rm = TRUE)
  } else {
    x[as.integer(sub("^row:", "", choice)), ]
  }
  stats::setNames(values, data$wavelength)
}

finder_choices <- function(data) {
  choices <- c(`Mean of all spectra` = "mean")
  if (!is.null(data$group)) {
    groups <- unique(data$group)
    choices <- c(choices, stats::setNames(paste0("group:", groups),
                                          paste0("Mean of ", data$group_name, " ", groups)))
  }
  rows <- seq_len(nrow(data$x))
  row_labels <- if (is.null(data$group)) paste("Spectrum", rows) else
    paste0("Spectrum ", rows, " (", data$group, ")")
  c(choices, stats::setNames(paste0("row:", rows), row_labels))
}

# Periodic table layout: symbol, row and column (lanthanides and actinides
# in rows 9 and 10).
periodic_table <- function() {
  symbols <- c(
    "H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S",
    "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga",
    "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd",
    "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm",
    "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os",
    "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa",
    "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", "Lr"
  )
  z <- seq_along(symbols)
  row <- col <- integer(length(z))
  place <- function(range, r, cols) {
    row[range] <<- r
    col[range] <<- cols
  }
  place(1, 1, 1); place(2, 1, 18)
  place(3:4, 2, 1:2); place(5:10, 2, 13:18)
  place(11:12, 3, 1:2); place(13:18, 3, 13:18)
  place(19:36, 4, 1:18); place(37:54, 5, 1:18)
  place(55:56, 6, 1:2); place(57:71, 9, 3:17); place(72:86, 6, 4:18)
  place(87:88, 7, 1:2); place(89:103, 10, 3:17)
  data.frame(z = z, symbol = symbols, row = row, col = col, stringsAsFactors = FALSE)
}

# ---- UI ----------------------------------------------------------------------

finder_css <- "
.pt-grid { display: grid; grid-template-columns: repeat(18, minmax(26px, 1fr)); gap: 2px; }
.pt-el { font-size: 11px; padding: 3px 0; border: 1px solid #c8c8c8; background: #f7f7f7;
         border-radius: 3px; cursor: pointer; text-align: center; line-height: 1.1; }
.pt-el:hover { border-color: #666; }
.pt-el small { display: block; font-size: 8px; color: #888; }
.pt-el.pt-selected { background: #1b9e77; color: white; border-color: #137a5c; }
.pt-el.pt-selected small { color: #e8f5f0; }
.pt-el.pt-empty { opacity: 0.35; }
.pt-gap { grid-column: 1 / span 18; height: 6px; }
"

finder_js <- "
$(document).on('click', '.pt-el', function() {
  $(this).toggleClass('pt-selected');
  var selected = [];
  $('.pt-el.pt-selected').each(function() { selected.push($(this).data('el')); });
  Shiny.setInputValue('elements', selected);
});
Shiny.addCustomMessageHandler('pt-empty', function(elements) {
  [].concat(elements).forEach(function(e) { $('.pt-el[data-el=\"' + e + '\"]').addClass('pt-empty'); });
});
Shiny.addCustomMessageHandler('pt-clear', function(x) {
  $('.pt-el').removeClass('pt-selected');
  Shiny.setInputValue('elements', []);
});
"

periodic_table_ui <- function() {
  pt <- periodic_table()
  cells <- lapply(seq_len(nrow(pt)), function(i) {
    shiny::tags$div(
      class = "pt-el", `data-el` = pt$symbol[i], title = paste(pt$symbol[i], "(Z =", pt$z[i], ")"),
      style = sprintf("grid-row: %d; grid-column: %d;", pt$row[i], pt$col[i]),
      pt$symbol[i], shiny::tags$small(pt$z[i])
    )
  })
  shiny::tags$div(class = "pt-grid", cells, shiny::tags$div(class = "pt-gap", style = "grid-row: 8;"))
}

finder_ui <- function(data) {
  range_wl <- range(data$wavelength)
  bslib::page_sidebar(
    title = "specProc line finder",
    shiny::tags$head(shiny::tags$style(shiny::HTML(finder_css)),
                     shiny::tags$script(shiny::HTML(finder_js))),
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput("spectrum", "Spectrum", choices = finder_choices(data)),
      shiny::checkboxGroupInput("stages", "Ionization stages",
                                choices = c(I = 1, II = 2, III = 3), selected = c(1, 2), inline = TRUE),
      shiny::sliderInput("temperature", "Temperature (K)", min = 3000, max = 30000,
                         value = 10000, step = 500),
      shiny::sliderInput("wl_range", "Wavelength range (nm)", min = floor(range_wl[1]),
                         max = ceiling(range_wl[2]), value = c(floor(range_wl[1]), ceiling(range_wl[2])),
                         step = 0.5),
      shiny::numericInput("top", "Lines per species", value = 15, min = 1, max = 500, step = 1),
      shiny::sliderInput("min_relative", "Minimum relative intensity", min = 0, max = 1,
                         value = 0.01, step = 0.01),
      shiny::numericInput("shift", "Wavelength shift (nm)", value = 0, step = 0.01),
      shiny::checkboxInput("scale", "Scale markers by expected intensity", value = TRUE),
      shiny::actionButton("clear", "Clear selection"),
      shiny::tags$hr(),
      shiny::downloadButton("download_lines", "Lines (CSV)"),
      shiny::downloadButton("save_cache", "Save fetched data"),
      shiny::fileInput("load_cache", "Load fetched data (.rds)", accept = ".rds")
    ),
    bslib::card(bslib::card_header("Elements"), periodic_table_ui()),
    bslib::card(full_screen = TRUE, bslib::card_header("Spectrum and candidate lines"),
                plotly::plotlyOutput("plot", height = "460px")),
    bslib::card(bslib::card_header("Displayed lines"), shiny::tableOutput("lines_table"))
  )
}

# ---- server ------------------------------------------------------------------

finder_server <- function(data, fetch) {
  fetch_range <- range(data$wavelength)
  function(input, output, session) {
    cache <- shiny::reactiveVal(list())   # species -> NIST lines

    species <- shiny::reactive({
      elements <- unlist(input$elements)
      stages <- as.integer(input$stages)
      if (length(elements) == 0 || length(stages) == 0) return(character())
      as.vector(outer(elements, roman_numerals[stages], paste))
    })

    # fetch the species not yet in the cache
    shiny::observeEvent(species(), {
      missing_species <- setdiff(species(), names(cache()))
      if (length(missing_species) == 0) return()
      store <- cache()
      shiny::withProgress(message = "Fetching NIST lines", value = 0, {
        for (sp in missing_species) {
          shiny::incProgress(1 / length(missing_species), detail = sp)
          lines <- tryCatch(fetch(sp, fetch_range), error = function(e) {
            shiny::showNotification(paste0(sp, ": ", conditionMessage(e)), type = "error")
            NULL
          })
          if (!is.null(lines)) store[[sp]] <- lines
        }
      })
      cache(store)
      # grey out the elements with no lines in any fetched stage
      elements <- unique(sub(" .*", "", names(store)))
      empty <- elements[vapply(elements, function(el) {
        all(vapply(store[startsWith(names(store), paste0(el, " "))], nrow, integer(1)) == 0)
      }, logical(1))]
      if (length(empty) > 0) session$sendCustomMessage("pt-empty", as.list(empty))
    })

    lines <- shiny::reactive({
      store <- cache()[intersect(species(), names(cache()))]
      if (length(store) == 0) return(empty_line_list())
      temperature <- input$temperature %||% 10000
      top <- max(1, as.integer(input$top %||% 15))
      rank_lines(do.call(rbind, store), temperature = temperature, top = top,
                 min_relative = input$min_relative %||% 0, wavelength = input$wl_range)
    })

    spectrum <- shiny::reactive(finder_spectrum(data, input$spectrum))

    output$plot <- plotly::renderPlotly({
      spec <- spectrum()
      wl_range <- input$wl_range %||% fetch_range
      keep <- as.numeric(names(spec)) >= wl_range[1] & as.numeric(names(spec)) <= wl_range[2]
      p <- plot_lines(spec[keep], lines(), shift = input$shift %||% 0,
                      scale_markers = isTRUE(input$scale %||% TRUE), interactive = TRUE)
      # keep the zoom when the lines change, reset it when the spectrum changes
      plotly::layout(p, uirevision = paste(input$spectrum, wl_range, collapse = " "))
    })

    output$lines_table <- shiny::renderTable({
      l <- lines()
      if (nrow(l) == 0) return(NULL)
      data.frame(
        species = l$species, `wavelength (nm)` = sprintf("%.3f", l$wavelength),
        `relative intensity` = sprintf("%.3f", l$relative_intensity),
        `Aki (s-1)` = sprintf("%.2e", l$Aki), `Ek (eV)` = sprintf("%.3f", l$Ek),
        gk = l$gk, accuracy = l$accuracy, transition = paste(l$lower, "-", l$upper),
        check.names = FALSE
      )
    })

    shiny::observeEvent(input$clear, session$sendCustomMessage("pt-clear", TRUE))

    output$download_lines <- shiny::downloadHandler(
      filename = function() "libs_lines.csv",
      content = function(file) utils::write.csv(lines(), file, row.names = FALSE)
    )
    output$save_cache <- shiny::downloadHandler(
      filename = function() "nist_lines_cache.rds",
      content = function(file) saveRDS(cache(), file)
    )
    shiny::observeEvent(input$load_cache, {
      loaded <- tryCatch(readRDS(input$load_cache$datapath), error = function(e) NULL)
      if (!is.list(loaded) || is.null(names(loaded))) {
        shiny::showNotification("Not a file saved by this app.", type = "error")
        return()
      }
      store <- cache()
      store[names(loaded)] <- loaded
      cache(store)
      shiny::showNotification(paste("Loaded", length(loaded), "species."), type = "message")
    })

    invisible(list(lines = lines, cache = cache, species = species))
  }
}
