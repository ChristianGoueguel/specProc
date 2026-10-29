#' @title Interactive Identification of Emission Lines
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Launches a Shiny app to identify the emission lines of LIBS spectra.
#' Elements are selected on a periodic table, and the strongest lines of
#' their selected ionization stages, from the NIST Atomic Spectra Database,
#' are overlaid on a spectrum in an interactive plotly graph, with one color
#' per ionization stage. The heads of molecular bands can be overlaid too.
#' Lines of a species picked on the spectrum build a Boltzmann plot, whose
#' temperature is fitted again as lines are added or left out.
#'
#' @details
#' The app offers:
#'  - a clickable periodic table; elements without lines in the wavelength
#'    range of the spectra are grayed out once queried. It can be hidden
#'    (**Hide table**) to enlarge the spectrum, and the selected elements
#'    stay listed in the panel header;
#'  - the ionization stages to show (I, II, III);
#'  - the plasma temperature and the number of lines per species, which set
#'    the lines kept and the height of their markers (relative intensities in
#'    local thermodynamic equilibrium, see [libs_lines()]);
#'  - molecules whose band heads are marked with dashed lines, at a
#'    vibrational temperature (see [molecular_bands()]; no download
#'    needed);
#'  - the wavelength range, a wavelength shift to correct the calibration of
#'    the spectrometer, and the spectrum to show: the mean of all spectra, the
#'    mean of a group (from the first character or factor column of
#'    `spectra`), or a single spectrum;
#'  - a download of the displayed lines as CSV, and buttons to save and reload
#'    the lines fetched so far, so that a session can continue offline.
#'
#' Each species is downloaded from NIST once per R session (see
#' [nist_lines()]); later changes of the settings are computed locally. The
#' zoom of the plot is kept when the lines change.
#'
#' **Boltzmann plot.** Clicking the dot at the top of a line marker adds the
#' line to the Boltzmann plot of its species, and clicking it again removes
#' it; **Add shown lines** adds all the lines shown for the species chosen in
#' the Boltzmann plot panel. The intensities are measured on the displayed
#' spectrum with [line_intensities()], around the tabulated wavelengths plus
#' the wavelength shift, with the settings of the panel, and the temperature
#' is fitted with [boltzmann()]. Clicking a point of the plot leaves its line
#' out of the fit or puts it back; lines without a positive intensity are
#' left out. The table under the plot lists the upper-level energy, the
#' signal-to-noise ratio and the residual of each line, **Use T** sets the
#' temperature of the candidate lines to the fitted one, and **Done** closes
#' the app and returns the fit to R. The spectra should be corrected for the
#' spectral response of the instrument first (see [correct_response()]), and
#' the lines should be optically thin (see [self_absorption()]).
#'
#' The app needs the shiny, plotly and bslib packages, and an internet
#' connection for the species not yet fetched.
#'
#' @param spectra Spectra, one per row: a numeric matrix, or a data frame
#'   whose wavelength columns are named by their wavelengths (other columns
#'   are used as labels).
#' @param launch.browser Passed to [shiny::runApp()]. Default is `TRUE`
#'   when R is interactive.
#' @param show_table A logical: show the periodic table when the app starts
#'   (`TRUE`, default). The **Hide table** button of the Elements panel hides
#'   or shows it at any time; hiding it gives the spectrum the whole height of
#'   the window.
#'
#' @return Invisibly, when the app is closed with **Done**, the [boltzmann()]
#'   fit of the Boltzmann plot, whose `lines` element holds the measured
#'   lines; `NULL` if the plot has fewer than 3 usable lines. Use
#'   [libs_lines()], [plot_lines()], [line_intensities()] and [boltzmann()]
#'   for the same results in scripts.
#'
#' @seealso [libs_lines()], [plot_lines()], [nist_lines()], [boltzmann()]
#' @export line_finder
#'
#' @examples
#' if (interactive() && rlang::is_installed(c("shiny", "plotly", "bslib"))) {
#'   data(forageLIBS)
#'   fit <- line_finder(forageLIBS[-(1:14)])
#'   # after a Boltzmann plot closed with Done
#'   if (!is.null(fit)) plot_boltzmann(fit)
#' }
line_finder <- function(spectra, launch.browser = interactive(), show_table = TRUE) {
  rlang::check_installed(c("shiny", "plotly", "bslib"), reason = "to run the line finder app.")
  check_flag(show_table, "show_table")
  invisible(shiny::runApp(line_finder_app(spectra, show_table = show_table),
                          launch.browser = launch.browser))
}

# The app object; `fetch(species, wavelength)` returns the NIST lines of a
# species (replaced by a stub in tests).
line_finder_app <- function(spectra, fetch = fetch_species_lines, show_table = TRUE) {
  data <- finder_spectra(spectra)
  shiny::shinyApp(ui = finder_ui(data, show_table), server = finder_server(data, fetch))
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

# Periodic table: symbol, layout row and column (lanthanides and actinides in
# rows 9 and 10), category and atomic weight.
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
  category <- rep("transition metal", length(z))
  category[c(3, 11, 19, 37, 55, 87)] <- "alkali metal"
  category[c(4, 12, 20, 38, 56, 88)] <- "alkaline earth metal"
  category[c(13, 31, 49, 50, 81, 82, 83, 84)] <- "post-transition metal"
  category[c(5, 14, 32, 33, 51, 52)] <- "metalloid"
  category[c(1, 6, 7, 8, 15, 16, 34)] <- "nonmetal"
  category[c(9, 17, 35, 53, 85)] <- "halogen"
  category[c(2, 10, 18, 36, 54, 86)] <- "noble gas"
  category[57:71] <- "lanthanide"
  category[89:103] <- "actinide"
  # standard atomic weights (IUPAC, abridged); mass number of the longest-lived
  # isotope for the elements without stable isotopes
  mass <- c(
    1.008, 4.0026, 6.94, 9.0122, 10.81, 12.011, 14.007, 15.999, 18.998, 20.180, 22.990, 24.305,
    26.982, 28.085, 30.974, 32.06, 35.45, 39.95, 39.098, 40.078, 44.956, 47.867, 50.942, 51.996,
    54.938, 55.845, 58.933, 58.693, 63.546, 65.38, 69.723, 72.630, 74.922, 78.971, 79.904,
    83.798, 85.468, 87.62, 88.906, 91.224, 92.906, 95.95, 98, 101.07, 102.91, 106.42, 107.87,
    112.41, 114.82, 118.71, 121.76, 127.60, 126.90, 131.29, 132.91, 137.33, 138.91, 140.12,
    140.91, 144.24, 145, 150.36, 151.96, 157.25, 158.93, 162.50, 164.93, 167.26, 168.93, 173.05,
    174.97, 178.49, 180.95, 183.84, 186.21, 190.23, 192.22, 195.08, 196.97, 200.59, 204.38,
    207.2, 208.98, 209, 210, 222, 223, 226, 227, 232.04, 231.04, 238.03, 237, 244, 243, 247,
    247, 251, 252, 257, 258, 259, 262
  )
  data.frame(z = z, symbol = symbols, row = row, col = col, category = category, mass = mass,
             stringsAsFactors = FALSE)
}

# ---- UI ----------------------------------------------------------------------

finder_primary <- "#1f4e79"

finder_categories <- c(
  `alkali metal` = "#f9d8d2", `alkaline earth metal` = "#fce5c6", `transition metal` = "#fbf0c2",
  `post-transition metal` = "#dbead2", metalloid = "#d2e9e3", nonmetal = "#d5e5f4",
  halogen = "#dfdcf2", `noble gas` = "#ecdcef", lanthanide = "#ebe5d9", actinide = "#e2e2e2"
)

finder_css <- "
.bslib-page-sidebar > .navbar { background: #14263d; border-bottom: 3px solid #1f4e79; }
.lf-title { color: #fff; font-size: 1.05rem; font-weight: 600; letter-spacing: .01em; }
.lf-title .lf-sub { font-weight: 400; color: #a9bdd3; padding-left: .6rem; margin-left: .6rem;
                    border-left: 1px solid #3d5673; }
.card-header { background: #fff; font-weight: 600; }
.lf-header { display: flex; align-items: center; justify-content: space-between; gap: .75rem; }
.lf-actions .shiny-text-output { font-weight: 400; color: #6c7a89; font-size: .8rem; }
.lf-actions { display: flex; align-items: center; gap: .5rem; }
.pt-grid { display: grid; grid-template-columns: repeat(18, minmax(0, 1fr)); gap: 3px;
           max-width: 900px; margin: 0 auto; }
.pt-el { height: 24px; display: flex; align-items: center; justify-content: center;
         font-size: 11.5px; font-weight: 600; color: #1f2933; background: var(--pt-bg);
         border: 1px solid rgba(0, 0, 0, .07); border-radius: 4px; cursor: pointer;
         user-select: none; transition: box-shadow .1s, background .1s; }
.pt-el:hover { box-shadow: 0 0 0 2px rgba(31, 78, 121, .5); }
.pt-el.pt-selected { background: #1f4e79; color: #fff; border-color: #1f4e79;
                     box-shadow: 0 1px 3px rgba(0, 0, 0, .3); }
.pt-el.pt-empty { opacity: .3; }
.pt-series { height: 24px; display: flex; align-items: center; justify-content: center;
             font-size: 9px; color: #8a96a3; border: 1px dashed #c9d0d8; border-radius: 4px; }
.pt-gap { grid-column: 1 / span 18; height: 4px; }
.pt-legend { display: flex; flex-wrap: wrap; justify-content: center; gap: .25rem .9rem;
             margin-top: .5rem; font-size: 11px; color: #52606d; }
.pt-legend span::before { content: ''; display: inline-block; width: 11px; height: 11px;
                          margin-right: 4px; border-radius: 2px; vertical-align: -1px;
                          background: var(--pt-bg); border: 1px solid rgba(0, 0, 0, .18); }
.bslib-sidebar-layout > .sidebar .accordion-button { font-weight: 600; font-size: .85rem; }
.bslib-sidebar-layout > .sidebar .form-label { font-size: .8rem; color: #52606d; }
.pt-card.pt-collapsed .pt-body { display: none; }
.lf-main.html-fill-container { flex-direction: row; gap: 1rem; min-height: 380px; }
.lf-main > .card { margin-bottom: 0; }
.lf-main > .card.bp-card { flex: 0 0 410px; }
.lf-main.bp-hidden > .card.bp-card { display: none; }
@media (max-width: 991.98px) {
  .lf-main.html-fill-container { flex-direction: column; }
  .lf-main > .card.bp-card { flex: 0 0 auto; }
}
.bp-status { font-size: .8rem; color: #52606d; margin-bottom: .25rem; }
.bp-status strong { color: #1f2933; font-size: .95rem; }
.bp-table { font-size: .75rem; overflow-y: auto; }
.bp-table table { margin-bottom: 0; }
.bp-table td, .bp-table th { padding: .15rem .35rem !important; white-space: nowrap; }
.lf-buttons { display: flex; gap: .4rem; margin-bottom: .75rem; }
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
// show or hide the periodic table; the plot then resizes to the free space
$(document).on('click', '#pt-toggle', function() {
  var card = $('.pt-card').toggleClass('pt-collapsed');
  var hidden = card.hasClass('pt-collapsed');
  $(this).text(hidden ? 'Show table' : 'Hide table').attr('aria-expanded', !hidden);
  setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 50);
});
// show or hide the Boltzmann plot beside the spectrum
function lfBoltzmann(show) {
  $('.lf-main').toggleClass('bp-hidden', !show);
  $('#bp-toggle').toggleClass('active', show).attr('aria-pressed', show);
  setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 50);
}
$(document).on('click', '#bp-toggle', function() {
  lfBoltzmann($('.lf-main').hasClass('bp-hidden'));
});
Shiny.addCustomMessageHandler('bp-show', function(x) { lfBoltzmann(true); });
"

periodic_table_ui <- function() {
  pt <- periodic_table()
  cell <- function(class, row, col, bg = NULL, ...) {
    shiny::tags$div(class = class,
                    style = sprintf("grid-row: %d; grid-column: %d;%s", row, col,
                                    if (is.null(bg)) "" else paste0(" --pt-bg: ", bg, ";")), ...)
  }
  cells <- lapply(seq_len(nrow(pt)), function(i) {
    cell("pt-el", pt$row[i], pt$col[i], finder_categories[[pt$category[i]]],
         `data-el` = pt$symbol[i], title = sprintf("%s (Z = %d), %s", pt$symbol[i], pt$z[i], pt$category[i]),
         pt$symbol[i])
  })
  legend <- lapply(names(finder_categories), function(cat) {
    shiny::tags$span(style = paste0("--pt-bg: ", finder_categories[[cat]], ";"), cat)
  })
  shiny::tagList(
    shiny::tags$div(class = "pt-grid", cells,
                    cell("pt-series", 6, 3, NULL, "57-71"), cell("pt-series", 7, 3, NULL, "89-103"),
                    shiny::tags$div(class = "pt-gap", style = "grid-row: 8;")),
    shiny::tags$div(class = "pt-legend", legend)
  )
}

finder_header <- function(title, status, ...) {
  bslib::card_header(class = "lf-header", shiny::tags$span(title),
                     shiny::tags$div(class = "lf-actions", status, ...))
}

finder_ui <- function(data, show_table = TRUE) {
  range_wl <- range(data$wavelength)
  small_button <- "btn-sm btn-outline-secondary"
  bslib::page_sidebar(
    title = shiny::tags$span(class = "lf-title", "specProc",
                             shiny::tags$span(class = "lf-sub", "LIBS line finder")),
    window_title = "specProc line finder",
    theme = bslib::bs_theme(version = 5, primary = finder_primary, "font-size-base" = "0.875rem"),
    shiny::tags$head(shiny::tags$style(shiny::HTML(finder_css)),
                     shiny::tags$script(shiny::HTML(finder_js))),
    sidebar = bslib::sidebar(
      width = 290,
      bslib::accordion(
        multiple = TRUE, open = c("Spectrum", "Lines"),
        bslib::accordion_panel(
          "Spectrum",
          shiny::selectInput("spectrum", "Show", choices = finder_choices(data)),
          shiny::sliderInput("wl_range", "Wavelength range (nm)", min = floor(range_wl[1]),
                             max = ceiling(range_wl[2]),
                             value = c(floor(range_wl[1]), ceiling(range_wl[2])), step = 0.5),
          shiny::numericInput("shift", "Wavelength shift (nm)", value = 0, step = 0.01)
        ),
        bslib::accordion_panel(
          "Lines",
          shiny::checkboxGroupInput("stages", "Ionization stages", choices = c(I = 1, II = 2, III = 3),
                                    selected = c(1, 2), inline = TRUE),
          shiny::sliderInput("temperature", "Temperature (K)", min = 3000, max = 30000,
                             value = 10000, step = 500),
          shiny::numericInput("top", "Lines per species", value = 15, min = 1, max = 500, step = 1),
          shiny::sliderInput("min_relative", "Minimum relative intensity", min = 0, max = 1,
                             value = 0.01, step = 0.01),
          shiny::checkboxInput("scale", "Scale markers by expected intensity", value = TRUE),
          shiny::selectizeInput("molecules", "Molecular bands", multiple = TRUE,
                                choices = unique(molecular_constants()$molecule),
                                options = list(placeholder = "None")),
          shiny::numericInput("band_temperature", "Vibrational temperature of the bands (K)",
                              value = 5000, min = 500, step = 500)
        ),
        bslib::accordion_panel(
          "Boltzmann plot",
          shiny::selectInput("bp_species", "Species", choices = character()),
          shiny::tags$p(class = "small text-muted",
                        "Click the dot of a line marker to add or remove the line. Click a point",
                        "of the Boltzmann plot to leave it out of the fit."),
          shiny::tags$div(class = "lf-buttons",
                          shiny::actionButton("bp_add", "Add shown lines", class = small_button),
                          shiny::actionButton("bp_clear", "Clear", class = small_button)),
          shiny::selectInput("bp_method", "Intensity", choices = c(`Line area` = "area",
                                                                    `Peak height` = "height")),
          shiny::numericInput("bp_half_width", "Integration half-width (nm)", value = 0.15,
                              min = 0.01, step = 0.01),
          shiny::numericInput("bp_search", "Peak search half-width (nm)", value = 0.2, min = 0,
                              step = 0.01),
          shiny::checkboxInput("bp_baseline", "Subtract a linear baseline", value = TRUE),
          shiny::radioButtons("bp_units", "Intensity units", inline = TRUE,
                              choices = c(Energy = "energy", Photons = "photons"))
        ),
        bslib::accordion_panel(
          "Session",
          shiny::tags$p(class = "small text-muted",
                        "Save the lines fetched from NIST to continue offline later."),
          shiny::downloadButton("save_cache", "Save fetched data", class = small_button),
          shiny::fileInput("load_cache", NULL, accept = ".rds", buttonLabel = "Load...",
                           placeholder = "fetched data (.rds)")
        )
      )
    ),
    bslib::card(
      fill = FALSE, class = paste("pt-card", if (!show_table) "pt-collapsed"),
      finder_header("Elements", shiny::textOutput("selection", inline = TRUE),
                    shiny::actionButton("clear", "Clear", class = small_button),
                    shiny::tags$button(id = "pt-toggle", type = "button", class = paste("btn", small_button),
                                       `aria-expanded` = tolower(show_table),
                                       if (show_table) "Hide table" else "Show table")),
      bslib::card_body(class = "pt-body", periodic_table_ui())
    ),
    bslib::as_fill_carrier(shiny::tags$div(
      class = "lf-main bp-hidden",
      bslib::card(
        full_screen = TRUE, min_height = 380,
        finder_header("Spectrum and candidate lines", shiny::textOutput("n_lines", inline = TRUE),
                      shiny::downloadButton("download_lines", "CSV", class = small_button),
                      shiny::tags$button(id = "bp-toggle", type = "button", `aria-pressed` = "false",
                                         class = paste("btn", small_button), "Boltzmann plot")),
        bslib::card_body(padding = c(4, 8), plotly::plotlyOutput("plot", height = "100%"))
      ),
      bslib::card(
        class = "bp-card", full_screen = TRUE,
        finder_header("Boltzmann plot", NULL,
                      shiny::actionButton("bp_use_t", "Use T", class = small_button,
                                          title = "Rank the candidate lines at the fitted temperature"),
                      shiny::downloadButton("bp_download", "CSV", class = small_button),
                      shiny::actionButton("done", "Done", class = "btn-sm btn-primary",
                                          title = "Close the app and return the fit to R")),
        bslib::card_body(
          padding = c(6, 10), gap = 4,
          shiny::uiOutput("bp_status", class = "bp-status"),
          plotly::plotlyOutput("bp_plot", height = "260px"),
          shiny::tags$div(class = "bp-table", shiny::tableOutput("bp_table"))
        )
      )
    ))
  )
}

# Plot styling of the app.
finder_axis <- list(gridcolor = "#eef1f5", zeroline = FALSE, showline = TRUE, linecolor = "#c3cad4",
                    ticks = "outside", tickcolor = "#c3cad4")
finder_font <- list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif", size = 12,
                    color = "#1f2933")

finder_layout <- function(p, uirevision, empty) {
  axis <- finder_axis
  p <- plotly::layout(
    p, uirevision = uirevision, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "#fff",
    font = finder_font,
    margin = list(l = 64, r = 16, t = 28, b = 48),
    legend = list(orientation = "h", x = 1, xanchor = "right", y = 1, yanchor = "bottom",
                  bgcolor = "rgba(0,0,0,0)"),
    xaxis = c(list(title = "Wavelength (nm)"), axis), yaxis = c(list(title = "Intensity"), axis),
    annotations = if (empty) list(list(
      text = "Select elements in the periodic table to overlay their lines", showarrow = FALSE,
      xref = "paper", yref = "paper", x = 0.5, y = 0.98, font = list(color = "#8a96a3")
    ))
  )
  plotly::config(p, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"),
                 toImageButtonOptions = list(format = "svg", filename = "libs_lines"))
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
      spec <- as_spectrum(spectrum())
      wl_range <- input$wl_range %||% fetch_range
      spec <- spec[spec$wavelength >= wl_range[1] & spec$wavelength <= wl_range[2], ]
      l <- lines()
      segments <- line_segments(l, spec, shift = finder_number(input$shift, 0),
                                scale_markers = isTRUE(input$scale %||% TRUE))
      p <- lines_plotly(spec, segments, source = "spectrum", heads = TRUE)
      band_segs <- band_segments(bands(), spec, shift = finder_number(input$shift, 0),
                                 scale_markers = isTRUE(input$scale %||% TRUE))
      for (trace in band_traces(band_segs)) {
        p <- do.call(plotly::add_trace, c(list(p), trace))
      }
      p <- add_selected_lines(p, bp()$lines, spec)
      p <- plotly::event_register(p, "plotly_click")
      # keep the zoom when the lines change, reset it when the spectrum changes
      finder_layout(p, uirevision = paste(input$spectrum, wl_range, collapse = " "),
                    empty = nrow(l) == 0 && nrow(band_segs) == 0)
    })

    bands <- shiny::reactive({
      molecules <- unlist(input$molecules)
      if (length(molecules) == 0) return(NULL)
      molecular_bands(molecules, wavelength = fetch_range,
                      temperature = finder_number(input$band_temperature, 5000, positive = TRUE),
                      min_relative = input$min_relative %||% 0.01)
    })

    # ---- Boltzmann plot ----
    bp_keys <- shiny::reactiveVal(character())      # lines of the plot
    bp_excluded <- shiny::reactiveVal(character())  # lines left out of the fit

    shiny::observe({
      available <- intersect(species(), names(cache())[vapply(cache(), nrow, integer(1)) > 0])
      current <- shiny::isolate(input$bp_species)
      shiny::updateSelectInput(session, "bp_species", choices = available,
                               selected = if (isTRUE(current %in% available)) current else
                                 utils::head(available, 1))
    })

    bp <- shiny::reactive({
      # the chosen lines of the species, from the fetched lines: they stay when
      # the settings of the candidate lines change
      raw <- if (isTRUE(input$bp_species %in% names(cache()))) cache()[[input$bp_species]]
      chosen <- if (is.null(raw)) empty_line_list() else
        rank_lines(raw[line_key(raw) %in% bp_keys(), ], temperature = 10000, top = Inf,
                   min_relative = 0)
      chosen$key <- line_key(chosen)
      boltzmann_selection(
        spectrum(), chosen, shift = finder_number(input$shift, 0),
        half_width = finder_number(input$bp_half_width, 0.15, positive = TRUE),
        search = finder_number(input$bp_search, 0.2), method = input$bp_method %||% "area",
        baseline = isTRUE(input$bp_baseline %||% TRUE), units = input$bp_units %||% "energy",
        excluded = bp_excluded()
      )
    })

    # a click on the dot of a line marker adds or removes the line
    spectrum_click <- function() plotly::event_data("plotly_click", source = "spectrum", priority = "event")
    boltzmann_click <- function() plotly::event_data("plotly_click", source = "boltzmann", priority = "event")
    shiny::observeEvent(spectrum_click(), {
      key <- clicked_key(spectrum_click())
      if (is.null(key)) return()
      sp <- sub("[|].*", "", key)
      if (!identical(input$bp_species, sp)) shiny::updateSelectInput(session, "bp_species", selected = sp)
      bp_keys(toggle_key(bp_keys(), key))
      session$sendCustomMessage("bp-show", TRUE)
    })
    # a click on a point of the Boltzmann plot leaves it out of the fit
    shiny::observeEvent(boltzmann_click(), {
      key <- clicked_key(boltzmann_click())
      if (!is.null(key)) bp_excluded(toggle_key(bp_excluded(), key))
    })
    shiny::observeEvent(input$bp_add, {
      shown <- lines()
      bp_keys(union(bp_keys(), line_key(shown[shown$species %in% input$bp_species, ])))
      session$sendCustomMessage("bp-show", TRUE)
    })
    shiny::observeEvent(input$bp_clear, {
      mine <- startsWith(bp_keys(), paste0(input$bp_species, "|"))
      bp_excluded(setdiff(bp_excluded(), bp_keys()[mine]))
      bp_keys(bp_keys()[!mine])
    })
    shiny::observeEvent(input$bp_use_t, {
      fit <- bp()$fit
      if (is.null(fit)) return()
      shiny::updateSliderInput(session, "temperature",
                               value = min(30000, max(3000, round(fit$temperature / 500) * 500)))
    })
    shiny::observeEvent(input$done, shiny::stopApp(bp()$fit))

    output$bp_plot <- plotly::renderPlotly(boltzmann_plotly(bp()))
    # rendered while hidden, so that its click events are registered
    shiny::outputOptions(output, "bp_plot", suspendWhenHidden = FALSE)
    output$bp_status <- shiny::renderUI(boltzmann_status(bp()))
    output$bp_table <- shiny::renderTable(boltzmann_table(bp()$lines), align = "r",
                                          striped = TRUE, hover = TRUE, spacing = "xs")
    output$bp_download <- shiny::downloadHandler(
      filename = function() "boltzmann_lines.csv",
      content = function(file) {
        out <- bp()$lines
        utils::write.csv(out[setdiff(names(out), c("key", "x", "y", "y_se"))], file, row.names = FALSE)
      }
    )

    output$selection <- shiny::renderText({
      elements <- unlist(input$elements)
      if (length(elements) == 0) return("No element selected")
      count <- paste(length(elements), if (length(elements) == 1) "element" else "elements")
      # the names stay visible when the table is hidden
      shown <- sort(elements)
      if (length(shown) > 8) shown <- c(shown[1:8], "...")
      paste0(count, ": ", paste(shown, collapse = ", "))
    })

    output$n_lines <- shiny::renderText({
      n <- nrow(lines())
      text <- paste(n, if (n == 1) "line" else "lines")
      nb <- if (is.null(bands())) 0 else nrow(bands())
      if (nb > 0) text <- paste0(text, ", ", nb, if (nb == 1) " band" else " bands")
      text
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

    invisible(list(lines = lines, cache = cache, species = species, bp = bp, bands = bands))
  }
}

# ---- Boltzmann plot ----------------------------------------------------------

# A numeric input, or its default when empty (or not positive).
finder_number <- function(x, default, positive = FALSE) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || (positive && x <= 0)) default else x
}

toggle_key <- function(keys, key) {
  if (key %in% keys) setdiff(keys, key) else c(keys, key)
}

# The key of the first clicked point that has one.
clicked_key <- function(event) {
  keys <- unlist(event$customdata)
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (length(keys) == 0) NULL else keys[[1]]
}

# Measures the lines in the spectrum and fits the Boltzmann plot of those
# not excluded: the lines with their key, point (x, y and the standard error
# of y), whether they are `included` and their residual, the fit (or NULL),
# and a message when there is no fit.
boltzmann_selection <- function(spectrum, lines, shift = 0, half_width = 0.15, search = 0.2,
                                method = "area", baseline = TRUE, units = "energy",
                                excluded = character()) {
  if (nrow(lines) == 0) {
    return(list(lines = NULL, fit = NULL, units = units,
                message = "Click the dots of line markers, or use Add shown lines."))
  }
  centers <- data.frame(wavelength = lines$wavelength + shift)
  measured <- line_intensities(spectrum, centers, half_width = half_width, search = search,
                               method = method, baseline = baseline)
  out <- lines[setdiff(names(lines), "relative_intensity")]
  out$peak_wavelength <- measured$peak_wavelength
  out$shift <- measured$peak_wavelength - lines$wavelength
  for (col in c("height", "intensity", "intensity_se", "snr", "detected")) out[[col]] <- measured[[col]]
  usable <- is.finite(out$intensity) & out$intensity > 0
  out$x <- out$Ek
  out$y <- NA_real_
  out$y[usable] <- boltzmann_ordinate(out[usable, ], units)
  out$y_se <- out$intensity_se / out$intensity
  out$included <- usable & !out$key %in% excluded
  out$residual <- NA_real_
  out <- out[order(out$Ek, out$wavelength), ]
  fit <- NULL
  message <- NULL
  if (sum(out$included) < 3) {
    message <- sprintf("%d line%s in the fit; at least 3 are needed.", sum(out$included),
                       if (sum(out$included) == 1) "" else "s")
  } else {
    used <- out[out$included, setdiff(names(out), c("key", "x", "y", "y_se", "included", "residual"))]
    fit <- tryCatch(boltzmann(used, units = units), error = function(e) conditionMessage(e))
    if (is.character(fit)) {
      message <- if (length(unique(used$Ek)) < 2) "The lines share one upper level; add lines of other levels." else fit
      fit <- NULL
    } else {
      cf <- stats::coef(fit$fit)
      out$residual <- out$y - (cf[[1]] + cf[[2]] * out$x)
    }
  }
  list(lines = out, fit = fit, units = units, message = message)
}

boltzmann_status <- function(sel) {
  if (is.null(sel$fit)) {
    return(shiny::tags$span(sel$message))
  }
  fit <- sel$fit
  shiny::tagList(
    shiny::tags$div(shiny::tags$strong(sprintf("T = %s \u00b1 %s K",
                                               format(round(fit$temperature), big.mark = ","),
                                               format(round(fit$temperature_se), big.mark = ",")))),
    shiny::tags$div(sprintf("R\u00b2 = %.3f, %d of %d lines in the fit", fit$r_squared,
                            nrow(fit$points), nrow(sel$lines)))
  )
}

boltzmann_plotly <- function(sel) {
  pts <- sel$lines
  p <- plotly::plot_ly(source = "boltzmann")
  ylab <- if (identical(sel$units, "photons")) "ln(I / g<sub>k</sub>A<sub>ki</sub>)" else
    "ln(I\u03bb / g<sub>k</sub>A<sub>ki</sub>)"
  if (!is.null(pts)) pts <- pts[is.finite(pts$y), ]
  if (!is.null(sel$fit) && nrow(pts) > 0) {
    cf <- stats::coef(sel$fit$fit)
    xs <- range(pts$x)
    p <- plotly::add_trace(p, x = xs, y = cf[[1]] + cf[[2]] * xs, type = "scatter", mode = "lines",
                           line = list(color = "#8a96a3", width = 1.5), hoverinfo = "skip",
                           name = "fit")
  }
  if (!is.null(pts) && nrow(pts) > 0) {
    text <- sprintf(
      "%s %.3f nm<br>E<sub>k</sub> = %.3f eV, g<sub>k</sub>A<sub>ki</sub> = %.2e s<sup>-1</sup><br>SNR %.0f%s<br>%s",
      pts$species, pts$wavelength, pts$Ek, pts$gk * pts$Aki, pts$snr,
      ifelse(is.finite(pts$residual), sprintf(", residual %+.2f", pts$residual), ""),
      ifelse(pts$included, "click to leave out of the fit", "left out: click to use")
    )
    for (used in c(TRUE, FALSE)) {
      k <- pts$included == used
      if (!any(k)) next
      p <- plotly::add_trace(
        p, x = pts$x[k], y = pts$y[k], customdata = pts$key[k], text = text[k],
        type = "scatter", mode = "markers", hoverinfo = "text",
        name = if (used) "in the fit" else "left out",
        error_y = list(type = "data", array = pts$y_se[k], color = "#c3cad4", thickness = 1, width = 0),
        marker = if (used) list(color = finder_primary, size = 9) else
          list(color = "#fff", size = 9, line = list(color = "#8a96a3", width = 1.5))
      )
    }
  }
  if (is.null(pts) || nrow(pts) == 0) {
    p <- plotly::add_trace(p, x = numeric(0), y = numeric(0), type = "scatter", mode = "markers",
                           hoverinfo = "skip")
  }
  p <- plotly::layout(
    p, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "#fff", font = finder_font,
    margin = list(l = 56, r = 8, t = 8, b = 44), showlegend = FALSE,
    uirevision = if (is.null(pts) || nrow(pts) == 0) "" else pts$species[1],
    xaxis = c(list(title = "Upper-level energy (eV)"), finder_axis),
    yaxis = c(list(title = ylab), finder_axis),
    annotations = if (is.null(pts) || nrow(pts) == 0) list(list(
      text = "Pick lines on the spectrum", showarrow = FALSE, xref = "paper", yref = "paper",
      x = 0.5, y = 0.5, font = list(color = "#8a96a3")
    ))
  )
  p <- plotly::event_register(p, "plotly_click")
  plotly::config(p, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"),
                 toImageButtonOptions = list(format = "svg", filename = "boltzmann_plot"))
}

boltzmann_table <- function(lines) {
  if (is.null(lines) || nrow(lines) == 0) return(NULL)
  data.frame(
    `Line (nm)` = sprintf("%.3f", lines$wavelength),
    `Ek (eV)` = sprintf("%.3f", lines$Ek),
    `gA (s-1)` = sprintf("%.2e", lines$gk * lines$Aki),
    SNR = sprintf("%.0f", lines$snr),
    Residual = ifelse(is.finite(lines$residual), sprintf("%+.2f", lines$residual), ""),
    Fit = ifelse(lines$included, "yes", "no"),
    check.names = FALSE
  )
}

# Markers of the lines of the Boltzmann plot on the spectrum, at their peak.
add_selected_lines <- function(p, lines, spec) {
  if (is.null(lines) || nrow(lines) == 0) return(p)
  x <- ifelse(is.na(lines$peak_wavelength), lines$wavelength, lines$peak_wavelength)
  inside <- x >= min(spec$wavelength) & x <= max(spec$wavelength)
  if (!any(inside)) return(p)
  # the lines in the fit first: the legend shows the symbol of the first
  keep <- which(inside)[order(!lines$included[inside])]
  lines <- lines[keep, ]
  x <- x[keep]
  y <- stats::approx(spec$wavelength, spec$intensity, xout = x)$y
  plotly::add_trace(
    p, x = x, y = y, customdata = lines$key, type = "scatter", mode = "markers",
    name = "Boltzmann plot", hoverinfo = "text",
    text = sprintf("%s %.3f nm, in the Boltzmann plot<br>click to remove", lines$species,
                   lines$wavelength),
    marker = list(size = 10, symbol = ifelse(lines$included, "diamond", "diamond-open"),
                  color = finder_primary, line = list(color = finder_primary, width = 1.5))
  )
}
