#' @title Plotting of Spectra
#'
#' @description
#' Spectrum plots are commonly x–y plots in which the x-axis represents the
#' wavelength and the y-axis represents intensity of a spectrum's signal.
#' The function allows to plot a spectrum or several spectra in a single plot,
#' identified either by an id (for example, the samples or spectra id) or by a
#' target variable (for example, the concentration of a chemical element).
#' The spectra can be split into panels by a grouping variable.
#'
#' @author Christian L. Goueguel
#'
#' @details
#' This function is based on the ggplot2 package, thus allowing users to easily
#' add or modify different components of the plot. Each row of `x` is drawn as
#' a separate line. All columns other than `id`, `colvar` and `panel` must be
#' named by their wavelength.
#'
#' With `color_as = "fill"`, the area between each spectrum and its baseline
#' (zero intensity, shifted by `offset`) is filled, with the spectrum drawn as
#' a thin dark line. Overlaid spectra are filled with translucent colors.
#' Stacked spectra (with a vertical `offset`) are filled with opaque colors and
#' drawn from the back to the front (from the top to the bottom of the stack),
#' so that the front spectra hide the back ones, as in a waterfall plot.
#'
#' @param x data frame or tibble of the spectra.
#' @param id optional (`NULL` by default). Column name of a factor variable
#' that identified each spectrum.
#' @param colvar optional (`NULL` by default). Column name of a numeric
#' variable to be display in color scale.
#' @param .interactive optional (`FALSE` by default). When set to `TRUE`
#' enables interactive plot.
#' @param drop_na Optional (`FALSE` by default). Remove rows with NA intensity
#' if drop_na is `TRUE`.
#' @param offset Optional (`NULL` by default). The offsets between successive
#'   spectra, to separate them: a number (vertical offset, in intensity
#'   units), or two numbers `c(horizontal, vertical)` (in nm and intensity
#'   units). The spectrum of row `i` is shifted by `(i - 1)` times the
#'   offsets, so that the first one stays in place. With `panel`, the rows
#'   are counted in each panel, so that the first spectrum of every panel
#'   stays in place.
#' @param panel Optional (`NULL` by default). Column name of a grouping
#'   variable: the spectra of each group are drawn in a separate panel, with
#'   the same axes.
#' @param layout The arrangement of the panels: `"vertical"` (default), one
#'   above the other, or `"horizontal"`, side by side. Ignored without
#'   `panel`.
#' @param grid A logical: draw grid lines at the major breaks of the axes
#'   (`FALSE`, default).
#' @param color_as How the colors of `colvar` (or of `id`, without `colvar`)
#'   are shown: as the color of the lines (`"line"`, default), or as the fill
#'   of the area under each spectrum (`"fill"`).
#'
#' @return Object of class ggplot or of class plotly if `.interactive = TRUE`.
#'
#' @export plot_spectra
#'
#' @examples
#' data(forageLIBS)
#' wl <- as.numeric(names(forageLIBS)[-(1:14)])
#' k_lines <- names(forageLIBS)[-(1:14)][wl > 764 & wl < 772]
#' # the K I resonance lines, colored by potassium content
#' plot_spectra(forageLIBS[1:20, c("K", k_lines)], colvar = K)
#'
#' # five spectra stacked, each shifted up by 20000 counts and right by 0.5 nm
#' plot_spectra(forageLIBS[1:5, c("Measurement", k_lines)], id = Measurement,
#'              offset = c(0.5, 20000))
#'
#' # one panel per potassium level, side by side, with the area under the
#' # spectra filled by potassium content and grid lines
#' spectra <- forageLIBS[1:30, c("K", k_lines)]
#' spectra$level <- cut(spectra$K, 3, labels = c("Low K", "Medium K", "High K"))
#' plot_spectra(spectra, colvar = K, panel = level, layout = "horizontal",
#'              offset = c(0.2, 8000), color_as = "fill", grid = TRUE)
plot_spectra <- function(x, id = NULL, colvar = NULL, .interactive = FALSE, drop_na = FALSE,
                         offset = NULL, panel = NULL, layout = c("vertical", "horizontal"),
                         grid = FALSE, color_as = c("line", "fill")) {
  if (missing(x)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(x)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  id_quo <- rlang::enquo(id)
  col_quo <- rlang::enquo(colvar)
  panel_quo <- rlang::enquo(panel)
  id_name <- if (rlang::quo_is_null(id_quo)) NULL else rlang::as_name(id_quo)
  col_name <- if (rlang::quo_is_null(col_quo)) NULL else rlang::as_name(col_quo)
  panel_name <- if (rlang::quo_is_null(panel_quo)) NULL else rlang::as_name(panel_quo)
  if (!is.null(id_name) && !id_name %in% colnames(x)) {
    stop("The 'id' column does not exist in the provided data.")
  }
  if (!is.null(col_name) && !col_name %in% colnames(x)) {
    stop("The 'colvar' column does not exist in the provided data.")
  }
  if (!is.null(panel_name) && !panel_name %in% colnames(x)) {
    stop("The 'panel' column does not exist in the provided data.")
  }
  if (!is.logical(.interactive)) {
    stop("The argument '.interactive' must be of type boolean (TRUE or FALSE)")
  }
  if (!is.logical(drop_na)) {
    stop("The argument 'drop_na' must be of type boolean (TRUE or FALSE)")
  }
  if (!is.logical(grid) || length(grid) != 1 || is.na(grid)) {
    stop("The argument 'grid' must be TRUE or FALSE.", call. = FALSE)
  }
  layout <- match.arg(layout)
  color_as <- match.arg(color_as)
  if (!is.null(offset)) {
    if (!is.numeric(offset) || !length(offset) %in% 1:2 || anyNA(offset)) {
      stop("'offset' must be a number (vertical offset) or two numbers c(horizontal, vertical).",
           call. = FALSE)
    }
    offset <- if (length(offset) == 1) c(0, offset) else offset
  }

  spec_cols <- setdiff(names(x), c(id_name, col_name, panel_name))
  wl <- parse_wavelength(spec_cols)
  if (anyNA(wl)) {
    stop("Spectral column names must be wavelengths; use 'id', 'colvar' and 'panel' for the other columns.")
  }

  intensity <- wavelength <- .draw <- .base <- NULL

  n <- nrow(x)
  # the position of each spectrum in its panel, from which it is offset
  position <- if (is.null(panel_name)) {
    seq_len(n)
  } else {
    stats::ave(seq_len(n), match(x[[panel_name]], unique(x[[panel_name]])), FUN = seq_along)
  }
  fill <- color_as == "fill"
  x_long <- x
  x_long$.shift <- position - 1
  # filled spectra are drawn from the back (the top of a stack) to the front
  x_long$.draw <- if (fill && (is.null(offset) || offset[2] >= 0)) n + 1 - seq_len(n) else seq_len(n)
  x_long <- tidyr::pivot_longer(
    x_long,
    cols = dplyr::all_of(spec_cols),
    names_to = "wavelength",
    values_to = "intensity"
  )
  x_long$wavelength <- wl[match(x_long$wavelength, spec_cols)]

  if (drop_na) {
    x_long <- x_long[!is.na(x_long$intensity), ]
  }
  x_long$.base <- 0
  if (!is.null(offset)) {
    x_long$wavelength <- x_long$wavelength + x_long$.shift * offset[1]
    x_long$.base <- x_long$.shift * offset[2]
    x_long$intensity <- x_long$intensity + x_long$.base
  }

  p <- ggplot2::ggplot(x_long) +
    ggplot2::aes(x = wavelength, y = intensity, group = .draw)

  colour_by <- if (!is.null(col_name)) {
    rlang::quo(.data[[!!col_name]])
  } else if (!is.null(id_name)) {
    rlang::quo(factor(.data[[!!id_name]]))
  }
  if (fill) {
    # opaque fills when the spectra are stacked, translucent when they overlap
    stacked <- !is.null(offset) && offset[2] != 0
    alpha <- if (stacked) 1 else 0.6
    p <- p + if (is.null(colour_by)) {
      # #667f97 is #002a52 at alpha 0.6 on white
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .base, ymax = intensity),
                           fill = if (stacked) "#667f97" else "#002a52", alpha = alpha,
                           colour = "#002a52", linewidth = 0.3, outline.type = "upper")
    } else {
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .base, ymax = intensity, fill = !!colour_by),
                           alpha = alpha, colour = "grey20", linewidth = 0.3, outline.type = "upper")
    }
  } else {
    p <- p + if (is.null(colour_by)) {
      ggplot2::geom_line(color = "#002a52")
    } else {
      ggplot2::geom_line(ggplot2::aes(colour = !!colour_by))
    }
  }
  aesthetic <- if (fill) "fill" else "colour"
  if (!is.null(col_name)) {
    p <- p + ggplot2::scale_colour_gradient(low = "blue", high = "red", aesthetics = aesthetic)
  } else if (!is.null(id_name)) {
    p <- p + ggplot2::scale_colour_viridis_d(direction = -1, aesthetics = aesthetic)
  }
  if (!is.null(colour_by)) {
    p <- p + ggplot2::labs(!!aesthetic := col_name %||% id_name)
  }

  p <- p +
    ggplot2::labs(x = "Wavelength (nm)", y = "Intensity (arb. units)") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      axis.line = ggplot2::element_line(color = "#4b4b4b", linewidth = 1)
    )

  if (!is.null(panel_name)) {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(.data[[!!panel_name]]),
                          ncol = if (layout == "vertical") 1 else NULL,
                          nrow = if (layout == "horizontal") 1 else NULL,
                          axes = "all", axis.labels = "margins") +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
                     strip.text = ggplot2::element_text(hjust = 0))
  }
  if (grid) {
    p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.3))
  }

  if (.interactive == FALSE) {
    return(p)
  }
  rlang::check_installed("plotly", reason = "to create interactive plots.")
  return(plotly::ggplotly(p, tooltip = "all"))
}
