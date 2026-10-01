#' @title Plotting of Spectra
#'
#' @description
#' Spectrum plots are commonly x–y plots in which the x-axis represents the
#' wavelength and the y-axis represents intensity of a spectrum's signal.
#' The function allows to plot a spectrum or several spectra in a single plot,
#' identified either by an id (for example, the samples or spectra id) or by a
#' target variable (for example, the concentration of a chemical element).
#'
#' @author Christian L. Goueguel
#'
#' @details
#' This function is based on the ggplot2 package, thus allowing users to easily
#' add or modify different components of the plot. Each row of `x` is drawn as
#' a separate line. All columns other than `id` and `colvar` must be named by
#' their wavelength.
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
#'   offsets, so that the first one stays in place.
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
plot_spectra <- function(x, id = NULL, colvar = NULL, .interactive = FALSE, drop_na = FALSE,
                         offset = NULL) {
  if (missing(x)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(x)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  id_quo <- rlang::enquo(id)
  col_quo <- rlang::enquo(colvar)
  id_name <- if (rlang::quo_is_null(id_quo)) NULL else rlang::as_name(id_quo)
  col_name <- if (rlang::quo_is_null(col_quo)) NULL else rlang::as_name(col_quo)
  if (!is.null(id_name) && !id_name %in% colnames(x)) {
    stop("The 'id' column does not exist in the provided data.")
  }
  if (!is.null(col_name) && !col_name %in% colnames(x)) {
    stop("The 'colvar' column does not exist in the provided data.")
  }
  if (!is.logical(.interactive)) {
    stop("The argument '.interactive' must be of type boolean (TRUE or FALSE)")
  }
  if (!is.logical(drop_na)) {
    stop("The argument 'drop_na' must be of type boolean (TRUE or FALSE)")
  }
  if (!is.null(offset)) {
    if (!is.numeric(offset) || !length(offset) %in% 1:2 || anyNA(offset)) {
      stop("'offset' must be a number (vertical offset) or two numbers c(horizontal, vertical).",
           call. = FALSE)
    }
    offset <- if (length(offset) == 1) c(0, offset) else offset
  }

  spec_cols <- setdiff(names(x), c(id_name, col_name))
  wl <- parse_wavelength(spec_cols)
  if (anyNA(wl)) {
    stop("Spectral column names must be wavelengths; use 'id' and 'colvar' for the other columns.")
  }

  intensity <- wavelength <- .spectrum <- NULL

  x_long <- x
  x_long$.spectrum <- seq_len(nrow(x))
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
  if (!is.null(offset)) {
    shift <- x_long$.spectrum - 1
    x_long$wavelength <- x_long$wavelength + shift * offset[1]
    x_long$intensity <- x_long$intensity + shift * offset[2]
  }

  p <- ggplot2::ggplot(x_long) +
    ggplot2::aes(x = wavelength, y = intensity, group = .spectrum)

  if (!is.null(col_name)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = .data[[col_name]])) +
      ggplot2::scale_color_gradient(low = "blue", high = "red")
  } else if (!is.null(id_name)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = factor(.data[[id_name]]))) +
      ggplot2::scale_colour_viridis_d(direction = -1)
  } else {
    p <- p +
      ggplot2::geom_line(color = "#002a52")
  }

  p <- p +
    ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]", color = col_name %||% id_name) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      axis.line = ggplot2::element_line(color = "#4b4b4b", linewidth = 1)
    )

  if (.interactive == FALSE) {
    return(p)
  }
  rlang::check_installed("plotly", reason = "to create interactive plots.")
  return(plotly::ggplotly(p, tooltip = "all"))
}
