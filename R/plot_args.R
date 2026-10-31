# Arguments shared by the plots of the package, with the same names and
# meanings everywhere:
#  - title, xlab, ylab and base_size on every plot (xlab and ylab default to
#    NULL when the axis titles depend on the data);
#  - point_size and point_alpha on the plots that draw observations as
#    points;
#  - linewidth on the plots that draw spectra or other curves along the
#    wavelength axis, and wavelength = c(min, max) to show a window of it;
#  - interactive on the plots that have a plotly version;
#  - color_by for what colors the points.
# Sizes and widths are in ggplot2 units, as given; where they default to
# NULL, they follow base_size.

check_label <- function(x, arg) {
  ok <- is.null(x) || is.expression(x) || is.call(x) ||
    (is.character(x) && length(x) == 1 && !is.na(x))
  if (!ok) {
    stop("'", arg, "' must be a character string (or NULL).", call. = FALSE)
  }
  invisible(x)
}

check_size <- function(x, arg) {
  check_number(x, arg, lower = 0, lower_open = TRUE)
}

check_alpha <- function(x, arg) {
  check_number(x, arg, lower = 0, upper = 1, lower_open = TRUE)
}

check_colour <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x) ||
      inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")) {
    stop("'", arg, "' must be a color.", call. = FALSE)
  }
  invisible(x)
}

# The text and size arguments common to all the plots.
check_plot_text <- function(title, xlab, ylab, base_size) {
  check_label(title, "title")
  check_label(xlab, "xlab")
  check_label(ylab, "ylab")
  check_size(base_size, "base_size")
  invisible(TRUE)
}

# A wavelength window c(min, max), sorted, or NULL for all the wavelengths.
wavelength_window <- function(wavelength) {
  if (is.null(wavelength)) {
    return(NULL)
  }
  if (!is.numeric(wavelength) || length(wavelength) != 2 || anyNA(wavelength) ||
      wavelength[1] == wavelength[2]) {
    stop("'wavelength' must be a range c(min, max), in nm.", call. = FALSE)
  }
  sort(wavelength)
}

in_window <- function(wl, window) {
  if (is.null(window)) rep(TRUE, length(wl)) else wl >= window[1] & wl <= window[2]
}

# A ggplot2 line width (mm) as a plotly one (px, at 96 dpi).
plotly_width <- function(linewidth) linewidth * 96 / 25.4

# An R color (such as "grey35", unknown to plotly) as a hex code.
plotly_colour <- function(colour) {
  grDevices::rgb(t(grDevices::col2rgb(colour)), maxColorValue = 255)
}
