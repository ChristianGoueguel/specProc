#' @title Spectral Data Plot
#' @description Spectrum plots are commonly x–y plots in which the x-axis represents the wavelength and the y-axis represents intensity of a spectrum's signal. The function allows to plot a spectrum or several spectra in a single plot, identified either by an id (for example, the samples or spectra id) or by a target variable (for example, the concentration of a chemical element) .
#' @author Christian L. Goueguel
#' @details This function is based on the ggplot2 package, thus allowing users to easily add or modify different components of the plot.
#' @param .data Data frame of emission spectra.
#' @param id Optional factor variable that identified each spectrum  (`NULL` by default).
#' @param colvar Optional numeric variable to be display in color scale (`NULL` by default).
#' @param .interactive Optional interactive plot (`FALSE` by default).
#' @return ggplot2::ggplot object or a plotly object if `.interactive = TRUE`.
#' @export plotSpec
plotSpec <- function(.data, id = NULL, colvar = NULL, .interactive = FALSE) {
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("plotly", quietly = TRUE)

  # check input validity
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (!is.null(id) && !(id %in% colnames(.data))) {
    stop("The 'id' column does not exist in the provided data.")
  }
  if (!is.null(colvar) && !(colvar %in% colnames(.data))) {
    stop("The 'colvar' column does not exist in the provided data.")
  }
  if (!is.logical(.interactive)) {
    stop("'.interactive' must be of type boolean (TRUE or FALSE)")
  }

  exclude_cols <- c(id, colvar) %>% purrr::discard(is.null)

  aes_params <- list()
  color_params <- NULL

  if (!is.null(id)) {
    aes_params$group <- "id"
    color_params <- list(ggplot2::aes(colour = !!as.name(id)))
  }
  if (!is.null(colvar)) {
    aes_params$colour <- "colvar"
    color_params <- list(ggplot2::aes(colour = !!as.name(colvar)), ggplot2::scale_colour_gradient(low = "blue", high = "red"))
  }

  if (length(exclude_cols) == 0) {
    X <- .data %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "wavelength",
        values_to = "intensity"
      ) %>%
      purrr::modify_at("wavelength", as.numeric)
  } else {
    X <- .data %>%
      tidyr::pivot_longer(
        cols = -dplyr::all_of(exclude_cols),
        names_to = "wavelength",
        values_to = "intensity"
      ) %>%
      purrr::modify_at("wavelength", as.numeric)
  }

  create_plot <- function(plot_data, aes_params, color_params = NULL) {
    p <- plot_data %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = wavelength, y = intensity, !!!aes_params) +
      ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = if (is.null(id) && is.null(colvar)) "none" else "right", axis.line = ggplot2::element_line(colour = "grey50", linewidth = 1))

    if (!is.null(color_params)) {
      p <- p + do.call(ggplot2::geom_line, color_params)
    } else {
      p <- p + ggplot2::geom_line()
    }

    return(p)
  }

  .plot <- create_plot(X, aes_params, color_params)
  if (.interactive == FALSE) {
    return(.plot)
  } else {
    return(plotly::ggplotly(.plot))
  }

}
