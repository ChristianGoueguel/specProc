#' @title Spectral Data Plot
#' @description Spectrum plots are commonly x–y plots in which the x-axis represents the wavelength and the y-axis represents intensity of a spectrum's signal. The function allows to plot a spectrum or several spectra in a single plot, identified either by an id (for example, the samples or spectra id) or by a target variable (for example, the concentration of a chemical element) .
#' @author Christian L. Goueguel
#' @details This function is based on the ggplot2 package, thus allowing users to easily add or modify different components of the plot.
#' @param .data data frame or tibble of the spectra.
#' @param id optional (`NULL` by default). Column name of a factor variable that identified each spectrum.
#' @param colvar optional (`NULL` by default). Column name of a numeric variable to be display in color scale.
#' @param .interactive optional (`FALSE` by default). When set to `TRUE` enables interactive plot.
#' @param drop_na Optional (`FALSE` by default). Remove rows with NA intensity if drop_na is `TRUE`.
#' @return Object of class ggplot or of class plotly if `.interactive = TRUE`.
#' @export plotSpec
plotSpec <- function(.data, id = NULL, colvar = NULL, .interactive = FALSE, drop_na = FALSE) {
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (!rlang::quo_is_null(rlang::enquo(id)) && !(rlang::quo_name(rlang::enquo(id)) %in% colnames(.data))) {
    stop("The 'id' column does not exist in the provided data.")
  }
  if (!rlang::quo_is_null(rlang::enquo(colvar)) && !(rlang::quo_name(rlang::enquo(colvar)) %in% colnames(.data))) {
    stop("The 'colvar' column does not exist in the provided data.")
  }
  if (!is.logical(.interactive)) {
    stop("The argument '.interactive' must be of type boolean (TRUE or FALSE)")
  }
  if (!is.logical(drop_na)) {
    stop("The argument 'drop_na' must be of type boolean (TRUE or FALSE)")
  }

  id_cols <- c(rlang::quo_name(rlang::enquo(id)), rlang::quo_name(rlang::enquo(colvar)))
  id_cols <- id_cols[!is.na(id_cols)]
  id_cols <- tidyselect::all_of(id_cols)
  intensity <- NULL
  wavelength <- NULL

  x_long <- .data %>%
    tidyr::pivot_longer(
      cols = setdiff(names(.data), id_cols),
      names_to = "wavelength",
      values_to = "intensity"
    ) %>%
    purrr::modify_at("wavelength", as.numeric)

  if (drop_na) {
    x_long <- x_long %>%
      dplyr::filter(!is.na(intensity)) %>%
      dplyr::filter(!is.na(wavelength))
  }

  p <- x_long %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = wavelength, y = intensity)

  if (!rlang::quo_is_null(rlang::enquo(id)) && !rlang::quo_is_null(rlang::enquo(colvar))) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = {{id}}, color = {{colvar}})) +
      ggplot2::scale_color_gradient(low = "blue", high = "red")
  } else if (!rlang::quo_is_null(rlang::enquo(id))) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = {{id}})) +
      ggplot2::scale_colour_viridis_d(direction = -1)
  } else if (!rlang::quo_is_null(rlang::enquo(colvar))) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = {{colvar}})) +
      ggplot2::scale_color_gradient(low = "blue", high = "red")
  } else {
    p <- p +
      ggplot2::geom_line(color = "#002a52")
  }

  p <- p +
    ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      axis.line = ggplot2::element_line(color = "#4b4b4b", linewidth = 1)
    )

  if (.interactive == FALSE) {
    return(p)
  } else {
    return(plotly::ggplotly(p, tooltip = "all"))
  }
}
