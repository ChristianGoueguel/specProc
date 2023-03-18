#' @title Spectral Data Plot
#' @description Spectrum plots are commonly x–y plots in which the x-axis represents the wavelength and the y-axis represents intensity of a spectrum's signal. The function allows to plot a spectrum or several spectra in a single plot, identified either by an id (for example, the samples or spectra id) or by a target variable (for example, the concentration of a chemical element) .
#' @author Christian L. Goueguel
#' @details This function is based on the ggplot2 package, thus allowing users to easily add or modify different components of the plot.
#' @param .data Data frame of emission spectra.
#' @param id Factor variable that identified each spectrum  (`NULL` by default).
#' @param colvar Numeric variable to be display in color scale (`NULL` by default).
#' @return ggplot2 object.
#' @importFrom utils "globalVariables"
#' @export plotSpec
plotSpec <- function(.data, id = NULL, colvar = NULL) {

  # check input validity
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (!is.null(id) && !(id %in% colnames(data))) {
    stop("The 'id' column does not exist in the provided data.")
  }
  if (!is.null(colvar) && !(colvar %in% colnames(.data))) {
    stop("The 'colvar' column does not exist in the provided data.")
  }
  if (!is.factor(.data[[id]])) {
    stop("The 'id' column must be a factor.")
  }
  if (!is.numeric(.data[[colvar]])) {
    stop("The 'colvar' column must be a factor.")
  }

  select_cols <- if (is.null(id) && is.null(colvar)) {
    tidyselect::everything()
  } else if (!is.null(id) && is.null(colvar)) {
    !id
  } else if (is.null(id) && !is.null(colvar)) {
    !colvar
  } else {
    !c(id, colvar)
  }

  X <- .data %>%
    tidyr::pivot_longer(
      cols = select_cols,
      names_to = "wavelength",
      values_to = "intensity"
    ) %>%
    purrr::modify_at("wavelength", as.numeric)


  create_plot <- function(plot_data, aes_params, color_params = NULL) {
    p <-
      ggplot2::ggplot(plot_data, ggplot2::aes(.data$wavelength, .data$intensity, !!!aes_params)) +
      ggplot2::geom_line(!!!color_params) +
      ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = if (is.null(id) && is.null(colvar)) "none" else "right", axis.line = ggplot2::element_line(colour = "grey50", size = 1))
    return(p)
  }

  if (is.null(id) && is.null(colvar)) {
    .plot <- create_plot(X, list())
  }
  else if (!is.null(id) && is.null(colvar)) {
    .plot <- create_plot(X, list(colour = "id", group = "id"))
  }
  else if (is.null(id) && !is.null(colvar)) {
    .plot <- create_plot(X, list(colour = "colvar", group = "colvar"), list(ggplot2::aes(colour = colvar), ggplot2::scale_colour_gradient(low = "blue", high = "red")))
  }
  else {
    .plot <- create_plot(X, list(colour = "colvar", group = "id"), list(ggplot2::aes(colour = colvar), ggplot2::scale_colour_gradient(low = "blue", high = "red")))
  }

  return(.plot)
}










# plotSpec <- function(data, id = NULL, colvar = NULL) {
#
#   if (length(data) == 0 | is.null(data) == TRUE) {
#     stop("Seems you forgot to provide spectra data.")
#   }
#
#   if (is.data.frame(data) == FALSE) {
#     stop("Data must be of class tbl_df, tbl or data.frame")
#   }
#
#   globalVariables(names = c("wavelength", "intensity"))
#
#   if (is.null(id) == TRUE & is.null(colvar) == TRUE) {
#     X <- data %>%
#       tidyr::pivot_longer(
#         cols = tidyselect::everything(),
#         names_to = "wavelength",
#         values_to = "intensity"
#         ) %>%
#       purrr::modify_at("wavelength", as.numeric)
#
#       p <- X %>%
#         ggplot2::ggplot(ggplot2::aes_string("wavelength", "intensity")) +
#         ggplot2::geom_line(colour = "darkblue") +
#         ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
#         ggplot2::theme_classic() +
#         ggplot2::theme(legend.position = "none", axis.line = ggplot2::element_line(colour = "grey50", size = 1))
#       return(p)
#   }
#
#   if (is.null(id) == FALSE & is.null(colvar) == TRUE) {
#     X <- data %>%
#       tidyr::pivot_longer(
#         cols = !id,
#         names_to = "wavelength",
#         values_to = "intensity"
#         ) %>%
#       purrr::modify_at("wavelength", as.numeric)
#
#     p <- X %>%
#       ggplot2::ggplot(ggplot2::aes_string("wavelength", "intensity")) +
#       ggplot2::geom_line(ggplot2::aes(colour = id, group = id)) +
#       ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
#       ggplot2::theme_classic() +
#       ggplot2::theme(legend.position = "none", axis.line = ggplot2::element_line(colour = "grey50", size = 1))
#     return(p)
#   }
#
#   if (is.null(id) == TRUE & is.null(colvar) == FALSE) {
#     X <- data %>%
#       tidyr::pivot_longer(
#         cols = !colvar,
#         names_to = "wavelength",
#         values_to = "intensity"
#         ) %>%
#       purrr::modify_at("wavelength", as.numeric)
#
#     p <- X %>%
#       ggplot2::ggplot(ggplot2::aes_string("wavelength", "intensity")) +
#       ggplot2::geom_line(ggplot2::aes(colour = colvar, group = colvar)) +
#       ggplot2::scale_colour_gradient(low = "blue", high = "red") +
#       ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
#       ggplot2::theme_classic() +
#       ggplot2::theme(legend.position = "none", axis.line = ggplot2::element_line(colour = "grey50", size = 1))
#     return(p)
#   }
#
#   if (is.null(id) == FALSE & is.null(colvar) == FALSE) {
#     X <- data %>%
#       tidyr::pivot_longer(
#         cols = !c(id, colvar),
#         names_to = "wavelength",
#         values_to = "intensity"
#         ) %>%
#       purrr::modify_at("wavelength", as.numeric)
#
#     p <- X %>%
#       ggplot2::ggplot(ggplot2::aes_string("wavelength", "intensity")) +
#       ggplot2::geom_line(ggplot2::aes(colour = colvar, group = id)) +
#       ggplot2::scale_colour_gradient(low = "blue", high = "red") +
#       ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
#       ggplot2::theme_classic() +
#       ggplot2::theme(legend.position = "right", axis.line = ggplot2::element_line(colour = "grey50", size = 1))
#     return(p)
#   }
#
# }
