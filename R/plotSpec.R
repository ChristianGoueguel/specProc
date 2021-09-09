#' @title Visualization of LIBS Spectra
#' @description Plot LIBS spectrum, or multiple spectra identified by a sample id column or a target variable.
#' @author Christian L. Goueguel
#' @details This function is based on the ggplot2 package, so that users can add or modified its components.
#' @param data Data frame of LIBS spectra.
#' @param id Column of the data frame that identified each spectrum by an unique name (optional).
#' @param colvar Column of the data frame specifying a variable to be display in color scale (optional).
#' @return Plot of LIBS spectrum or spectra.
#' @importFrom utils "globalVariables"
#' @export plotSpec
plotSpec <- function(data, id = NULL, colvar = NULL) {

  if (length(data) == 0 | is.null(data) == TRUE) {
    stop("Seems you forgot to provide spectra data.")
  }

  globalVariables(c("wavelength", "intensity"))

  if (is.null(id) == TRUE & is.null(colvar) == TRUE) {
    data %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "wavelength", values_to = "intensity") %>%
      purrr::modify_at("wavelength", as.numeric) %>%
      ggplot2::ggplot(ggplot2::aes_string("wavelength", "intensity")) +
      ggplot2::geom_line(colour = "darkblue") +
      ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none", axis.line = ggplot2::element_line(colour = "grey50", size = 1))
  }

  if (is.null(id) == FALSE & is.null(colvar) == TRUE) {
    data %>%
      dplyr::select(id) %>%
      tidyr::pivot_longer(cols = !id, names_to = "wavelength", values_to = "intensity") %>%
      purrr::modify_at("wavelength", as.numeric) %>%
      ggplot2::ggplot(ggplot2::aes_string("wavelength", "intensity")) +
      ggplot2::geom_line(ggplot2::aes(colour = id, group = id)) +
      ggplot2::scale_colour_gradient(low = "blue", high = "red") +
      ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none", axis.line = ggplot2::element_line(colour = "grey50", size = 1))
  }

  if (is.null(id) == TRUE & is.null(colvar) == FALSE) {
    data %>%
      dplyr::select(colvar) %>%
      tidyr::pivot_longer(cols = !colvar, names_to = "wavelength", values_to = "intensity") %>%
      purrr::modify_at("wavelength", as.numeric) %>%
      ggplot2::ggplot(ggplot2::aes_string("wavelength", "intensity")) +
      ggplot2::geom_line(ggplot2::aes(colour = colvar, group = colvar)) +
      ggplot2::scale_colour_gradient(low = "blue", high = "red") +
      ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none", axis.line = ggplot2::element_line(colour = "grey50", size = 1))
  }

  if (is.null(id) == FALSE & is.null(colvar) == FALSE) {
    data %>%
      dplyr::select(id, colvar) %>%
      tidyr::pivot_longer(cols = !c(id, colvar), names_to = "wavelength", values_to = "intensity") %>%
      purrr::modify_at("wavelength", as.numeric) %>%
      ggplot2::ggplot(ggplot2::aes_string("wavelength", "intensity")) +
      ggplot2::geom_line(ggplot2::aes(colour = colvar, group = id)) +
      ggplot2::scale_colour_gradient(low = "blue", high = "red") +
      ggplot2::labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "right", axis.line = ggplot2::element_line(colour = "grey50", size = 1))
  }

  }
