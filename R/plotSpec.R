#' @title Visualization of LIBS Spectra
#' @description
#' @author Christian L. Goueguel
#' @details This function is based on the ggplot2 package, so that users can add or modified its components.
#' @param data Data frame of LIBS spectra.
#' @param id Column of the data frame that identified each spectrum by an unique name (optional).
#' @param colvar Column of the data frame specifying a variable to be display in color scale (optional).
#' @return Plot of LIBS spectrum or spectra.
#' @export plotSpec
plotSpec <- function(data, id = NULL, colvar = NULL) {

  if (length(data) == 0 | is.null(data) == TRUE) {
    stop("Seems you forgot to provide spectra data.")
  }

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(magrittr, tidyr, dplyr, purrr, ggplot2)

  if (is.null(id) == TRUE & is.null(colvar) == TRUE) {
    data %>%
      pivot_longer(cols = everything(), names_to = "wavelength", values_to = "intensity") %>%
      modify_at("wavelength", as.numeric) %>%
      ggplot(aes(x = wavelength, y = intensity)) +
      geom_line(colour = "darkblue") +
      labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      theme_classic() +
      theme(legend.position = "none", axis.line = element_line(colour = "grey50", size = 1))
  }

  if (is.null(id) == FALSE & is.null(colvar) == TRUE) {
    data %>%
      select(id) %>%
      pivot_longer(cols = !id, names_to = "wavelength", values_to = "intensity") %>%
      modify_at("wavelength", as.numeric) %>%
      ggplot(aes(x = wavelength, y = intensity)) +
      geom_line(aes(colour = id, group = id)) +
      scale_colour_gradient(low = "blue", high = "red") +
      labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      theme_classic() +
      theme(legend.position = "none", axis.line = element_line(colour = "grey50", size = 1))
  }

  if (is.null(id) == TRUE & is.null(colvar) == FALSE) {
    data %>%
      select(colvar) %>%
      pivot_longer(cols = !colvar, names_to = "wavelength", values_to = "intensity") %>%
      modify_at("wavelength", as.numeric) %>%
      ggplot(aes(x = wavelength, y = intensity)) +
      geom_line(aes(colour = colvar, group = colvar)) +
      scale_colour_gradient(low = "blue", high = "red") +
      labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      theme_classic() +
      theme(legend.position = "none", axis.line = element_line(colour = "grey50", size = 1))
  }

  if (is.null(id) == FALSE & is.null(colvar) == FALSE) {
    data %>%
      select(id, colvar) %>%
      pivot_longer(cols = !c(id, colvar), names_to = "wavelength", values_to = "intensity") %>%
      modify_at("wavelength", as.numeric) %>%
      ggplot(aes(x = wavelength, y = intensity)) +
      geom_line(aes(colour = colvar, group = id)) +
      scale_colour_gradient(low = "blue", high = "red") +
      labs(x = "Wavelength [nm]", y = "Intensity [arb. units]") +
      theme_classic() +
      theme(legend.position = "right", axis.line = element_line(colour = "grey50", size = 1))
  }

  }
