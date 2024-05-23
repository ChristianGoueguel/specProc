#' @title Plotting of Fitted Spectral Line
#' @author Christian L. Goueguel
#' @param data Data to be displayed from the `peakfit` function
#' @param title Plot title
#' @param pt.size Size of original data point
#' @param pt.colour Colour of original data points
#' @param pt.shape Shape of original data points
#' @param pt.fill Colour fill of original data points
#' @param line.size Size of the fitted data line
#' @param line.colour Colour of the fitted data line
#' @param linetype Type of the fitted data line
#' @param resid.shape Shape of the residuals data points
#' @param resid.size Size of the residuals data points
#' @param resid.fill Colour fill of the residuals data points
#' @param resid.colour Colour of the residuals data points
#' @return ggplot2 object
#' @export plotfit
plotfit <- function(data, title = NULL, pt.size = 3, pt.colour = "black", pt.shape = 21, pt.fill = "black", line.size = 1, line.colour = "red", linetype = "solid", resid.shape = 21, resid.size = 2, resid.fill = "blue", resid.colour = "black") {
  if (length(data) == 0 | is.null(data) == TRUE) {
    stop("Seems you forgot to provide spectra data.")
  }
  if (is.data.frame(data) == FALSE) {
    stop("Data must be of class tbl_df, tbl or data.frame")
  }

  x <- NULL
  y <- NULL
  .fitted <- NULL
  .resid <- NULL

  plot1 <- data %>%
    purrr::pluck("augmented") %>%
    as.data.frame() %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y), size = pt.size, colour = pt.colour, shape = pt.shape, fill = pt.fill) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = .fitted), size = line.size, colour = line.colour, linetype = linetype) +
    ggplot2::labs(subtitle = title, x = "Wavelength [nm]", y = "Intensity [arb. units]") +
    ggplot2::theme_bw(base_size = 10)
  plot2 <- data %>%
    purrr::pluck("augmented") %>%
    as.data.frame() %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = .resid)) +
    ggplot2::geom_point(shape = resid.shape, size = resid.size, fill = resid.fill, colour = resid.colour) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x = "Wavelength [nm]", y = "Residual") +
    ggplot2::theme_bw(base_size = 10)
  (plot1 / plot2) + patchwork::plot_layout(ncol = 1, heights = c(5, 1))
}
