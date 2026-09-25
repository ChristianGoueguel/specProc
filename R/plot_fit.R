#' @title Plotting of Fitted Spectral Line
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Plots the data and the fitted lineshape returned by [peak_fit()] or
#' [multipeak_fit()], together with the residuals. The fitted curve is drawn
#' on a fine wavelength grid, so it shows the fitted profile between the
#' measured channels. For multi-peak fits, the
#' individual peak contributions are drawn as dashed lines. When several
#' spectra were fitted, one panel is drawn per spectrum.
#'
#' @param data Data to be displayed from the `peak_fit` or `multipeak_fit` function
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
#' @return A `patchwork` (ggplot2) object.
#' @export plot_fit
#'
#' @examples
#' \donttest{
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   wl <- seq(395, 397, by = 0.02)
#'   set.seed(1)
#'   spec <- 10 + gaussian_profile(wl, y0 = 0, xc = 396.15, wG = 0.2, A = 50) + rnorm(length(wl))
#'   df <- as.data.frame(t(spec))
#'   names(df) <- wl
#'   plot_fit(peak_fit(df, profile = "gaussian"))
#' }
#' }
#'
plot_fit <- function(data, title = NULL, pt.size = 3, pt.colour = "black", pt.shape = 21, pt.fill = "black", line.size = 1, line.colour = "red", linetype = "solid", resid.shape = 21, resid.size = 2, resid.fill = "blue", resid.colour = "black") {
  if (missing(data) || is.null(data) || length(data) == 0) {
    stop("Seems you forgot to provide spectra data.")
  }
  if (!is.data.frame(data) || !"augmented" %in% names(data)) {
    stop("'data' must be the output of peak_fit() or multipeak_fit().")
  }
  rlang::check_installed("patchwork", reason = "to combine the fit and residual plots.")

  id_col <- names(data)[1]
  ok <- !vapply(data$augmented, is.null, logical(1))
  if (!any(ok)) {
    stop("None of the spectra could be fitted.")
  }
  aug <- dplyr::bind_rows(
    lapply(which(ok), function(i) {
      a <- data$augmented[[i]]
      a$.id <- as.character(data[[id_col]][i])
      a
    })
  )
  aug$.id <- factor(aug$.id, levels = unique(aug$.id))

  x <- y <- .fitted <- .resid <- value <- peak <- NULL

  # Smooth fitted curves on a fine wavelength grid: lines are often sampled
  # by only a few channels, so joining the fitted values at the data points
  # would misrepresent the fitted profile.
  curve <- dplyr::bind_rows(lapply(which(ok), function(i) {
    xr <- range(data$augmented[[i]]$x)
    cv <- fit_curve(data$fit[[i]], seq(xr[1], xr[2], length.out = 500))
    cv$.id <- as.character(data[[id_col]][i])
    cv
  }))
  curve$.id <- factor(curve$.id, levels = levels(aug$.id))

  plot1 <- ggplot2::ggplot(aug) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y), size = pt.size, colour = pt.colour, shape = pt.shape, fill = pt.fill)

  peak_cols <- grep("^\\.peak_", names(curve), value = TRUE)
  if (length(peak_cols) > 0) {
    comp <- tidyr::pivot_longer(curve, dplyr::all_of(peak_cols), names_to = "peak", values_to = "value")
    plot1 <- plot1 +
      ggplot2::geom_line(data = comp, ggplot2::aes(x = x, y = value, group = peak), linetype = "dashed", colour = "grey40")
  }

  plot1 <- plot1 +
    ggplot2::geom_line(data = curve, ggplot2::aes(x = x, y = .fitted), linewidth = line.size, colour = line.colour, linetype = linetype) +
    ggplot2::labs(subtitle = title, x = NULL, y = "Intensity [arb. units]") +
    ggplot2::theme_bw(base_size = 10)
  plot2 <- ggplot2::ggplot(aug, ggplot2::aes(x = x, y = .resid)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_point(shape = resid.shape, size = resid.size, fill = resid.fill, colour = resid.colour) +
    ggplot2::labs(x = "Wavelength [nm]", y = "Residual") +
    ggplot2::theme_bw(base_size = 10)

  if (nlevels(aug$.id) > 1) {
    plot1 <- plot1 + ggplot2::facet_wrap(ggplot2::vars(.data$.id), nrow = 1, scales = "free_y")
    plot2 <- plot2 + ggplot2::facet_wrap(ggplot2::vars(.data$.id), nrow = 1, scales = "free_y")
  }

  patchwork::wrap_plots(plot1, plot2, ncol = 1, heights = c(5, 1))
}
