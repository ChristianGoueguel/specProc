#' @title Plotting of Fitted Spectral Line
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Plots the data and the fitted lineshape returned by [peak_fit()] or
#' [multipeak_fit()], together with the residuals, and the fitted parameters
#' of each peak.
#'
#' @details
#' The fitted curve is drawn on a fine wavelength grid, so it shows the
#' fitted profile between the measured channels. For multi-peak fits, the
#' individual peak contributions are drawn as dashed lines, numbered at their
#' maximum. The dotted line is the fitted baseline \eqn{y_0}.
#'
#' **Parameters.** With `annotate = TRUE`, the top of each panel gives, for
#' each peak, its center, full width at half maximum (FWHM) and area, with
#' their standard errors, and the quality of the fit: \eqn{R^2} and the root mean
#' square error (RMSE, the residual standard error). The model is
#' \eqn{y = y_0 + \sum_j A_j f_j(x)}, with \eqn{f_j} of unit area, so that
#' \eqn{A_j} is the area of peak \eqn{j}. The FWHM is `wG` (Gaussian), `wL`
#' (Lorentzian), or for a Voigt or pseudo-Voigt profile the FWHM of the
#' profile from both (Olivero and Longbothum, 1977, see [voigt_fwhm()]), its
#' standard error propagated from the covariance of the estimates. With
#' `show_fwhm = TRUE`, the FWHM of each peak is drawn as a bar at its half
#' maximum. The estimates are rounded to two significant digits of their
#' standard errors; all of them are in the `tidied` column of the fit.
#'
#' **Residuals.** The lower panel gives the residuals, with dashed lines at
#' plus and minus twice the RMSE: a structure in the residuals, beyond these
#' lines or along the wavelength, shows a profile that does not fit the line
#' (such as an asymmetric or self-absorbed line).
#'
#' When several spectra were fitted, each gets its own fit and residual
#' panels, `ncol` per row.
#'
#' @references
#' Olivero, J.J., Longbothum, R.L. (1977). Empirical fits to the Voigt line
#' width: A brief review. Journal of Quantitative Spectroscopy and Radiative
#' Transfer, 17(2):233-236.
#'
#' @param data The output of [peak_fit()] or [multipeak_fit()].
#' @param title The plot title. A long title is split into a title and a
#'   subtitle.
#' @param annotate A logical: write the fitted parameters and the quality of
#'   the fit above each panel (`TRUE`, default).
#' @param show_fwhm A logical: draw the FWHM of each peak as a bar at its
#'   half maximum (`FALSE`, default).
#' @param ncol The number of spectra per row. Default is up to 2.
#' @param xlab,ylab The axis titles. Default are `"Wavelength (nm)"` and
#'   `"Intensity (arb. units)"`. The unit in the parentheses of `xlab` is
#'   that of the center and FWHM in the annotations.
#' @param caption `TRUE` (default), a caption naming the profiles and the
#'   lines drawn; `FALSE`, no caption; or a caption of your own.
#' @param base_size The size of the text, in points. Default is 11; use the
#'   text size of the journal (often 7 to 9 points) for a figure saved at its
#'   printed size.
#' @param point_size The size of the data points (default 1.5).
#' @param point_color The color of the data points (default `"grey20"`).
#' @param line_width The width of the fitted curve (default 0.8).
#' @param fit_color The color of the fitted curve (default `"#b2182b"`).
#' @param pt.size,pt.colour,pt.shape,pt.fill,line.size,line.colour,linetype,resid.shape,resid.size,resid.fill,resid.colour
#'   `r lifecycle::badge("deprecated")` Use `point_size`, `point_color`,
#'   `line_width` and `fit_color`; the other styles are fixed.
#' @return A `patchwork` (ggplot2) object.
#' @seealso [peak_fit()], [multipeak_fit()], [voigt_fwhm()]
#' @export plot_fit
#'
#' @examplesIf rlang::is_installed("patchwork")
#' data(forageLIBS)
#' mean_spectrum <- colMeans(forageLIBS[-(1:14)])
#' wl <- as.numeric(names(mean_spectrum))
#' # the Stark-broadened H-alpha line, with its FWHM
#' halpha <- tibble::as_tibble(as.list(mean_spectrum[wl > 653.5 & wl < 659.5]))
#' plot_fit(peak_fit(halpha, profile = "lorentzian"), title = "H-alpha 656.28 nm",
#'          show_fwhm = TRUE)
#'
#' # the K I doublet: two Gaussian peaks, for a journal column
#' doublet <- tibble::as_tibble(as.list(mean_spectrum[wl > 404.1 & wl < 405.0]))
#' plot_fit(multipeak_fit(doublet, peaks = c(404.414, 404.721), profiles = "gaussian"),
#'          title = "K I 404.41 and 404.72 nm", base_size = 8)
plot_fit <- function(data, title = NULL, annotate = TRUE, show_fwhm = FALSE, ncol = NULL,
                     xlab = "Wavelength (nm)", ylab = "Intensity (arb. units)", caption = TRUE,
                     base_size = 11, point_size = 1.5, point_color = "grey20", line_width = 0.8,
                     fit_color = "#b2182b", pt.size = deprecated(), pt.colour = deprecated(),
                     pt.shape = deprecated(), pt.fill = deprecated(), line.size = deprecated(),
                     line.colour = deprecated(), linetype = deprecated(),
                     resid.shape = deprecated(), resid.size = deprecated(),
                     resid.fill = deprecated(), resid.colour = deprecated()) {
  if (missing(data) || is.null(data) || length(data) == 0) {
    stop("Seems you forgot to provide spectra data.")
  }
  if (!is.data.frame(data) || !"augmented" %in% names(data)) {
    stop("'data' must be the output of peak_fit() or multipeak_fit().")
  }
  # the former arguments: the sizes and colors are renamed, the others ignored
  renamed <- function(old_value, new_value, old, new) {
    if (!lifecycle::is_present(old_value)) {
      return(new_value)
    }
    lifecycle::deprecate_warn("0.9.0", sprintf("plot_fit(%s)", old), sprintf("plot_fit(%s)", new))
    old_value
  }
  point_size <- renamed(pt.size, point_size, "pt.size", "point_size")
  point_color <- renamed(pt.colour, point_color, "pt.colour", "point_color")
  line_width <- renamed(line.size, line_width, "line.size", "line_width")
  fit_color <- renamed(line.colour, fit_color, "line.colour", "fit_color")
  ignored <- c(pt.shape = lifecycle::is_present(pt.shape), pt.fill = lifecycle::is_present(pt.fill),
               linetype = lifecycle::is_present(linetype),
               resid.shape = lifecycle::is_present(resid.shape),
               resid.size = lifecycle::is_present(resid.size),
               resid.fill = lifecycle::is_present(resid.fill),
               resid.colour = lifecycle::is_present(resid.colour))
  for (old in names(ignored)[ignored]) {
    lifecycle::deprecate_warn("0.9.0", sprintf("plot_fit(%s)", old),
                              details = "It is ignored: the points and lines have a fixed style.")
  }
  check_flag(annotate, "annotate")
  check_flag(show_fwhm, "show_fwhm")
  check_number(base_size, "base_size", lower = 0, lower_open = TRUE)
  check_number(point_size, "point_size", lower = 0, lower_open = TRUE)
  check_number(line_width, "line_width", lower = 0, lower_open = TRUE)
  for (col in list(point_color = point_color, fit_color = fit_color)) {
    if (!is.character(col) || length(col) != 1 ||
        inherits(try(grDevices::col2rgb(col), silent = TRUE), "try-error")) {
      stop("'point_color' and 'fit_color' must be colors.", call. = FALSE)
    }
  }
  if (!is.null(ncol)) ncol <- check_count(ncol, "ncol")
  if (!(isTRUE(caption) || isFALSE(caption) ||
        (is.character(caption) && length(caption) == 1 && !is.na(caption)))) {
    stop("'caption' must be TRUE, FALSE or a character string.", call. = FALSE)
  }
  rlang::check_installed("patchwork", reason = "to combine the fit and residual plots.")

  id_col <- names(data)[1]
  ok <- which(!vapply(data$augmented, is.null, logical(1)))
  if (length(ok) == 0) {
    stop("None of the spectra could be fitted.")
  }
  several <- length(ok) > 1
  unit <- regmatches(xlab, regexpr("(?<=\\()[^()]+(?=\\)\\s*$)", xlab, perl = TRUE))
  style <- list(base_size = base_size, point_size = point_size, point_color = point_color,
                line_width = line_width, fit_color = fit_color, annotate = annotate,
                show_fwhm = show_fwhm, xlab = xlab, ylab = ylab,
                unit = if (length(unit) == 1) unit else "")
  profiles <- character()
  pairs <- lapply(ok, function(i) {
    fit <- data$fit[[i]]
    aug <- data$augmented[[i]]
    xr <- range(aug$x)
    curve <- fit_curve(fit, seq(xr[1], xr[2], length.out = 500))
    summary <- fit_summary(fit, aug, curve)
    profiles <<- c(profiles, summary$peaks$profile)
    fit_pair(aug, curve, summary, if (several) as.character(data[[id_col]][i]), style)
  })
  caption_text <- if (isTRUE(caption)) {
    fit_caption(unique(profiles), show_fwhm)
  } else if (is.character(caption)) {
    caption
  }
  heading <- split_title(title)
  patchwork::wrap_plots(pairs, ncol = ncol %||% min(length(pairs), 2)) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = heading$title, subtitle = heading$subtitle, caption = caption_text,
      theme = bold_title() + ggplot2::theme(
        plot.title = ggplot2::element_text(size = 1.2 * base_size),
        plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30", size = 0.8 * base_size)
      )
    ) &
    ggplot2::theme(legend.position = "bottom")
}

# ---- internals ---------------------------------------------------------------

profile_names <- c(gaussian = "Gaussian", lorentzian = "Lorentzian", voigt = "Voigt",
                   pseudo_voigt = "pseudo-Voigt")

# The parameters of each peak of a fit (center, FWHM and area with their
# standard errors, height above the baseline), the baseline, R2 and RMSE.
fit_summary <- function(fit, aug, curve) {
  est <- stats::coef(fit)
  cov <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  se <- function(term) {
    if (is.null(cov) || !term %in% rownames(cov)) NA_real_ else sqrt(max(cov[term, term], 0))
  }
  terms <- attr(fit, "peak_terms")
  profiles <- sub("^.*profile_([a-z_]+)\\(.*$", "\\1", terms)
  multi <- length(terms) > 1
  peaks <- lapply(seq_along(terms), function(j) {
    suffix <- if (multi) paste0("_", j) else ""
    name <- function(p) paste0(p, suffix)
    profile <- profiles[j]
    if (profile %in% c("voigt", "pseudo_voigt")) {
      wG <- est[[name("wG")]]
      wL <- est[[name("wL")]]
      fwhm <- voigt_fwhm(wG, wL)
      # delta method, with the covariance of wG and wL
      root <- sqrt(0.2166 * wL^2 + wG^2)
      grad <- c(if (root > 0) wG / root else 0, 0.5346 + if (root > 0) 0.2166 * wL / root else 0)
      fwhm_se <- if (is.null(cov)) NA_real_ else {
        v <- cov[c(name("wG"), name("wL")), c(name("wG"), name("wL"))]
        sqrt(max(drop(t(grad) %*% v %*% grad), 0))
      }
    } else {
      width <- if (profile == "gaussian") name("wG") else name("wL")
      fwhm <- est[[width]]
      fwhm_se <- se(width)
    }
    component <- if (multi) curve[[paste0(".peak_", j)]] else curve$.fitted
    data.frame(peak = j, profile = profile, center = est[[name("xc")]],
               center_se = se(name("xc")), fwhm = fwhm, fwhm_se = fwhm_se,
               area = est[[name("A")]], area_se = se(name("A")),
               height = max(component) - est[["y0"]])
  })
  resid <- aug$.resid
  rss <- sum(resid^2)
  list(peaks = do.call(rbind, peaks), y0 = est[["y0"]],
       r2 = 1 - rss / sum((aug$y - mean(aug$y))^2),
       rmse = sqrt(rss / max(length(resid) - length(est), 1)))
}

# An estimate and its standard error, rounded to two significant digits of
# the standard error: "656.3312 \u00b1 0.0094".
estimate_text <- function(estimate, se) {
  if (is.na(se) || !is.finite(se) || se <= 0) {
    return(format(signif(estimate, 4), big.mark = ",", scientific = FALSE, trim = TRUE))
  }
  digit <- floor(log10(se)) - 1
  decimals <- max(0, -digit)
  number <- function(v) {
    formatC(round(v, -digit), format = "f", digits = decimals, big.mark = ",")
  }
  paste(number(estimate), "\u00b1", number(se))
}

# The text of the parameters of a fit.
fit_text <- function(summary, unit) {
  u <- if (nzchar(unit)) paste0(" ", unit) else ""
  p <- summary$peaks
  # two short lines per peak, which fit narrow panels
  lines <- unlist(lapply(seq_len(nrow(p)), function(j) {
    c(paste0(if (nrow(p) > 1) paste0("Peak ", j, ": ") else "Center ",
             estimate_text(p$center[j], p$center_se[j]), u),
      paste0(if (nrow(p) > 1) "  " else "", "FWHM ", estimate_text(p$fwhm[j], p$fwhm_se[j]), u,
             ", area ", estimate_text(p$area[j], p$area_se[j])))
  }))
  rmse <- format(signif(summary$rmse, 3), big.mark = ",", scientific = FALSE, trim = TRUE)
  paste(c(lines, sprintf("R\u00b2 %.4f, RMSE %s", summary$r2, rmse)), collapse = "\n")
}

# The fit and residual panels of one spectrum.
fit_pair <- function(aug, curve, summary, label, style) {
  x <- y <- .fitted <- .resid <- value <- peak <- NULL
  peaks <- summary$peaks
  multi <- nrow(peaks) > 1
  top <- ggplot2::ggplot(aug, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(yintercept = summary$y0, colour = "grey55", linewidth = 0.3,
                        linetype = "dotted")
  if (multi) {
    peak_cols <- grep("^\\.peak_", names(curve), value = TRUE)
    comp <- tidyr::pivot_longer(curve, dplyr::all_of(peak_cols), names_to = "peak",
                                values_to = "value")
    top <- top + ggplot2::geom_line(data = comp,
                                    ggplot2::aes(x = x, y = value, group = peak, linetype = "Peaks"),
                                    colour = "grey40", linewidth = 0.5 * style$line_width)
  }
  top <- top +
    ggplot2::geom_point(ggplot2::aes(shape = "Data"), colour = style$point_color,
                        size = style$point_size, stroke = 0.5) +
    ggplot2::geom_line(data = curve, ggplot2::aes(x = x, y = .fitted, colour = "Fit"),
                       linewidth = style$line_width)
  if (style$show_fwhm) {
    bars <- data.frame(x = peaks$center - peaks$fwhm / 2, xend = peaks$center + peaks$fwhm / 2,
                       y = summary$y0 + peaks$height / 2)
    top <- top + ggplot2::geom_segment(
      data = bars, ggplot2::aes(x = x, xend = .data$xend, y = y, yend = y), colour = "grey15",
      linewidth = 0.4, arrow = ggplot2::arrow(ends = "both", angle = 90,
                                              length = ggplot2::unit(0.06, "inches"))
    )
  }
  if (multi) {
    # the peak numbers above their tops, moved apart along x by ggrepel
    tops <- data.frame(x = peaks$center, y = summary$y0 + peaks$height, label = peaks$peak)
    lift <- 0.04 * diff(range(c(aug$y, tops$y), na.rm = TRUE))
    top <- top + ggrepel::geom_text_repel(data = tops, ggplot2::aes(x = x, y = y, label = .data$label),
                                          nudge_y = lift, direction = "x", vjust = 0,
                                          size = 0.7 * style$base_size / ggplot2::.pt,
                                          colour = "grey25", segment.colour = "grey60",
                                          min.segment.length = 0.2, max.overlaps = Inf, seed = 1)
  }
  # the parameters above the panel, where they cannot hide the data
  top <- top +
    ggplot2::scale_shape_manual(values = c(Data = 1), name = NULL,
                                guide = ggplot2::guide_legend(order = 1)) +
    ggplot2::scale_colour_manual(values = c(Fit = style$fit_color), name = NULL,
                                 guide = ggplot2::guide_legend(order = 2)) +
    ggplot2::scale_y_continuous(labels = plain_numbers,
                                expand = ggplot2::expansion(mult = c(0.04, if (multi) 0.16 else 0.06))) +
    ggplot2::labs(x = NULL, y = style$ylab, title = label,
                  subtitle = if (style$annotate) fit_text(summary, style$unit)) +
    ggplot2::coord_cartesian(clip = "off") +
    fit_theme(style$base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  if (multi) {
    top <- top + ggplot2::scale_linetype_manual(values = c(Peaks = "dashed"), name = NULL,
                                                guide = ggplot2::guide_legend(order = 3))
  }
  band <- 2 * summary$rmse
  bottom <- ggplot2::ggplot(aug, ggplot2::aes(x = x, y = .resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = c(-band, band), colour = "grey60", linewidth = 0.3,
                        linetype = "dashed") +
    ggplot2::geom_point(shape = 1, colour = style$point_color, size = 0.8 * style$point_size,
                        stroke = 0.5) +
    ggplot2::scale_y_continuous(labels = plain_numbers, n.breaks = 3) +
    # named above the panel: a y title would meet the long title of the fit
    ggplot2::labs(x = style$xlab, y = NULL, title = "Residuals") +
    fit_theme(style$base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "plain", size = ggplot2::rel(0.75),
                                                      margin = ggplot2::margin(b = 2)))
  patchwork::wrap_plots(top, bottom, ncol = 1, heights = c(4, 1.3))
}

fit_theme <- function(base_size) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "#4b4b4b", linewidth = base_size / 16),
      axis.ticks = ggplot2::element_line(colour = "#4b4b4b"),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.9)),
      plot.subtitle = ggplot2::element_text(colour = "grey20", size = ggplot2::rel(0.72),
                                            lineheight = 1.05)
    )
}

# The caption: the profiles fitted, and the lines drawn.
fit_caption <- function(profiles, show_fwhm) {
  names <- unname(profile_names[profiles])
  what <- if (length(names) == 1) {
    paste(names, "profile")
  } else {
    paste(paste(names, collapse = " and "), "profiles")
  }
  text <- paste0(what, " fitted by nonlinear least squares (Levenberg-Marquardt), ",
                 "y = y0 + sum of A f(x), with f of unit area (A: the area). ",
                 "Dotted: the baseline y0; dashed: the peaks; residuals: dashed at ",
                 "\u00b12 RMSE.")
  if (any(profiles %in% c("voigt", "pseudo_voigt"))) {
    text <- paste(text, "FWHM of the Voigt profiles from wG and wL (Olivero and",
                  "Longbothum, 1977).")
  }
  if (show_fwhm) {
    text <- paste(text, "Bars: the FWHM.")
  }
  paste(strwrap(text, width = 80), collapse = "\n")
}
