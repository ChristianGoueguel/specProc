#' @title Wavelength Calibration from Reference Lines
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Measures the positions of known emission lines in spectra and fits a
#' correction of the wavelength axis, so that the lines fall at their
#' reference wavelengths (for example, from the NIST Atomic Spectra
#' Database). [apply_calibration()] then relabels the wavelengths of the
#' spectra, without changing their intensities.
#'
#' @details
#' The lines are located in the mean of the spectra. For each reference
#' line, the peak is the channel of highest intensity within `search` nm of
#' the reference wavelength, and its position is refined to a fraction of a
#' channel by the vertex of the parabola through the logarithms of the peak
#' channel and its two neighbors, above the lowest channel of the window
#' (Gaussian interpolation, exact for Gaussian lines; Caruana et al., 1986).
#' A line is not used when
#'  - `"edge"`: the highest channel is at the border of the search window
#'    (the peak is not in the window, or a stronger line is nearby);
#'  - `"weak"`: its height above the lowest channel of the window is less
#'    than `min_snr` times the noise of the spectrum;
#'  - `"flat"`: three channels or more are within 1% of its maximum, or its
#'    curvature is not that of a peak, as for saturated or strongly
#'    self-absorbed lines;
#'  - `"outlier"`: its residual after the fit exceeds 3 robust standard
#'    deviations of the residuals (for example, a blend, or a line
#'    identified wrongly), when `reject = TRUE`.
#'
#' The correction is a polynomial of degree `degree` of the measured
#' wavelength, \eqn{\lambda_{true} = \lambda + \sum_{j=0}^{d} c_j (\lambda -
#' \lambda_0)^j}, where \eqn{\lambda_0} is the mean position of the lines: a
#' constant shift for `degree = 0`, linear for `degree = 1` (default). It
#' needs at least `degree + 1` usable lines, and more to estimate its
#' residual error. Beyond the range of the lines, the polynomial is
#' extrapolated: spread the reference lines over the spectral range, and
#' prefer low degrees.
#'
#' Instruments made of several spectrometers have one wavelength
#' calibration per detector. With `segments = TRUE` (default), the axis is
#' split where the wavelengths step back (overlapping detectors) or jump
#' (gaps of more than 5 times the median channel spacing), and each segment
#' gets its own correction. A segment with fewer usable lines than
#' `degree + 1` gets a constant shift if it has one line, and no correction
#' otherwise, with a warning.
#'
#' @param spectra The spectra: a numeric vector named by wavelength, or a
#'   matrix or data frame with one spectrum per row and the wavelengths as
#'   column names (other columns are ignored). The lines are located in their
#'   mean.
#' @param lines The reference lines: a numeric vector of wavelengths (nm),
#'   preferably named, or a data frame with a `wavelength` column (and
#'   optionally a `species` column), such as returned by [nist_lines()].
#'   Choose isolated, strong but not saturated lines, spread over the range.
#' @param degree The degree of the correction polynomial: 0 (shift), 1
#'   (default) or 2.
#' @param search The half-width of the window in which each line is
#'   searched, in nm. Default is 0.3.
#' @param min_snr The minimum height of a line, in units of the noise of
#'   the spectrum. Default is 20.
#' @param segments A logical: fit each detector segment separately (`TRUE`,
#'   default).
#' @param reject A logical: remove the outlying lines after the fit (`TRUE`,
#'   default).
#'
#' @return An object of class `specproc_wavelength_calibration`, a list with
#'  - `lines`: a tibble with, for each reference line, its `line` label,
#'    `reference` and `measured` wavelengths, the `offset` (reference minus
#'    measured), its `segment`, `snr`, whether it was `used` (otherwise the
#'    `reason`), the `correction` of the fit at the line and the `residual`;
#'  - `segments`: a tibble with, for each segment, its wavelength range
#'    (`from`, `to`), the `degree` fitted, the number of lines `n_lines` and
#'    the root mean square residual `rmse` (nm);
#'  - `wavelength`: the wavelengths of the calibrated axis, in column order.
#'
#' Use [apply_calibration()] to correct spectra,
#' [predict()][predict.specproc_wavelength_calibration] to correct any
#' wavelength, and [plot_wavelength_calibration()] to draw the fit.
#'
#' @references
#'  - Caruana, R.A., Searle, R.B., Heller, T., Shupack, S.I. (1986). Fast
#'    algorithm for the resolution of spectra. Analytical Chemistry,
#'    58(6):1162-1167.
#'
#' @seealso [apply_calibration()], [plot_wavelength_calibration()],
#'   [nist_lines()], [line_intensities()], [line_finder()]
#' @export wavelength_calibration
#'
#' @examples
#' data(soilLIBS)
#' reference <- c(`Mg II` = 279.553, `Mg II` = 280.270, `Si I` = 288.158,
#'                `Al I` = 308.215, `Al I` = 309.271, `Ca II` = 393.366,
#'                `Al I` = 394.401, `Al I` = 396.152, `Ca II` = 396.847,
#'                `Ca I` = 422.673, `Na I` = 588.995, `Na I` = 589.592,
#'                `Li I` = 670.791, `K I` = 766.490, `K I` = 769.896)
#' cal <- wavelength_calibration(soilLIBS, reference)
#' cal
#' plot_wavelength_calibration(cal)
#'
#' corrected <- apply_calibration(soilLIBS, cal)
#' head(names(corrected)[-(1:8)])
wavelength_calibration <- function(spectra, lines, degree = 1, search = 0.3, min_snr = 20,
                                   segments = TRUE, reject = TRUE) {
  if (!degree %in% 0:2) stop("'degree' must be 0, 1 or 2.", call. = FALSE)
  check_number(search, "search", lower = 0, lower_open = TRUE)
  check_number(min_snr, "min_snr", lower = 0)
  check_flag(segments, "segments")
  check_flag(reject, "reject")
  sp <- line_spectra(spectra)
  wl <- sp$wavelength
  spectrum <- colMeans(sp$x, na.rm = TRUE)
  ref <- line_table(lines)
  labels <- if ("line" %in% names(ref)) ref$line else if ("species" %in% names(ref)) {
    paste(ref$species, format(ref$wavelength, nsmall = 3))
  } else {
    format(ref$wavelength, nsmall = 3)
  }
  seg <- if (segments) wavelength_segments(wl) else rep(1L, length(wl))
  noise <- spectrum_noise(spectrum)

  found <- lapply(ref$wavelength, function(r) locate_line(spectrum, wl, seg, r, search, noise, min_snr))
  out <- tibble::tibble(
    line = labels, reference = ref$wavelength,
    measured = vapply(found, `[[`, numeric(1), "position"),
    segment = vapply(found, `[[`, integer(1), "segment"),
    snr = vapply(found, `[[`, numeric(1), "snr"),
    used = vapply(found, function(f) is.na(f$reason), logical(1)),
    reason = vapply(found, `[[`, character(1), "reason")
  )
  out$offset <- out$reference - out$measured

  fits <- list()
  seg_rows <- list()
  for (s in sort(unique(seg))) {
    rows <- which(out$segment %in% s & out$used)
    fit <- fit_wavelength_segment(out, rows, degree, reject)
    out$used[fit$rejected] <- FALSE
    out$reason[fit$rejected] <- "outlier"
    fits[[as.character(s)]] <- fit$model
    axis <- wl[seg == s]
    seg_rows[[length(seg_rows) + 1]] <- tibble::tibble(
      segment = s, from = min(axis), to = max(axis), degree = fit$degree,
      n_lines = length(fit$rows), rmse = fit$rmse
    )
  }
  out$correction <- NA_real_
  ok <- !is.na(out$measured)
  out$correction[ok] <- vapply(which(ok), function(i) {
    wavelength_correction(fits[[as.character(out$segment[i])]], out$measured[i])
  }, numeric(1))
  out$residual <- out$offset - out$correction
  seg_tbl <- do.call(rbind, seg_rows)
  none <- seg_tbl$degree < 0
  if (any(none)) {
    warning("No usable reference line in ", sum(none), " segment(s) (",
            paste(sprintf("%.1f-%.1f nm", seg_tbl$from[none], seg_tbl$to[none]), collapse = ", "),
            "): their wavelengths are not corrected.", call. = FALSE)
  }
  lowered <- seg_tbl$degree >= 0 & seg_tbl$degree < degree
  if (any(lowered)) {
    warning("Too few usable lines for degree ", degree, " in ", sum(lowered),
            " segment(s): a lower degree is fitted there.", call. = FALSE)
  }
  structure(
    list(lines = out[c("line", "reference", "measured", "offset", "segment", "snr", "used",
                       "reason", "correction", "residual")],
         segments = seg_tbl, fits = fits, wavelength = wl, segment_of = seg, degree = degree),
    class = "specproc_wavelength_calibration"
  )
}

#' @title Apply a Wavelength Calibration
#'
#' @description
#' Relabels the wavelengths of spectra with the correction fitted by
#' [wavelength_calibration()]. The intensities are not changed.
#'
#' @details
#' The spectra must have the wavelength axis of the calibration (the same
#' channels, in the same order), so that each channel gets the correction of
#' its detector segment. The corrected wavelengths are rounded to 4 decimals
#' (0.1 pm) for the column names.
#'
#' @param spectra Spectra with the wavelength axis of the calibration: a
#'   numeric vector named by wavelength, or a matrix or data frame with the
#'   wavelengths as column names (other columns are kept unchanged).
#' @param calibration The result of [wavelength_calibration()].
#'
#' @return `spectra`, with the corrected wavelengths as names.
#' @seealso [wavelength_calibration()]
#' @export apply_calibration
apply_calibration <- function(spectra, calibration) {
  if (!inherits(calibration, "specproc_wavelength_calibration")) {
    stop("'calibration' must be returned by wavelength_calibration().", call. = FALSE)
  }
  nms <- if (is.null(dim(spectra))) names(spectra) else colnames(spectra)
  if (is.null(nms)) stop("'spectra' must have wavelengths as names.", call. = FALSE)
  wl <- suppressWarnings(as.numeric(nms))
  channel <- !is.na(wl)
  if (is.data.frame(spectra)) channel <- channel & vapply(spectra, is.numeric, logical(1))
  if (sum(channel) != length(calibration$wavelength) ||
      max(abs(wl[channel] - calibration$wavelength)) > 1e-6) {
    stop("'spectra' do not have the wavelength axis of the calibration.", call. = FALSE)
  }
  corrected <- calibrated_axis(calibration)
  new <- sprintf("%.4f", corrected)
  if (anyDuplicated(new)) {
    stop("The corrected wavelengths are not unique; check the calibration.", call. = FALSE)
  }
  nms[channel] <- new
  if (is.null(dim(spectra))) {
    names(spectra) <- nms
  } else {
    colnames(spectra) <- nms
  }
  spectra
}

#' @title Correct Wavelengths with a Wavelength Calibration
#'
#' @description
#' Converts measured wavelengths to calibrated ones with the correction
#' fitted by [wavelength_calibration()].
#'
#' @param object The result of [wavelength_calibration()].
#' @param wavelength Measured wavelengths, in nm.
#' @param ... Not used.
#'
#' @return The corrected wavelengths. Each wavelength gets the correction of
#'   the detector segment whose range contains it (the first one where
#'   detectors overlap), or of the nearest segment.
#' @seealso [wavelength_calibration()], [apply_calibration()]
#' @export
predict.specproc_wavelength_calibration <- function(object, wavelength, ...) {
  if (!is.numeric(wavelength)) stop("'wavelength' must be numeric.", call. = FALSE)
  seg <- object$segments
  vapply(wavelength, function(w) {
    if (is.na(w)) return(NA_real_)
    inside <- which(w >= seg$from & w <= seg$to)
    s <- if (length(inside) > 0) seg$segment[inside[1]] else
      seg$segment[which.min(pmin(abs(w - seg$from), abs(w - seg$to)))]
    w + wavelength_correction(object$fits[[as.character(s)]], w)
  }, numeric(1))
}

#' @title Plot a Wavelength Calibration
#'
#' @description
#' Plots the offset of each reference line (reference minus measured
#' wavelength) against its measured wavelength, with the fitted correction
#' of each detector segment. Lines not used are drawn as open circles and
#' labeled with the reason.
#'
#' @param object The result of [wavelength_calibration()].
#' @param title The plot title.
#'
#' @return A ggplot object.
#' @seealso [wavelength_calibration()]
#' @export plot_wavelength_calibration
plot_wavelength_calibration <- function(object, title = NULL) {
  if (!inherits(object, "specproc_wavelength_calibration")) {
    stop("'object' must be returned by wavelength_calibration().", call. = FALSE)
  }
  pts <- as.data.frame(object$lines[!is.na(object$lines$measured), ])
  pts$status <- ifelse(pts$used, "used", "not used")
  pts$label <- ifelse(pts$used, pts$line, paste0(pts$line, " (", pts$reason, ")"))
  pts$segment <- factor(pts$segment, levels = object$segments$segment)
  curves <- do.call(rbind, lapply(seq_len(nrow(object$segments)), function(i) {
    s <- object$segments[i, ]
    w <- seq(s$from, s$to, length.out = 100)
    data.frame(wavelength = w, correction = vapply(w, function(v) {
      wavelength_correction(object$fits[[as.character(s$segment)]], v)
    }, numeric(1)), segment = factor(s$segment, levels = levels(pts$segment)))
  }))
  if (is.null(title)) {
    used <- object$lines$used
    title <- sprintf("Wavelength calibration: %d of %d lines, RMS residual %.4f nm", sum(used),
                     nrow(object$lines),
                     sqrt(mean(object$lines$residual[used]^2, na.rm = TRUE)))
  }
  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_line(data = curves, ggplot2::aes(.data$wavelength, .data$correction,
                                                   colour = .data$segment), linewidth = 0.7) +
    ggplot2::geom_point(data = pts, ggplot2::aes(.data$measured, .data$offset, colour = .data$segment,
                                                 shape = .data$status), size = 2.5) +
    ggplot2::geom_text(data = pts, ggplot2::aes(.data$measured, .data$offset, label = .data$label),
                       size = 2.6, vjust = -0.9, colour = "grey25", check_overlap = TRUE) +
    ggplot2::scale_shape_manual(values = c(used = 16, `not used` = 1), name = NULL) +
    ggplot2::labs(x = "Measured wavelength (nm)", y = "Reference - measured (nm)",
                  colour = "Segment", title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}

#' @export
print.specproc_wavelength_calibration <- function(x, ...) {
  used <- x$lines$used
  cat("Wavelength calibration (degree ", x$degree, "): ", sum(used), " of ", nrow(x$lines),
      " reference lines used\n\n", sep = "")
  seg <- as.data.frame(x$segments)
  seg$degree <- ifelse(seg$degree < 0, "none", as.character(seg$degree))
  seg$rmse <- sprintf("%.4f", seg$rmse)
  print(seg, row.names = FALSE)
  rejected <- x$lines[!used, , drop = FALSE]
  if (nrow(rejected) > 0) {
    cat("\nNot used: ", paste0(rejected$line, " (", rejected$reason, ")", collapse = ", "), "\n",
        sep = "")
  }
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

# Detector segments of a wavelength axis, in column order: a new segment
# starts where the wavelengths step back or jump by more than 5 times the
# median spacing.
wavelength_segments <- function(wl) {
  if (length(wl) < 3) return(rep(1L, length(wl)))
  step <- diff(wl)
  spacing <- stats::median(abs(step))
  as.integer(cumsum(c(TRUE, step <= 0 | step > 5 * spacing)))
}

# Sub-channel position of a line near `reference` in one spectrum.
locate_line <- function(spectrum, wl, seg, reference, search, noise, min_snr) {
  result <- function(position = NA_real_, segment = NA_integer_, snr = NA_real_, reason = NA_character_) {
    list(position = position, segment = segment, snr = snr, reason = reason)
  }
  near <- which(abs(wl - reference) <= search)
  if (length(near) < 3) return(result(reason = "outside"))
  # the segment holding most of the window (overlapping detectors)
  s <- as.integer(names(which.max(table(seg[near]))))
  near <- near[seg[near] == s]
  near <- near[order(wl[near])]
  if (length(near) < 3) return(result(segment = s, reason = "outside"))
  i <- which.max(spectrum[near])
  snr <- (spectrum[near][i] - min(spectrum[near])) / noise
  if (i == 1 || i == length(near)) return(result(segment = s, snr = snr, reason = "edge"))
  if (!is.finite(snr) || snr < min_snr) return(result(segment = s, snr = snr, reason = "weak"))
  idx <- near[(i - 1):(i + 1)]
  x <- wl[idx]
  y <- spectrum[idx]
  # parabola through the three points; its curvature must be that of a peak
  a <- ((y[3] - y[2]) / (x[3] - x[2]) - (y[2] - y[1]) / (x[2] - x[1])) / (x[3] - x[1])
  # a plateau of 3 channels or more near the maximum: saturated or self-absorbed
  height <- spectrum[near][i] - min(spectrum[near])
  plateau <- sum(spectrum[near] >= spectrum[near][i] - 0.01 * height)
  if (!is.finite(a) || a >= 0 || plateau >= 3) {
    return(result(segment = s, snr = snr, reason = "flat"))
  }
  result(position = parabola_vertex(x, y, log(y - min(spectrum[near]))), segment = s, snr = snr)
}

# Vertex of the parabola through three points: on the logarithm of the
# intensities above the background when possible (exact for a Gaussian peak,
# Caruana et al., 1986), on the intensities otherwise.
parabola_vertex <- function(x, y, log_y) {
  vertex <- function(v) {
    a <- ((v[3] - v[2]) / (x[3] - x[2]) - (v[2] - v[1]) / (x[2] - x[1])) / (x[3] - x[1])
    b <- (v[2] - v[1]) / (x[2] - x[1]) - a * (x[1] + x[2])
    if (!is.finite(a) || a >= 0) return(NA_real_)
    -b / (2 * a)
  }
  position <- if (all(is.finite(log_y))) vertex(log_y) else NA_real_
  if (is.na(position) || position < x[1] || position > x[3]) position <- vertex(y)
  position
}

# Fits the offset polynomial of one segment, removing outlying lines.
fit_wavelength_segment <- function(lines, rows, degree, reject) {
  rejected <- integer()
  repeat {
    d <- min(degree, length(rows) - 1)
    if (length(rows) == 0) {
      return(list(model = NULL, degree = -1L, rows = rows, rmse = NA_real_, rejected = rejected))
    }
    center <- mean(lines$measured[rows])
    xx <- lines$measured[rows] - center
    design <- outer(xx, 0:d, `^`)
    coef <- qr.solve(design, lines$offset[rows])
    res <- lines$offset[rows] - design %*% coef
    model <- list(coef = unname(coef), center = center)
    free <- length(rows) - (d + 1)
    if (!reject || free < 2) break
    scale <- max(stats::mad(res), 0.005)
    worst <- which.max(abs(res))
    if (abs(res[worst]) <= 3 * scale) break
    rejected <- c(rejected, rows[worst])
    rows <- rows[-worst]
  }
  rmse <- if (length(rows) > d + 1) sqrt(sum(res^2) / (length(rows) - d - 1)) else NA_real_
  list(model = model, degree = as.integer(d), rows = rows, rmse = rmse, rejected = rejected)
}

wavelength_correction <- function(model, w) {
  if (is.null(model)) return(0)
  sum(model$coef * (w - model$center)^(seq_along(model$coef) - 1))
}

# Corrected wavelengths of the calibrated axis, each channel in its segment.
calibrated_axis <- function(calibration) {
  wl <- calibration$wavelength
  vapply(seq_along(wl), function(i) {
    wl[i] + wavelength_correction(calibration$fits[[as.character(calibration$segment_of[i])]], wl[i])
  }, numeric(1))
}
