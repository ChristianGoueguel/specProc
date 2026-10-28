#' @title Spectral Response from a Calibration Lamp
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Measures the relative spectral response of an instrument (the
#' efficiency of the collection optics, fiber, grating and detector at each
#' wavelength) from spectra of a calibration lamp of known spectral
#' irradiance or radiance, such as a deuterium-halogen lamp, with its
#' uncertainty. [correct_response()] then corrects spectra or line
#' intensities.
#'
#' @details
#' The response at each channel is the ratio of the measured lamp spectrum
#' (after subtraction of `dark`) to the certified irradiance of the lamp,
#' interpolated at the channel. The certified values, often tabulated every
#' few nanometers, are interpolated by a monotone cubic spline of their
#' logarithm; channels outside their range get no response. The ratio is
#' smoothed by a local linear fit over `smooth` nm, within each detector
#' segment when `segments = TRUE` (the axis is split where the wavelengths
#' step back or jump, as in [wavelength_calibration()]), so that the noise
#' of the lamp spectrum is averaged without biasing the steep edges of the
#' response. The channels in the `exclude` ranges, such as the Balmer lines
#' of a deuterium lamp (486.0 and 656.1 nm) that the certificate leaves
#' out, are interpolated from their neighbors. The response is scaled to a
#' maximum of 1.
#'
#' The relative standard uncertainty of the response (`rsd`) combines in
#' quadrature that of the certified values (column `rsd` of `reference`,
#' interpolated linearly) and that of the measurement: with several lamp
#' spectra, the standard error of their mean response (each spectrum
#' scaled to a common level first, so that drifts of the lamp output do not
#' count); with one, the noise of the spectrum (estimated from its
#' channel-to-channel differences) averaged over the smoothing window.
#' Certificates often give expanded uncertainties (coverage factor 2):
#' divide them by 2.
#'
#' The lamp must be measured through the same optical path as the plasma
#' (collection optics, fiber, spectrometer settings), and its spectrum
#' must not be saturated. With a certificate in energy units (W per nm),
#' the corrected intensities are proportional to the spectral radiance
#' (energy units, as `units = "energy"` of [boltzmann()]).
#'
#' @param lamp The spectra of the calibration lamp: a numeric vector named
#'   by wavelength, or a matrix or data frame with one spectrum per row and
#'   the wavelengths as column names (other columns are ignored). Several
#'   spectra are averaged.
#' @param reference The certified spectral irradiance (or radiance) of the
#'   lamp: a data frame with columns `wavelength` (nm) and `value`, and
#'   optionally `rsd`, the relative standard uncertainties of the values
#'   (0.03 for 3%).
#' @param dark An optional dark spectrum (or several, averaged) on the same
#'   axis as `lamp`, subtracted from it.
#' @param smooth The width of the smoothing window, in nm. Default is 1; 0
#'   turns the smoothing off.
#' @param exclude Wavelength ranges left out of the response and
#'   interpolated: a numeric vector of length 2, a list of them, or a
#'   two-column matrix. Default is `NULL`.
#' @param segments A logical: smooth each detector segment separately
#'   (`TRUE`, default).
#'
#' @return An object of class `specproc_response`, a list with
#'  - `response`: a tibble with, for each channel in column order, its
#'    `wavelength`, detector `segment`, relative `response` (`NA` without
#'    response), and its relative standard uncertainty `rsd`, with the
#'    parts from the reference (`reference_rsd`) and from the measurement
#'    (`measurement_rsd`);
#'  - `n_spectra`, `smooth`, `exclude` and the wavelength `range` of the
#'    reference.
#'
#' Use [correct_response()] to correct spectra or line intensities, and
#' [plot_response()] to draw the response.
#'
#' @seealso [correct_response()], [plot_response()], [line_intensities()]
#' @export response_calibration
#'
#' @examples
#' # the certified spectral irradiance of a lamp, every 10 nm, known within
#' # 3% (simulated here as a blackbody at 3100 K)
#' planck <- function(wl, t) 1 / (wl^5 * (exp(1.4388e7 / (wl * t)) - 1))
#' reference <- data.frame(wavelength = seq(250, 850, by = 10))
#' reference$value <- planck(reference$wavelength, 3100)
#' reference$rsd <- 0.03
#'
#' # five spectra of the lamp, through an instrument of unknown response
#' wl <- seq(260, 840, by = 0.1)
#' true_response <- exp(-((wl - 500) / 250)^2)
#' set.seed(1)
#' lamp <- t(replicate(5, 2e4 * true_response * planck(wl, 3100) / planck(500, 3100) +
#'                       rnorm(length(wl), sd = 20)))
#' colnames(lamp) <- wl
#'
#' response <- response_calibration(lamp, reference)
#' response
#' plot_response(response)
response_calibration <- function(lamp, reference, dark = NULL, smooth = 1, exclude = NULL,
                                 segments = TRUE) {
  sp <- line_spectra(lamp)
  x <- sp$x
  wl <- sp$wavelength
  if (!is.null(dark)) {
    dk <- line_spectra(dark)
    if (length(dk$wavelength) != length(wl) || max(abs(dk$wavelength - wl)) > 1e-6) {
      stop("'dark' must have the wavelengths of 'lamp'.", call. = FALSE)
    }
    x <- sweep(x, 2, colMeans(dk$x, na.rm = TRUE))
  }
  reference <- check_response_reference(reference)
  check_number(smooth, "smooth", lower = 0)
  check_flag(segments, "segments")
  ranges <- exclude_ranges(exclude)
  excluded <- vapply(wl, function(w) any(w >= ranges[, 1] & w <= ranges[, 2]), logical(1))

  value <- reference_values(reference, wl)
  reference_rsd <- if ("rsd" %in% names(reference)) {
    stats::approx(reference$wavelength, reference$rsd, xout = wl)$y
  } else {
    ifelse(is.na(value), NA_real_, 0)
  }
  outside <- is.na(value)
  if (any(outside)) {
    warning(sum(outside), " channel(s) outside the range of the reference (",
            paste(format(range(reference$wavelength)), collapse = " to "),
            " nm) have no response.", call. = FALSE)
  }

  # ratio of each lamp spectrum to the reference, scaled to a common level
  ratio <- sweep(x, 2, value, "/")
  ratio[, excluded | outside] <- NA
  ratio[!is.finite(ratio)] <- NA
  level <- apply(ratio, 1, stats::median, na.rm = TRUE)
  if (any(!is.finite(level) | level <= 0)) {
    stop("A lamp spectrum has no positive signal within the range of the reference.",
         call. = FALSE)
  }
  ratio <- ratio / level
  seg <- if (segments) wavelength_segments(wl) else rep(1L, length(wl))
  fits <- lapply(seq_len(nrow(ratio)), function(i) smooth_response(ratio[i, ], wl, seg, smooth))
  smoothed <- do.call(rbind, lapply(fits, `[[`, "fit"))
  n <- nrow(smoothed)
  mean_ratio <- colMeans(smoothed)
  measurement_rsd <- if (n > 1) {
    apply(smoothed, 2, stats::sd) / sqrt(n) / mean_ratio
  } else {
    # noise of the lamp spectrum, averaged over the channels of the window
    noise <- spectrum_noise(x[1, ])
    rel <- noise / (mean_ratio * level * value) / sqrt(fits[[1]]$n)
    rel[fits[[1]]$n == 0] <- NA
    fill_segments(rel, wl, seg)
  }
  mean_ratio[outside] <- NA
  measurement_rsd[outside] <- NA
  mean_ratio[!is.na(mean_ratio) & mean_ratio <= 0] <- NA
  response <- mean_ratio / max(mean_ratio, na.rm = TRUE)
  measurement_rsd[is.na(response)] <- NA
  reference_rsd[is.na(response)] <- NA
  response_rsd <- sqrt(reference_rsd^2 + measurement_rsd^2)

  structure(
    list(response = tibble::tibble(wavelength = wl, segment = seg, response = response,
                                   rsd = response_rsd, reference_rsd = reference_rsd,
                                   measurement_rsd = measurement_rsd),
         n_spectra = n, smooth = smooth, exclude = ranges,
         range = range(reference$wavelength)),
    class = "specproc_response"
  )
}

#' @title Correct Spectra or Line Intensities for the Spectral Response
#'
#' @description
#' Divides spectra, or the intensities of emission lines, by the relative
#' spectral response of the instrument measured by
#' [response_calibration()].
#'
#' @details
#' Spectra must have the wavelength axis of the response (the same
#' channels, in the same order): each channel is divided by its response.
#'
#' A table of lines, such as returned by [line_intensities()], is
#' recognized by its `wavelength` and `intensity` columns. The `intensity`
#' (and `height`) of each line is divided by the response at the channel of
#' its peak (`peak_wavelength`, or else the channel nearest to its
#' `wavelength`), and its standard error `intensity_se` (created if
#' absent) combines in quadrature its relative standard error and that of
#' the response. Correcting the line intensities rather than the spectra
#' keeps the noise estimates of [line_intensities()] valid. The errors of
#' the response of nearby lines are correlated, which [plasma_uncertainty()]
#' does not account for.
#'
#' Correct the intensities for the response before
#' [correct_self_absorption()], [boltzmann()] or [cf_libs()].
#'
#' @param x Spectra (a numeric vector named by wavelength, or a matrix or
#'   data frame with one spectrum per row and the wavelengths as column
#'   names), or a table of lines with `wavelength` and `intensity` columns.
#' @param response The result of [response_calibration()].
#'
#' @return `x`, corrected. For a table of lines, with the corrected
#'   `intensity` (and `height`), its standard error `intensity_se`, and the
#'   `response` used. Channels or lines without response become `NA`.
#'
#' @seealso [response_calibration()], [line_intensities()]
#' @export correct_response
#'
#' @examples
#' planck <- function(wl, t) 1 / (wl^5 * (exp(1.4388e7 / (wl * t)) - 1))
#' reference <- data.frame(wavelength = seq(250, 850, by = 10))
#' reference$value <- planck(reference$wavelength, 3100)
#' reference$rsd <- 0.03
#' wl <- seq(260, 840, by = 0.1)
#' true_response <- exp(-((wl - 500) / 250)^2)
#' set.seed(1)
#' lamp <- 2e4 * true_response * planck(wl, 3100) / planck(500, 3100) + rnorm(length(wl), sd = 20)
#' names(lamp) <- wl
#' response <- response_calibration(lamp, reference)
#'
#' # a spectrum with two lines of equal emission, at 300 and 650 nm
#' emission <- 1000 * exp(-(wl - 300)^2 / 0.01) + 1000 * exp(-(wl - 650)^2 / 0.01)
#' spectrum <- stats::setNames(true_response * emission + rnorm(length(wl)), wl)
#' lines <- line_intensities(spectrum, c(a = 300, b = 650))
#' lines$intensity   # unequal: the response differs
#' corrected <- correct_response(lines, response)
#' corrected[c("line", "intensity", "intensity_se", "response")]
#'
#' # or the whole spectrum
#' corrected_spectrum <- correct_response(spectrum, response)
correct_response <- function(x, response) {
  if (!inherits(response, "specproc_response")) {
    stop("'response' must be returned by response_calibration().", call. = FALSE)
  }
  r <- response$response
  if (is.data.frame(x) && all(c("wavelength", "intensity") %in% names(x))) {
    return(correct_line_response(x, r))
  }
  nms <- if (is.null(dim(x))) names(x) else colnames(x)
  if (is.null(nms)) {
    stop("'x' must be spectra with wavelengths as names, or a table of lines.", call. = FALSE)
  }
  wl <- suppressWarnings(as.numeric(nms))
  channel <- !is.na(wl)
  if (is.data.frame(x)) channel <- channel & vapply(x, is.numeric, logical(1))
  if (sum(channel) != nrow(r) || max(abs(wl[channel] - r$wavelength)) > 1e-6) {
    stop("'x' does not have the wavelength axis of the response.", call. = FALSE)
  }
  if (is.null(dim(x))) {
    x[channel] <- x[channel] / r$response
  } else if (is.matrix(x)) {
    x[, channel] <- sweep(x[, channel, drop = FALSE], 2, r$response, "/")
  } else {
    cols <- which(channel)
    for (j in seq_along(cols)) x[[cols[j]]] <- x[[cols[j]]] / r$response[j]
  }
  x
}

#' @title Plot a Spectral Response
#'
#' @description
#' Plots the relative spectral response measured by
#' [response_calibration()], with a band of plus or minus one standard
#' uncertainty.
#'
#' @param object The result of [response_calibration()].
#' @param title The plot title. By default, the number of lamp spectra and
#'   the median uncertainty.
#'
#' @return A ggplot object.
#' @seealso [response_calibration()]
#' @export plot_response
#'
#' @examples
#' planck <- function(wl, t) 1 / (wl^5 * (exp(1.4388e7 / (wl * t)) - 1))
#' reference <- data.frame(wavelength = seq(250, 850, by = 10))
#' reference$value <- planck(reference$wavelength, 3100)
#' reference$rsd <- 0.03
#' wl <- seq(260, 840, by = 0.1)
#' set.seed(1)
#' lamp <- 2e4 * exp(-((wl - 500) / 250)^2) * planck(wl, 3100) / planck(500, 3100) +
#'   rnorm(length(wl), sd = 20)
#' names(lamp) <- wl
#' plot_response(response_calibration(lamp, reference))
plot_response <- function(object, title = NULL) {
  if (!inherits(object, "specproc_response")) {
    stop("'object' must be returned by response_calibration().", call. = FALSE)
  }
  d <- as.data.frame(object$response)
  d <- d[!is.na(d$response), , drop = FALSE]
  d$lower <- d$response * (1 - d$rsd)
  d$upper <- d$response * (1 + d$rsd)
  d$segment <- factor(d$segment)
  if (is.null(title)) {
    title <- sprintf("Spectral response: %d lamp spectr%s, median uncertainty %s%%",
                     object$n_spectra, if (object$n_spectra == 1) "um" else "a",
                     format(signif(100 * stats::median(d$rsd), 2)))
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$wavelength, group = .data$segment)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), fill = "grey75") +
    ggplot2::geom_line(ggplot2::aes(y = .data$response), colour = "#1b9e77", linewidth = 0.6) +
    ggplot2::labs(x = "Wavelength (nm)", y = "Relative response", title = title) +
    ggplot2::theme_bw()
  finish_title(p)
}

#' @export
print.specproc_response <- function(x, ...) {
  r <- x$response
  ok <- !is.na(r$response)
  cat("Spectral response from ", x$n_spectra, " lamp spectr", if (x$n_spectra == 1) "um" else "a",
      if (x$smooth > 0) paste0(" (smoothed over ", format(x$smooth), " nm)"), "\n\n", sep = "")
  cat("Channels:     ", nrow(r), " (", format(round(min(r$wavelength), 1)), " to ",
      format(round(max(r$wavelength), 1)), " nm, ", length(unique(r$segment)), " segment",
      if (length(unique(r$segment)) > 1) "s", ")\n", sep = "")
  pct <- function(v) paste0(format(signif(100 * v, 2)), "%")
  if (any(ok)) {
    cat("Uncertainty:  median ", pct(stats::median(r$rsd[ok])), " (", pct(min(r$rsd[ok])), " to ",
        pct(max(r$rsd[ok])), "); reference ", pct(stats::median(r$reference_rsd[ok])),
        ", measurement ", pct(stats::median(r$measurement_rsd[ok])), " (medians)\n", sep = "")
  }
  if (nrow(x$exclude) > 0) {
    cat("Excluded:     ", paste(format(x$exclude[, 1]), "to", format(x$exclude[, 2]), "nm",
                                collapse = ", "), "\n", sep = "")
  }
  if (any(!ok)) {
    cat("No response:  ", sum(!ok), " channel(s) (the reference covers ",
        paste(format(x$range), collapse = " to "), " nm)\n", sep = "")
  }
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

check_response_reference <- function(reference) {
  if (!is.data.frame(reference) || !all(c("wavelength", "value") %in% names(reference))) {
    stop("'reference' must be a data frame with columns `wavelength` and `value`.", call. = FALSE)
  }
  ok <- is.numeric(reference$wavelength) && is.numeric(reference$value) &&
    !anyNA(reference$wavelength) && !anyNA(reference$value) && all(reference$value > 0)
  if (!ok || nrow(reference) < 2 || anyDuplicated(reference$wavelength)) {
    stop("'reference' needs at least 2 distinct wavelengths with positive values.", call. = FALSE)
  }
  if ("rsd" %in% names(reference) &&
      (!is.numeric(reference$rsd) || anyNA(reference$rsd) || any(reference$rsd < 0))) {
    stop("The column `rsd` of 'reference' must contain non-negative numbers.", call. = FALSE)
  }
  reference[order(reference$wavelength), , drop = FALSE]
}

# Certified values at the wavelengths wl: monotone cubic spline of their
# logarithm, NA outside their range.
reference_values <- function(reference, wl) {
  f <- stats::splinefun(reference$wavelength, log(reference$value), method = "monoH.FC")
  out <- exp(f(wl))
  out[wl < min(reference$wavelength) | wl > max(reference$wavelength)] <- NA
  out
}

# Wavelength ranges as a two-column matrix.
exclude_ranges <- function(exclude) {
  if (is.null(exclude)) return(matrix(numeric(), 0, 2))
  m <- if (is.matrix(exclude)) exclude else if (is.list(exclude)) do.call(rbind, exclude) else
    matrix(exclude, ncol = 2, byrow = TRUE)
  if (!is.numeric(m) || ncol(m) != 2 || anyNA(m) || any(m[, 1] >= m[, 2])) {
    stop("'exclude' must give wavelength ranges (from, to) with from < to.", call. = FALSE)
  }
  unname(m)
}

# Local linear fit of v over `width` nm around each channel, within each
# segment, with the number of channels of each window; the channels without
# a value are interpolated from their neighbors.
smooth_response <- function(v, wl, seg, width) {
  fit <- n <- rep(NA_real_, length(v))
  for (s in unique(seg)) {
    idx <- which(seg == s)
    idx <- idx[order(wl[idx])]   # overlapping detectors in one segment
    one <- local_linear(v[idx], wl[idx], width)
    fit[idx] <- one$fit
    n[idx] <- one$n
  }
  list(fit = fill_segments(fit, wl, seg), n = n)
}

local_linear <- function(v, w, width) {
  ok <- !is.na(v)
  if (width <= 0) return(list(fit = v, n = as.numeric(ok)))
  w0 <- w - mean(w)
  total <- function(a) c(0, cumsum(ifelse(ok, a, 0)))
  window <- function(s) s[hi + 1] - s[lo + 1]
  lo <- findInterval(w - width / 2, w, left.open = TRUE)   # channels before the window
  hi <- findInterval(w + width / 2, w)                       # channels up to its end
  n <- window(total(1))
  sw <- window(total(w0))
  sv <- window(total(v))
  sww <- window(total(w0^2))
  swv <- window(total(w0 * v))
  var <- sww - sw^2 / n
  slope <- ifelse(n >= 2 & var > 1e-12, (swv - sw * sv / n) / var, 0)
  fit <- ifelse(n > 0, sv / n + slope * (w0 - sw / n), NA_real_)
  list(fit = fit, n = n)
}

# Linear interpolation of the missing values of v within each segment.
fill_segments <- function(v, wl, seg) {
  for (s in unique(seg)) {
    idx <- which(seg == s)
    ok <- !is.na(v[idx])
    if (any(ok) && !all(ok)) {
      v[idx] <- if (sum(ok) == 1) v[idx][ok] else
        stats::approx(wl[idx][ok], v[idx][ok], xout = wl[idx], rule = 2, ties = mean)$y
    }
  }
  v
}

# Divides the intensities of a table of lines by the response at their peak.
correct_line_response <- function(lines, r) {
  at <- lines$wavelength
  if ("peak_wavelength" %in% names(lines)) {
    at <- ifelse(is.na(lines$peak_wavelength), at, lines$peak_wavelength)
  }
  idx <- vapply(at, function(w) which.min(abs(r$wavelength - w)), integer(1))
  resp <- r$response[idx]
  rsd <- r$rsd[idx]
  far <- abs(r$wavelength[idx] - at) > 1
  resp[far] <- NA
  rsd[far] <- NA
  if (anyNA(resp)) {
    warning(sum(is.na(resp)), " line(s) have no response at their wavelength; their ",
            "intensity is NA.", call. = FALSE)
  }
  se_rel <- if ("intensity_se" %in% names(lines)) lines$intensity_se / lines$intensity else 0
  lines$intensity <- lines$intensity / resp
  if ("height" %in% names(lines)) lines$height <- lines$height / resp
  lines$intensity_se <- lines$intensity * sqrt(se_rel^2 + rsd^2)
  lines$response <- resp
  lines
}
